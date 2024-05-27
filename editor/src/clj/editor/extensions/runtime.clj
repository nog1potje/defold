;; Copyright 2020-2024 The Defold Foundation
;; Copyright 2014-2020 King
;; Copyright 2009-2014 Ragnar Svensson, Christian Murray
;; Licensed under the Defold License version 1.0 (the "License"); you may not use
;; this file except in compliance with the License.
;;
;; You may obtain a copy of the License, together with FAQs at
;; https://www.defold.com/license
;;
;; Unless required by applicable law or agreed to in writing, software distributed
;; under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
;; CONDITIONS OF ANY KIND, either express or implied. See the License for the
;; specific language governing permissions and limitations under the License.

(ns editor.extensions.runtime
  "Editor Lua runtime

  This provides functionality for the editor scripts to access the editor, and
  for the editor to interact with editor scripts

  One important feature of the editor runtime is support for long-running
  computations without blocking the VM. It is achieved with coroutines: when
  the editor scripts attempt to perform a long-running computation, the
  Lua code execution suspends and returns control back to the editor. The editor
  then schedules the computation and, once it's done, resumes the execution.
  This runtime splits coroutine into 2 contexts: 'user' and 'system'. With this
  split, editor-related suspends are invisible to the editor script, so the
  editor script may still use coroutines internally without interfering with
  the editor.

  This namespace defines 2 ways to run Lua code:
    1. invoke-suspending - call the LuaFunction in a system coroutine, allowing
       it to use suspendable functions defined by the editor.
    2. invoke-immediate - call the LuaFunction with suspendable functions
       defined by the editor disabled. This way there is no coroutine overhead,
       and code executes significantly faster."
  (:refer-clojure :exclude [read eval])
  (:require [cljfx.api :as fx]
            [clojure.java.io :as io]
            [dynamo.graph :as g]
            [editor.code.data :as data]
            [editor.defold-project :as project]
            [editor.extensions.vm :as vm]
            [editor.future :as future]
            [editor.workspace :as workspace])
  (:import [clojure.lang Var]
           [com.defold.editor.luart DefoldBaseLib DefoldCoroutineCreate DefoldCoroutineYield DefoldIoLib DefoldVarArgFn SearchPath]
           [java.io File PrintStream Writer]
           [java.nio.charset StandardCharsets]
           [java.nio.file Path]
           [org.apache.commons.io.output WriterOutputStream]
           [org.luaj.vm2 LoadState LuaError LuaFunction LuaTable LuaValue]
           [org.luaj.vm2.compiler LuaC]
           [org.luaj.vm2.lib Bit32Lib CoroutineLib PackageLib PackageLib$lua_searcher PackageLib$preload_searcher StringLib TableLib]
           [org.luaj.vm2.lib.jse JseMathLib JseOsLib]))

(set! *warn-on-reflection* true)

(deftype EditorExtensionsRuntime [lua-vm create resume status yield])

(deftype Suspend [f args])

(deftype SuspendResult [lua-value lua-error refresh-context])

(defn ->lua
  "Convert Clojure data structure to Lua data structure"
  ^LuaValue [x]
  (vm/->lua x))

(defn ->clj
  "Convert Lua data structure to Clojure data structure

  Might lock the runtime Lua VM while deserializing tables
  Converts tables either to:
  - vectors, if not empty and all keys are positive ints
  - maps, otherwise. string keys are converted to keywords

  Preserves LuaThread, File (from IoLib) and LuaFunction"
  [^EditorExtensionsRuntime runtime lua-value]
  (vm/->clj lua-value (.-lua-vm runtime)))

(def ^:private ^:dynamic *execution-context* nil)

(defn current-execution-context
  "Returns the current execution context, a map with following keys:
    :evaluation-context    the evaluation context for this execution
    :runtime               the EditorExtensionsRuntime used for execution
    :mode                  :suspendable or :immediate"
  []
  {:pre [(some? *execution-context*)]}
  *execution-context*)

(defn wrap-suspendable-function
  ^LuaFunction [f]
  (DefoldVarArgFn.
    (fn [& args]
      (let [ctx (current-execution-context)]
        (if (= :immediate (:mode ctx))
          (throw (LuaError. "Cannot use long-running editor function in immediate context")))
        (let [^EditorExtensionsRuntime runtime (:runtime ctx)
              vm (.-lua-vm runtime)
              suspend (Suspend. f args)
              ^SuspendResult result (->clj runtime (vm/invoke-1 vm (.-yield runtime) (->lua suspend)))]
          (if-let [lua-error (.-lua-error result)]
            (throw lua-error)
            (.-lua-value result)))))))


(defn suspend-result-success
  "Construct a successful SuspendResult with a resulting value

  Args:
    lua-value          LuaValue returned to the editor script that called a
                       suspendable function
    refresh-context    boolean flag indicating whether the runtime needs to
                       refresh the evaluation context for subsequent Lua
                       execution"
  [lua-value refresh-context]
  (SuspendResult. lua-value nil refresh-context))

(defn suspend-result-error
  "Construct a failed SuspendResult

  Args:
    lua-error    LuaError value that will be thrown as a result of editor script
                 calling a suspendable function"
  [lua-error]
  (SuspendResult. nil lua-error false))

(defmacro suspendable-lua-fn
  "Defines a suspendable Lua function

  The function will receive LuaValue args that were passed by the editor script.
  It must return a future that will eventually be completed with SuspendResult
  (see suspend-result-success and suspend-result-error).

  Returned function will be executed in an execution context that can be
  accessed using current-execution-context fn"
  [& fn-tail]
  `(wrap-suspendable-function (fn ~@fn-tail)))

(defmacro lua-fn
  "Defines a regular Lua function

  The function will receive LuaValue args that were passed by the editor script.
  It must return LuaValue (or Varargs) that will be returned back to the script.

  Returned function will be executed in an execution context that can be
  accessed using current-execution-context fn"
  [& fn-tail]
  `(DefoldVarArgFn. (fn ~@fn-tail)))

(defn- find-resource [project resource-path]
  (let [evaluation-context (:evaluation-context (current-execution-context))]
    (when-let [node (project/get-resource-node project (str "/" resource-path) evaluation-context)]
      (data/lines-input-stream (g/node-value node :lines evaluation-context)))))

(defn- resolve-file [^Path project-path ^String file-path]
  (let [target-path (.normalize (.resolve project-path file-path))]
    (if (.startsWith target-path project-path)
      (str target-path)
      (throw (LuaError. (format "Can't open %s: outside of project directory" file-path))))))

(defn read
  "Read a string with a chunk of lua code and return a Prototype for eval"
  ([chunk]
   (vm/read chunk "REPL"))
  ([chunk chunk-name]
   (vm/read chunk chunk-name)))

(def ^:private coronest-prototype
  (read (slurp (io/resource "coronest.lua")) "coronest.lua"))

(defn eval
  "Evaluate the Prototype produced by read and return resulting LuaValue"
  ^LuaValue [^EditorExtensionsRuntime runtime prototype]
  (vm/eval prototype (.-lua-vm runtime)))

(defn- writer->print-stream [^Writer writer]
  (PrintStream. (WriterOutputStream. writer StandardCharsets/UTF_8) true StandardCharsets/UTF_8))

(defn- merge-env-impl! [globals m]
  (reduce-kv
    (fn [^LuaValue acc k v]
      (let [lua-key (->lua k)
            old-lua-val (.get acc lua-key)]
        (if (and (map? v) (.istable old-lua-val))
          (merge-env-impl! old-lua-val v)
          (doto acc (.set lua-key (->lua v))))))
    globals
    m))

(defn make
  "Construct fresh Lua runtime for editor scripts

  Returned runtime is thread safe and does not block the LuaVM when it executes
  long-running computations started with suspendable functions.

  Optional kv-args:
    :out    Writer for the runtime standard output
    :err    Writer for the runtime error output
    :env    possibly nested map of values to merge with the initial environment"
  ^EditorExtensionsRuntime [project & {:keys [out err env] :or {out *out* err *err*}}]
  (let [project-path (g/with-auto-evaluation-context evaluation-context
                       (-> project
                           (project/workspace evaluation-context)
                           (workspace/project-path evaluation-context)
                           .toPath
                           .normalize))
        vm (vm/make)
        package-lib (PackageLib.)
        ;; We don't need to hold the lock on a LuaVM here because we are
        ;; creating it and no other thread can access it yet
        globals (doto (vm/env vm)
                  (.load (DefoldBaseLib. #(find-resource project %)))
                  (.load package-lib)
                  (.load (Bit32Lib.))
                  (.load (TableLib.))
                  (.load (StringLib.))
                  (.load (CoroutineLib.))
                  (.load (JseMathLib.))
                  (.load (DefoldIoLib. #(resolve-file project-path %)))
                  (.load (JseOsLib.))
                  (LoadState/install)
                  (LuaC/install)
                  (-> .-STDOUT (set! (writer->print-stream out)))
                  (-> .-STDERR (set! (writer->print-stream err))))
        package (.get globals "package")
        ;; Before splitting the coroutine module into 2 contexts (user and
        ;; system), we override the coroutine.create() function with a one that
        ;; conveys the thread bindings to the coroutine thread. This way, both
        ;; in system and user coroutines, the *execution-context* var will be
        ;; properly set.
        _ (doto (.get globals "coroutine")
            (.set "create" (DefoldCoroutineCreate. globals))
            (.set "yield" (DefoldCoroutineYield. globals)))

        ;; Now we split the coroutine into 2 contexts
        coronest (vm/eval coronest-prototype vm)
        user-coroutine (vm/invoke-1 vm coronest (->lua "user"))
        system-coroutine (vm/invoke-1 vm coronest (->lua "system"))]

    ;; Don't allow requiring java classes
    (.set package "searchers" (->lua [(PackageLib$preload_searcher. package-lib)
                                      (PackageLib$lua_searcher. package-lib)]))

    ;; Override coroutine as configured by CoroutineLib with user coroutine context
    (.set globals "coroutine" user-coroutine)
    (-> package (.get "loaded") (.set "coroutine" user-coroutine))

    ;; Always use forward slashes for resource path separators
    (.set package "searchpath" (SearchPath. globals))

    ;; Missing in luaj 3.0.1
    (.set package "config" (str File/separatorChar "\n;\n?\n!\n-"))

    (merge-env-impl! globals env)

    (EditorExtensionsRuntime.
      vm
      (.get system-coroutine "create")
      (.get system-coroutine "resume")
      (.get system-coroutine "status")
      (.get system-coroutine "yield"))))

(def ^:private lua-str-dead (->lua "dead"))

(defn- invoke-suspending-impl [execution-context ^EditorExtensionsRuntime runtime co & lua-args]
  (binding [*execution-context* execution-context]
    (let [vm (.-lua-vm runtime)
          [lua-success lua-ret] (apply vm/invoke-all vm (.-resume runtime) co lua-args)]
      (if (->clj runtime lua-success)
        (if (= lua-str-dead (vm/invoke-1 vm (.-status runtime) co))
          (future/completed lua-ret)
          (let [^Suspend suspend (->clj runtime lua-ret)]
            (-> (try
                  (apply (.-f suspend) (.-args suspend))
                  (catch LuaError e (future/completed (suspend-result-error e)))
                  (catch Throwable e (future/failed e)))
                (future/then-compose-async
                  (fn [^SuspendResult result]
                    (if (.-refresh-context result)
                      (do
                        (fx/on-fx-thread
                          (g/update-cache-from-evaluation-context! (:evaluation-context execution-context)))
                        (invoke-suspending-impl
                          {:evaluation-context (g/make-evaluation-context)
                           :runtime runtime
                           :mode :suspendable}
                          runtime co (->lua result)))
                      (invoke-suspending-impl execution-context runtime co (->lua result))))))))
        (future/failed (LuaError. ^String (->clj runtime lua-ret)))))))

(defn invoke-suspending
  "Invoke a potentially long-running LuaFunction

  Returns a CompletableFuture that will be either completed normally with the
  returned LuaValue or exceptionally. If exception is LuaError, treat it as a
  script error. Otherwise, treat it as editor error.

  Runtime will start invoking the LueFunction on the calling thread, then will
  move the execution to background threads if necessary. This means that
  invoke-suspending might return a completed CompletableFuture"
  [^EditorExtensionsRuntime runtime lua-fn & lua-args]
  (let [co (vm/invoke-1 (.-lua-vm runtime) (.-create runtime) lua-fn)
        execution-context {:evaluation-context (g/make-evaluation-context)
                           :runtime runtime
                           :mode :suspendable}]
    (apply invoke-suspending-impl execution-context runtime co lua-args)))

(defn invoke-immediate
  "Invoke a short-running LuaFunction

  Returns the result LuaValue. No calls to suspending functions are allowed
  during this invocation. Calling this function might throw an exception. If the
  exception is LuaError, treat is a script error. Otherwise, treat it as editor
  error.

  Args:
    runtime                the editor Lua runtime
    lua-fn                 LuaFunction to invoke
    args*                  0 or more LuaValue arguments
    evaluation-context?    optional evaluation context for the execution"
  {:arglists '([runtime lua-fn args* evaluation-context?])}
  [^EditorExtensionsRuntime runtime lua-fn & rest-args]
  (let [last-arg (last rest-args)
        context-provided (and (map? last-arg) (contains? last-arg :basis))
        evaluation-context (if context-provided last-arg (g/make-evaluation-context))
        lua-args (if context-provided (butlast rest-args) rest-args)
        result (binding [*execution-context* {:evaluation-context evaluation-context
                                              :runtime runtime
                                              :mode :immediate}]
                 (apply vm/invoke-1 (.-lua-vm runtime) lua-fn lua-args))]
    (when-not context-provided
      (g/update-cache-from-evaluation-context! evaluation-context))
    result))

(comment

  ;; I AM HERE!
  ;; TODO:
  ;;   1. Create tests for the runtime that ensure that it runs correctly in a
  ;;      multi-threaded environment
  ;;   2. Create extensions namespace that creates and configures the runtime
  ;;   3. Port current implementation

  (let [runtime (make (dev/project))
        vm (.-lua-vm runtime)
        slow-lua-fn (-> (vm/read "glob = 1 return function(x) local r = suspending_slow('foo', x, true) print(glob) return r end")
                        (vm/eval vm))
        fast-lua-fn (-> (vm/read "return function(x) glob = glob + 1 return x end")
                        (vm/eval vm))
        slow (invoke-suspending runtime slow-lua-fn (->lua 4))]
    (dotimes [_ 100]
      (future
        (dotimes [_ 100000]
          (invoke-immediate runtime fast-lua-fn (->lua 4)))))
    (tap> [:slow (->clj runtime (time @slow))]))

  (System/gc)

  (double (/ 80 1000)) ;; suspending: 0.08 ms per noop

  (let [runtime (make (dev/project))
        vm (.-lua-vm runtime)
        lua-fn (-> (vm/read "return function(x) return true end")
                   (vm/eval vm))]
    (time
      (g/with-auto-evaluation-context ec
        (dotimes [_ 1000000]
          (invoke-immediate runtime lua-fn (->lua 4) ec)))))

  (format "%f" (double (/ 400 1000000))) ;; 0.0004ms per noop

  (let [runtime (make (dev/project))
        vm (.-lua-vm runtime)
        lua-fn (-> (vm/read "return function(x)
                               local co = coroutine.create(resolve_suspending)
                               local s,v = coroutine.resume(co, x)
                               return {s,v}
                             end")
                   (vm/eval vm))]
    (vm/->clj @(invoke-suspending runtime lua-fn (->lua "ext/other.lua")) vm))

  (let [runtime (make (dev/project))
        vm (.-lua-vm runtime)
        lua-fn (-> (vm/read "return function(x)
                               local v, ret = pcall(resolve_suspending, x)
                               return {v, ret}
                             end")
                   (vm/eval vm))]
    (->clj runtime (invoke-immediate runtime lua-fn (->lua "ext/other.lua"))))

  #__)