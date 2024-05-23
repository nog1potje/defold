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
  This runtime also splits coroutine into 2 contexts: 'user' and 'system'. With
  this split, editor-related suspends are transparent to the editor script, so
  the editor script may still use coroutines internally without interfering with
  the editor.

  This namespace defines 2 ways to run Lua code:
    1. invoke-suspending - call the LuaFunction in a system coroutine, allowing
       it to use suspendable functions defined by the editor.
    2. invoke-immediate - call the LuaFunction with suspendable functions
       defined by the editor disabled. This way there is no coroutine overhead,
       and code executes significantly faster."
  (:require [cljfx.api :as fx]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [dynamo.graph :as g]
            [editor.code.data :as data]
            [editor.defold-project :as project]
            [editor.extensions.validation :as validation]
            [editor.extensions.vm :as vm]
            [editor.future :as future]
            [editor.workspace :as workspace])
  (:import [com.defold.editor.luart DefoldBaseLib DefoldIoLib DefoldVarArgFn SearchPath]
           [java.io File PrintStream Writer]
           [java.nio.charset StandardCharsets]
           [java.nio.file Path]
           [org.apache.commons.io.output WriterOutputStream]
           [org.luaj.vm2 LoadState LuaClosure LuaError LuaFunction LuaUserdata LuaValue Varargs]
           [org.luaj.vm2.compiler LuaC]
           [org.luaj.vm2.lib Bit32Lib CoroutineLib PackageLib PackageLib$lua_searcher PackageLib$preload_searcher StringLib TableLib]
           [org.luaj.vm2.lib.jse JseMathLib JseOsLib]))

(set! *warn-on-reflection* true)

(deftype EditorExtensionsRuntime [lua-vm coroutine-create coroutine-resume coroutine-status])
(deftype Suspend [f varargs])
(deftype SuspendResult [lua-value lua-error refresh-context])

(defn- suspend-result-success
  [lua-value refresh-context]
  (SuspendResult. lua-value nil refresh-context))

(defn- suspend-result-error
  [lua-error]
  (SuspendResult. nil lua-error false))

(defn lua-vm
  [^EditorExtensionsRuntime runtime]
  (.-lua-vm runtime))

(defn- system-coroutine-create
  "Thread-safe"
  ^LuaFunction [^EditorExtensionsRuntime runtime]
  (.-coroutine-create runtime))

(defn- system-coroutine-resume
  "Not thread-safe"
  ^LuaFunction [^EditorExtensionsRuntime runtime]
  (.-coroutine-resume runtime))

(defn- system-coroutine-status
  "Not thread-safe"
  ^LuaFunction [^EditorExtensionsRuntime runtime]
  (.-coroutine-status runtime))

(def ^:private ^:dynamic *execution-context*
  "A map with following keys:
    :evaluation-context    the evaluation context for this execution"
  nil)

(def ^:private coronest-prototype
  (vm/read (slurp (io/resource "coronest.lua")) "coronest.lua"))

(defn- current-evaluation-context []
  {:pre [(some? *execution-context*)]}
  (:evaluation-context *execution-context*))

(defn- find-resource [project resource-path]
  (let [evaluation-context (current-evaluation-context)]
    (when-let [node (project/get-resource-node project (str "/" resource-path) evaluation-context)]
      (data/lines-input-stream (g/node-value node :lines evaluation-context)))))

(defn- resolve-file [^Path project-path ^String file-path]
  (let [target-path (.normalize (.resolve project-path file-path))]
    (if (.startsWith target-path project-path)
      (str target-path)
      (throw (LuaError. (format "Can't open %s: outside of project directory" file-path))))))

(defn- writer->print-stream [^Writer writer]
  (PrintStream. (WriterOutputStream. writer StandardCharsets/UTF_8) true StandardCharsets/UTF_8))

(defn- make [project & {:keys [out err]
                        :or {out *out* err *err*}}]
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
        env (doto (vm/env vm)
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
        package (.get env "package")
        coronest (.call (LuaClosure. coronest-prototype env))
        user-coroutine (.call coronest "user")
        system-coroutine (.call coronest "system")
        yield (.get system-coroutine "yield")]

    ;; Don't allow requiring java classes
    (.set package "searchers" (vm/->lua [(PackageLib$preload_searcher. package-lib)
                                         (PackageLib$lua_searcher. package-lib)]))

    ;; Override coroutine as configured by CoroutineLib with user coroutine context
    (.set env "coroutine" user-coroutine)
    (-> package (.get "loaded") (.set "coroutine" user-coroutine))

    ;; Always use forward slashes for resource path separators
    (.set package "searchpath" (SearchPath. env))

    ;; Missing in luaj 3.0.1
    (.set package "config" (str File/separatorChar "\n;\n?\n!\n-"))

    ;; todo this is a test example, move suspending fn construction elsewhere
    (.set env "suspending" (DefoldVarArgFn.
                             ;; Don't lock on the vm here! coroutine runs in the luaj-managed
                             ;; coroutine thread, we trust luaj to do their job
                             (fn [^Varargs varargs]
                               (let [f (fn [& args]
                                         ;; my example suspend fn
                                         (future/completed (suspend-result-success (first args) false)))]
                                 (let [^SuspendResult result (.userdata ^LuaUserdata (.call yield (vm/->lua (Suspend. f varargs))))]
                                   (if-let [lua-error (.-lua-error result)]
                                     (throw lua-error)
                                     (.-lua-value result)))))))
    (EditorExtensionsRuntime.
      vm
      (.get system-coroutine "create")
      (.get system-coroutine "resume")
      (.get system-coroutine "status"))))

(def ^:private lua-str-dead (vm/->lua "dead"))

(defn- invoke-suspending-impl [execution-context vm resume status co lua-value]
  (binding [*execution-context* execution-context]
    (let [[lua-success lua-ret] (vm/invoke-all vm resume co lua-value)]
      (if (vm/->clj lua-success vm)
        (if (= lua-str-dead (vm/invoke-1 vm status co))
          (future/completed lua-ret)
          (let [^Suspend suspend (vm/->clj lua-ret vm)]
            (-> (try
                  (apply (.-f suspend) (vm/parse-varargs (.-varargs suspend)))
                  (catch LuaError e (future/completed (suspend-result-error e)))
                  (catch Throwable e (future/failed e)))
                (future/then-compose-async
                  (fn [^SuspendResult result]
                    ;; This is executed asynchronously, no execution context set
                    (if (.-refresh-context result)
                      (do
                        (fx/on-fx-thread
                          (g/update-cache-from-evaluation-context! (:evaluation-context execution-context)))
                        (invoke-suspending-impl {:evaluation-context (g/make-evaluation-context)} vm resume status co (vm/->lua result)))
                      (invoke-suspending-impl execution-context vm resume status co (vm/->lua result))))))))
        (future/failed (LuaError. ^String (vm/->clj lua-ret vm)))))))


(defn invoke-suspending [runtime ^LuaFunction lua-fn ^LuaValue lua-value]
  {:pre [(validation/lua-fn? lua-fn)
         (validation/lua-value? lua-value)]}
  (let [co (.call (system-coroutine-create runtime) lua-fn)
        resume (system-coroutine-resume runtime)
        status (system-coroutine-status runtime)
        vm (lua-vm runtime)
        execution-context {:evaluation-context (g/make-evaluation-context)}]
    (invoke-suspending-impl execution-context vm resume status co lua-value)))

(def ^String ^:private cannot-suspend-error-suffix
  "cannot yield main thread")

(defn invoke-immediate
  ([runtime lua-fn lua-value]
   (g/with-auto-evaluation-context evaluation-context
     (invoke-immediate runtime lua-fn lua-value evaluation-context)))
  ([runtime lua-fn lua-value evaluation-context]
   (binding [*execution-context* {:evaluation-context evaluation-context}]
     (try
       (vm/invoke-1 (lua-vm runtime) lua-fn lua-value)
       (catch LuaError ex
         (let [msg (.getMessage ex)]
           (if (string/ends-with? msg cannot-suspend-error-suffix)
             (throw (LuaError. (str (subs msg 0 (- (.length msg) (.length cannot-suspend-error-suffix)))
                                    "cannot use long-running editor function in this context")))
             (throw ex))))))))


(comment

  ;; TODO
  ;;   - documentations
  ;; todo document semantics:
  ;;   semantics:
  ;;   returns a CompletionStage, i.e. eventually this will either fail or succeed.
  ;;   starts execution on the calling thread, then switches to background thread
  ;;   when needs to do async tasks
  ;;   if exception is a lua error, treat it as editor script errors
  ;; todo document suspend function construction
  ;;   must return a future that will receive SuspendResult
  ;;   if throws, lua errors are treated as editor script errors
  ;;   - I will finish editor extensions porting later

  (let [runtime (make (dev/project))
        vm (lua-vm runtime)
        lua-fn (-> (vm/read "return function(x) return suspending('foo') end")
                   (vm/eval vm))]
    (time
      (dotimes [_ 1000]
        @(invoke-suspending runtime lua-fn (vm/->lua 4))))
    (vm/->clj @(invoke-suspending runtime lua-fn (vm/->lua 4)) vm))

  (double (/ 80 1000)) ;; suspending: 0.08 ms per noop

  (let [runtime (make (dev/project))
        vm (lua-vm runtime)
        lua-fn (-> (vm/read "return function(x) return true end")
                   (vm/eval vm))]
    (time
      (g/with-auto-evaluation-context ec
        (dotimes [_ 1000000]
          (invoke-immediate runtime lua-fn (vm/->lua 4) ec)))))

  (format "%f"(double (/ 400 1000000))) ;; 0.0004ms per noop

  (let [runtime (make (dev/project))
        vm (lua-vm runtime)
        lua-fn (-> (vm/read "return function(x) return {1, 2, 3, x} end")
                   (vm/eval vm))]
    (time
      (g/with-auto-evaluation-context ec
        (dotimes [_ 1000000]
          (invoke-immediate runtime lua-fn (vm/->lua 4) ec)))))

  #__)