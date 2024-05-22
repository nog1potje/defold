(ns editor.luart2
  (:refer-clojure :exclude [read-string eval])
  (:import [java.io ByteArrayInputStream]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent.locks ReentrantLock]
           [org.luaj.vm2 Globals LoadState LuaBoolean LuaClosure LuaDouble LuaFunction LuaInteger LuaNil LuaString LuaUserdata LuaValue Prototype Varargs]
           [org.luaj.vm2.compiler LuaC]
           [org.luaj.vm2.lib Bit32Lib CoroutineLib PackageLib StringLib TableLib]
           [org.luaj.vm2.lib.jse JseBaseLib JseMathLib JseOsLib]))

;; read does not require lock
;; eval requires lock
;; lua->clj requires lock
;; clj->lua does not require lock

(set! *warn-on-reflection* true)

(deftype LuaEnv [^Globals globals ^ReentrantLock lock])

(deftype LuaFn [^LuaFunction f ^LuaEnv env])

(defprotocol ->Clj (->clj [v env]))
(defprotocol ->Lua (->lua [v]))

(extend-protocol ->Lua
  Number (->lua [v] (LuaValue/valueOf (.doubleValue v))))

#_(defn- lua-wrapper [^LuaFunction f ^LuaEnv env]
    (fn [& args]
      (let [^Varargs varargs (LuaValue/varargsOf (into-array LuaValue (mapv ->lua args)))
            ^ReentrantLock lock (.-lock env)]
        (try
          (.lock lock)
          (->clj (.invoke f varargs) env)
          (finally (.unlock lock))))))

;; table
;; thread
;; weakvalue
;; file
;; function

;; We don't do varargs!
(extend-protocol ->Clj
  LuaNil (->clj [_ _] nil)
  LuaBoolean (->clj [v _] (.toboolean v))
  LuaInteger (->clj [v _] (.tolong v))
  LuaDouble (->clj [v _] (.todouble v))
  LuaString (->clj [v _] (.tojstring v))
  LuaUserdata (->clj [v _] (.userdata v))
  LuaFunction (->clj [v env] (LuaFn. v env)))

(defn env
  ^LuaEnv []
  (LuaEnv.
    (doto (Globals.)
      (.load (JseBaseLib.))
      (.load (PackageLib.))
      (.load (Bit32Lib.))
      (.load (TableLib.))
      (.load (StringLib.))
      (.load (CoroutineLib.))
      (.load (JseMathLib.))
      (.load (JseOsLib.))
      (LoadState/install)
      (LuaC/install))
    (ReentrantLock.)))

(defn read-string
  (^Prototype [s]
   (read-string s "REPL"))
  (^Prototype [s chunk-name]
   (.compile LuaC/instance
             (ByteArrayInputStream. (.getBytes ^String s StandardCharsets/UTF_8))
             chunk-name)))

(defn eval [prototype ^LuaEnv env]
  (let [closure (LuaClosure. prototype (.-globals env))
        ^ReentrantLock lock (.-lock env)]
    (try
      (.lock lock)
      (->clj (.call closure) env)
      (finally (.unlock lock)))))

;; invoke api:
;; invoke-1 (get first value, should un-vararg the thing)
;; invoke-all (get all values, always a vector) - not needed, our APIs only want 1 return
;; value

((eval (read-string "return function(a,b,c) print(a,b,c) return a+b+c, 'heh' end") (env))
 1 2 10)

(let [p (read-string "return function(x)
                        return function()
                          return math.abs(x) + 1
                        end
                      end")
      ^Globals g (.-globals (env))
      c (LuaClosure. p g)
      root (.call c)
      lv (LuaValue/valueOf -5)]
  (time (dotimes [_ 1000000]
          (.call root lv))))

;; what if...
;; 1. we call a function
;; 2. it calls an editor API
;; 3. the editor API spawns a background thread, and on that thread it calls
;;    another lua function => deadlock :(
;; okay, what about system coroutines? that's the thing that should prevent deadlocks
;; by returning control to the caller, right?



;; local state for cljfx??? where does it live? how does it work? when I change the local
;; state, when do I re-draw? next frame? how does the component know about it?

(require 'cljfx.lifecycle)

(def ext-local-state
  (reify cljfx.lifecycle/Lifecycle
    (create [this {:keys [desc initial-state]} opts]
      (let [state (volatile! initial-state)
            on-state-changed ...] ;; todo update the volatile, schedule a refresh
        (cljfx.lifecycle/create cljfx.lifecycle/dynamic (assoc :state initial-state))))
    (advance [this component desc opts]
      ;; if request is scheduled, unschedule and do refresh
      ;; is initial state is different, re-create
      ;; TODO: change handlers so re-setting the fn handlers does not create new listeners!
      ;; TODO: the problem with local state: what if the view changes the returned instance?
      ;;       then we need the parent to update! how? supply mutator as an argument to
      ;;       ext-local-state??? possible? access parent??
      "Advances component")
    (delete [this component opts]
      ;; cancel scheduled request
      "Deletes component")))

{:fx/type ext-local-state
 :initial-state "Hello!"
 :desc {:fx/type (fn [{:keys [state on-state-changed]}])}}