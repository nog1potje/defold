(ns editor.extensions.luart
  (:refer-clojure :exclude [eval read-string])
  (:import [java.io ByteArrayInputStream]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent.locks ReentrantLock]
           [org.luaj.vm2 Globals LuaBoolean LuaClosure LuaDouble LuaInteger LuaNil LuaString LuaTable LuaUserdata LuaValue Prototype]
           [org.luaj.vm2.compiler LuaC]))

(set! *warn-on-reflection* true)

(deftype LuaRT [globals lock coroutine])

(defn- ^Globals rt-globals [^LuaRT rt]
  (.-globals rt))

(defn- ^ReentrantLock rt-lock [^LuaRT rt]
  (.-lock rt))

(defn- ^LuaTable rt-coroutine [^LuaRT rt]
  (.-coroutine rt))

(defmacro with-lock [rt-expr & body]
  `(let [lock# (rt-lock ~rt-expr)]
     (try
       (.lock lock#)
       (do ~@body)
       (finally (.unlock lock#)))))

(defn make []
  (let [globals (doto (Globals.))]
    (LuaRT. globals (ReentrantLock.) nil)))

(defn read-string
  "Read a string with a chunk of lua code and return a Prototype for eval"
  (^Prototype [chunk]
   (read-string chunk "REPL"))
  (^Prototype [chunk chunk-name]
   (.compile LuaC/instance
             (ByteArrayInputStream. (.getBytes ^String chunk StandardCharsets/UTF_8))
             chunk-name)))

(defn eval
  "Evaluate the Prototype produced by read-string and return resulting LuaValue"
  ^LuaValue [prototype rt]
  (with-lock rt
    (.call (LuaClosure. prototype (rt-globals rt)))))

(defn ->clj-impl [lua-value]
  ;; Class hierarchy:
  ;; Varargs
  ;;   LuaValue
  ;;     LuaTable
  ;;     LuaThread
  ;;     LuaBoolean
  ;;     LuaUserdata
  ;;     LuaString
  ;;     LuaNumber
  ;;       LuaDouble
  ;;       LuaInteger
  ;;     LuaNil
  ;;     File
  ;;     LuaFunction
  (condp instance? lua-value
    LuaBoolean (.toboolean ^LuaBoolean lua-value)
    LuaUserdata (.userdata ^LuaUserdata lua-value)
    LuaString (.tojstring ^LuaString lua-value)
    LuaDouble (.todouble ^LuaDouble lua-value)
    LuaInteger (.tolong ^LuaInteger lua-value)
    LuaNil nil
    LuaTable (let [^LuaTable table lua-value]
               (loop [prev-k LuaValue/NIL
                      acc-v nil
                      acc-m nil]
                 (let [varargs (.next table prev-k)
                       lua-k (.arg1 varargs)]
                   (if (.isnil lua-k)
                     (or (some-> acc-v persistent!)
                         (some-> acc-m persistent!)
                         {})
                     (let [lua-v (.arg varargs 2)
                           clj-k (->clj-impl lua-k)
                           clj-v (->clj-impl lua-v)]
                       (cond
                         acc-m
                         (recur lua-k nil (assoc! acc-m (cond-> clj-k (string? clj-k) keyword) clj-v))

                         (pos-int? clj-k) ;; grow acc-v
                         (let [acc-v (or acc-v (transient []))
                               i (dec clj-k) ;; 1-indexed to 0-indexed
                               acc-v (loop [acc-v acc-v]
                                       (if (< (count acc-v) i)
                                         (recur (conj! acc-v nil))
                                         acc-v))]
                           (recur lua-k (assoc! acc-v i clj-v) nil))

                         :else ;; convert to map
                         (let [acc-m (transient {(cond-> clj-k (string? clj-k) keyword) clj-v})
                               acc-m (if acc-v
                                       (let [len (count acc-v)]
                                         (loop [i 0
                                                acc-m acc-m]
                                           (if (= i len)
                                             acc-m
                                             (let [v (acc-v i)]
                                               (if (nil? v)
                                                 (recur (inc i) acc-m)
                                                 (recur (inc i) (assoc! acc-m (inc i) v)))))))
                                       acc-m)]
                           (recur lua-k nil acc-m))))))))
    lua-value))

(defn ->clj
  "Convert Lua data structure to Clojure data structure

  Converts tables either to:
  - vectors if all keys are ints
  - maps otherwise

  Preserves LuaThread, File (from IoLib) and LuaFunction"
  [lua-value rt]
  (with-lock rt
    (->clj-impl lua-value)))

;; read does not require lock
;; eval requires lock
;; lua->clj requires lock
;; clj->lua does not require lock

;; use reentrant lock
;; 2 coroutine contexts

;; bottom api (this ns):
;; read, eval, conversions, coroutines
;; top api (other ns -- or this?????):
;; invoke-immediate
;; invoke-suspending - need to figure out which thread to choose
