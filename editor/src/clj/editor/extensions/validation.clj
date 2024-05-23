(ns editor.extensions.validation
  (:import [org.luaj.vm2 LuaFunction LuaValue]))

(defn lua-fn? [x]
  (instance? LuaFunction x))

(defn lua-value? [x]
  (instance? LuaValue x))
