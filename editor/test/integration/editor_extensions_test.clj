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

(ns integration.editor-extensions-test
  (:require [clojure.test :refer :all]
            [dynamo.graph :as g]
            [editor.extensions.runtime :as rt]
            [editor.future :as future]
            [editor.graph-util :as gu]
            [editor.ui :as ui]
            [integration.test-util :as test-util])
  (:import [org.luaj.vm2 LuaError]))

(deftest read-bind-test
  (test-util/with-loaded-project
    (let [rt (rt/make project)
          p (rt/read "return 1")]
      (= 1 (rt/->clj rt (rt/invoke-immediate rt (rt/bind rt p)))))))

(deftest thread-safe-access-test
  (test-util/with-loaded-project
    (let [rt (rt/make project)
          _ (rt/invoke-immediate rt (rt/bind rt (rt/read "global = 1")))
          inc-and-get (rt/read "return function () global = global + 1; return global end")
          lua-inc-and-get (rt/invoke-immediate rt (rt/bind rt inc-and-get))
          ec (g/make-evaluation-context)
          threads 10
          per-thread-calls 1000
          iterations 100]
      (dotimes [_ iterations]
        (when-not (is (distinct? (->> (fn []
                                        (future
                                          (->> #(rt/invoke-immediate rt lua-inc-and-get ec)
                                               (repeatedly per-thread-calls)
                                               (vec))))
                                      (repeatedly threads)
                                      (vec)                 ;; launch all threads in parallel
                                      (mapcat deref)        ;; await
                                      (map #(rt/->clj rt %)))))
          (throw (Exception. "Lua runtime is not thread-safe!")))))))

(deftest immediate-invocations-complete-while-suspending-invocations-are-suspended
  (test-util/with-loaded-project
    (let [completable-future (future/make)
          rt (rt/make project
                      :env {"suspend_with_promise" (rt/suspendable-lua-fn [] completable-future)
                            "no_suspend" (rt/lua-fn [] (rt/->lua "immediate-result"))})
          calls-suspending (rt/invoke-immediate rt (rt/bind rt (rt/read "return function() return suspend_with_promise() end ")))
          calls-immediate (rt/invoke-immediate rt (rt/bind rt (rt/read "return function() return no_suspend() end")))
          suspended-future (rt/invoke-suspending rt calls-suspending)]
      (is (false? (future/done? completable-future)))
      (is (= "immediate-result" (rt/->clj rt (rt/invoke-immediate rt calls-immediate))))
      (future/complete! completable-future "suspended-result")
      (when (is (true? (future/done? completable-future)))
        (is (= "suspended-result" (rt/->clj rt @suspended-future)))))))

(deftest suspending-calls-without-suspensions-complete-immediately
  (test-util/with-loaded-project
    (let [rt (rt/make project)
          lua-fib (->> (rt/read "local function fib(n)
                                   if n <= 1 then
                                     return n
                                   else
                                     return fib(n - 1) + fib(n - 2)
                                   end
                                 end

                                 return fib")
                       (rt/bind rt)
                       (rt/invoke-immediate rt))]
      ;; 30th fibonacci takes awhile to complete, but still done immediately
      (is (future/done? (rt/invoke-suspending rt lua-fib (rt/->lua 30)))))))

(deftest suspending-calls-in-immediate-mode-are-disallowed
  (test-util/with-loaded-project
    (let [rt (rt/make project :env {"suspending" (rt/suspendable-lua-fn [] (future/make))})
          calls-suspending (->> (rt/read "return function () suspending() end")
                                (rt/bind rt)
                                (rt/invoke-immediate rt))]
      (is (thrown-with-msg?
            LuaError
            #"Cannot use long-running editor function in immediate context"
            (rt/invoke-immediate rt calls-suspending))))))

(deftest user-coroutines-are-separated-from-system-coroutines
  (test-util/with-loaded-project
    (let [rt (rt/make project :env {"suspending" (rt/suspendable-lua-fn [x]
                                                   (let [rt (:runtime (rt/current-execution-context))]
                                                     (inc (rt/->clj rt x))))})
          coromix (->> (rt/read "local function yield_twice(x)
                                   local y = coroutine.yield(suspending(x))
                                   coroutine.yield(suspending(y))
                                   return 'done'
                                 end

                                 return function(n)
                                   local co = coroutine.create(yield_twice)
                                   local success1, result1 = coroutine.resume(co, n)
                                   local success2, result2 = coroutine.resume(co, result1)
                                   local success3, result3 = coroutine.resume(co, result2)
                                   local success4, result4 = coroutine.resume(co, result2)
                                   return {
                                     {success1, result1},
                                     {success2, result2},
                                     {success3, result3},
                                     {success4, result4},
                                   }
                                 end")
                       (rt/bind rt)
                       (rt/invoke-immediate rt))]
      (is (= [;; first yield: incremented input
              [true 6]
              ;; second yield: incremented again
              [true 7]
              ;; not a yield, but a return value
              [true "done"]
              ;; user coroutine done, nothing to return
              [false "cannot resume dead coroutine"]]
             (rt/->clj rt @(rt/invoke-suspending rt coromix (rt/->lua 5))))))))

(deftest user-coroutines-work-normally-in-immediate-mode
  (test-util/with-loaded-project
    (let [rt (rt/make project)
          lua-fn (->> (rt/read "local function yields_twice()
                                  coroutine.yield(1)
                                  coroutine.yield(2)
                                  return 'done'
                                end

                                return function()
                                  local co = coroutine.create(yields_twice)
                                  local success1, result1 = coroutine.resume(co)
                                  local success2, result2 = coroutine.resume(co)
                                  local success3, result3 = coroutine.resume(co)
                                  local success4, result4 = coroutine.resume(co)
                                  return {
                                    {success1, result1},
                                    {success2, result2},
                                    {success3, result3},
                                    {success4, result4},
                                  }
                                end")
                      (rt/bind rt)
                      (rt/invoke-immediate rt))]
      (is (= [;; first yield: 1
              [true 1]
              ;; second yield: 2
              [true 2]
              ;; not a yield, but a return value
              [true "done"]
              ;; user coroutine done, nothing to return
              [false "cannot resume dead coroutine"]]
             (rt/->clj rt (rt/invoke-immediate rt lua-fn)))))))

(g/defnode TestNode
  (property value g/Any)
  (output value g/Any :cached (gu/passthrough value)))

(deftest suspendable-functions-can-refresh-contexts
  (test-util/with-loaded-project
    (let [node-id (g/make-node! (g/node-id->graph-id project) TestNode :value 1)
          rt (rt/make project
                      :env {"get_value" (rt/lua-fn []
                                          (let [ec (:evaluation-context (rt/current-execution-context))]
                                            (rt/->lua (g/node-value node-id :value ec))))
                            "set_value" (rt/suspendable-lua-fn [n]
                                          (let [f (future/make)]
                                            (let [rt (:runtime (rt/current-execution-context))
                                                  set-val! (bound-fn []
                                                             (g/set-property! node-id :value (rt/->clj rt n)))]
                                              (ui/run-later
                                                (set-val!)
                                                (future/complete! f (rt/and-refresh-context true))))
                                            f))})
          lua-fn (->> (rt/read "return function()
                                  local v1 = get_value()
                                  local change_result = set_value(2)
                                  local v2 = get_value()
                                  return {v1, change_result, v2}
                                end")
                      (rt/bind rt)
                      (rt/invoke-immediate rt))]
      (is (= [;; initial value
              1
              ;; success notification about change
              true
              ;; updated value
              2]
             (rt/->clj rt @(rt/invoke-suspending rt lua-fn)))))))


(deftest suspending-lua-failure-test
  (test-util/with-loaded-project
    (let [rt (rt/make project :env {"suspend_fail_immediately" (rt/suspendable-lua-fn []
                                                                 (throw (LuaError. "failed immediately")))
                                    "suspend_fail_async" (rt/suspendable-lua-fn []
                                                           (future/failed (LuaError. "failed async")))})
          lua-fn (->> (rt/read "return function()
                                  local success1, value1 = pcall(suspend_fail_immediately)
                                  local success2, value2 = pcall(suspend_fail_async)
                                  return {
                                    {success1, value1},
                                    {success2, value2},
                                  }
                                end")
                      (rt/bind rt)
                      (rt/invoke-immediate rt))]
      (is (= [[false "failed immediately"]
              [false "failed async"]]
             (rt/->clj rt @(rt/invoke-suspending rt lua-fn)))))))

(deftest immediate-failures-test
  (test-util/with-loaded-project
    (let [rt (rt/make project :env {"immediate_error" (rt/lua-fn []
                                                        (throw (Exception. "fail")))
                                    "suspend_error" (rt/suspendable-lua-fn []
                                                      "success")})]
      (is
        (= [[false "fail"]
            [false "Cannot use long-running editor function in immediate context"]]
           (->> (rt/read "local success1, result1 = pcall(immediate_error)
                          local success2, result2 = pcall(suspend_error)
                          return {
                            {success1, result1},
                            {success2, result2}
                          }")
                (rt/bind rt)
                (rt/invoke-immediate rt)
                (rt/->clj rt)))))))