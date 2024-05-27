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

(deftest eval-test
  (test-util/with-loaded-project
    (let [rt (rt/make project)
          p (rt/read "return 1")]
      (= 1 (rt/->clj rt (rt/eval rt p))))))

(deftest thread-safe-access-test
  (test-util/with-loaded-project
    (let [rt (rt/make project)
          _ (rt/eval rt (rt/read "global = 1"))
          inc-and-get (rt/read "return function () global = global + 1; return global end")
          lua-inc-and-get (rt/eval rt inc-and-get)
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
                                      (vec) ;; launch all threads in parallel
                                      (mapcat deref) ;; await
                                      (map #(rt/->clj rt %)))))
          (throw (Exception. "Lua runtime is not thread-safe!")))))))

(deftest immediate-invocations-complete-while-suspending-invocations-are-suspended
  (test-util/with-loaded-project
    (let [completable-future (future/make)
          rt (rt/make project
                      :env {"suspend_with_promise" (rt/suspendable-lua-fn [] completable-future)
                            "no_suspend" (rt/lua-fn [] (rt/->lua "immediate-result"))})
          calls-suspending (rt/eval rt (rt/read "return function() return suspend_with_promise() end "))
          calls-immediate (rt/eval rt (rt/read "return function() return no_suspend() end"))
          suspended-future (rt/invoke-suspending rt calls-suspending)]
      (is (false? (future/done? completable-future)))
      (is (= "immediate-result" (rt/->clj rt (rt/invoke-immediate rt calls-immediate))))
      (future/complete! completable-future (rt/suspend-result-success (rt/->lua "suspended-result") false))
      (when (is (true? (future/done? completable-future)))
        (is (= "suspended-result" (rt/->clj rt @suspended-future)))))))

(deftest suspending-calls-without-suspensions-complete-immediately
  (test-util/with-loaded-project
    (let [rt (rt/make project)
          lua-fib (rt/eval rt (rt/read "local function fib(n)
                                          if n <= 1 then
                                            return n
                                          else
                                            return fib(n - 1) + fib(n - 2)
                                          end
                                        end

                                        return fib"))]
      ;; 30th fibonacci takes awhile to complete, but still done immediately
      (is (future/done? (rt/invoke-suspending rt lua-fib (rt/->lua 30)))))))

(deftest suspending-calls-in-immediate-mode-are-disallowed
  (test-util/with-loaded-project
    (let [rt (rt/make project :env {"suspending" (rt/suspendable-lua-fn [] (future/make))})
          calls-suspending (rt/eval rt (rt/read "return function () suspending() end"))]
      (is (thrown-with-msg?
            LuaError
            #"Cannot use long-running editor function in immediate context"
            (rt/invoke-immediate rt calls-suspending))))))

(deftest user-coroutines-are-separated-from-system-coroutines
  (test-util/with-loaded-project
    (let [rt (rt/make project :env {"suspending" (rt/suspendable-lua-fn [x]
                                                                        (let [rt (:runtime (rt/current-execution-context))]
                                                                          (future/completed
                                                                            (rt/suspend-result-success (rt/->lua (inc (rt/->clj rt x))) false))))})
          coromix (rt/eval rt (rt/read "local function yield_twice(x)
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
                                        end"))]
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
          lua-fn (rt/eval rt (rt/read "local function yields_twice()
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
                                       end"))]
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
  (property value g/Any))

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
                                                (future/complete! f (rt/suspend-result-success (rt/->lua true) true))))
                                            f))})
          lua-fn (rt/eval rt (rt/read "return function()
                                         local v1 = get_value()
                                         local change_result = set_value(2)
                                         local v2 = get_value()
                                         return {v1, change_result, v2}
                                       end"))]
      (is (= [;; initial value
              1
              ;; success notification about change
              true
              ;; updated value
              2]
             (rt/->clj rt @(rt/invoke-suspending rt lua-fn)))))))

;; todo make suspend result a protocol extended to lua values, errors and special result that
;;      forces a reload (even with error!)

;; todo document and explain yield/create/resume situation