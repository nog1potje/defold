(ns editor.future
  (:refer-clojure :exclude [future])
  (:import [java.util.concurrent CompletableFuture CompletionException]
           [java.util.function Supplier Function]))

(defn make
  (^CompletableFuture []
   (CompletableFuture.))
  (^CompletableFuture [f]
   (CompletableFuture/supplyAsync (reify Supplier (get [_] (f))))))

(defmacro supply-async [& body]
  ;; todo thread bindings!
  `(CompletableFuture/supplyAsync (reify Supplier (get [~'_] ~@body))))

(defn completed
  ^CompletableFuture [x]
  (CompletableFuture/completedFuture x))

(defn wrap [x]
  (if (instance? CompletableFuture x)
    x
    (completed x)))

(defn failed
  ^CompletableFuture [ex]
  (CompletableFuture/failedFuture ex))

(defn fail!
  [^CompletableFuture future ex]
  (doto future (.completeExceptionally ex)))

(defn done? [^CompletableFuture future]
  (.isDone future))

(defn complete!
  [^CompletableFuture future x]
  (doto future (.complete x)))

(defn then-compose-async
  [^CompletableFuture future f]
  (let [f (bound-fn* f)]
    (.thenComposeAsync future (reify Function (apply [_ x] (f x))))))

(defn catch [^CompletableFuture future f]
  (let [f (bound-fn* f)]
    (.exceptionally
      future
      (reify Function
        (apply [_ ex]
          (if (instance? CompletionException ex)
            (f (.getCause ^CompletionException ex))
            (f ex)))))))