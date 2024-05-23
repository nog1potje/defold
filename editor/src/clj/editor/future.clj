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
  `(CompletableFuture/supplyAsync (reify Supplier (get [~'_] ~@body))))

(defn completed
  ^CompletableFuture [x]
  (CompletableFuture/completedFuture x))

(defn failed
  ^CompletableFuture [ex]
  (CompletableFuture/failedFuture ex))

(defn fail!
  [^CompletableFuture future ex]
  (doto future (.completeExceptionally ex)))

(defn complete!
  [^CompletableFuture future x]
  (doto future (.complete x)))

(defn then
  ^CompletableFuture [^CompletableFuture future f]
  (.thenApply future (reify Function (apply [_ x] (f x)))))

(defn then-async
  [^CompletableFuture future f]
  (.thenApplyAsync future (reify Function (apply [_ x] (f x)))))

(defn then-compose-async
  [^CompletableFuture future f]
  (.thenComposeAsync future (reify Function (apply [_ x] (f x)))))

(defn catch [^CompletableFuture future f]
  (.exceptionally
    future
    (reify Function
      (apply [_ ex]
        (if (instance? CompletionException ex)
          (f (.getCause ^CompletionException ex))
          (f ex))))))