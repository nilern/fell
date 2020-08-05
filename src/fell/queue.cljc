(ns fell.queue
  "Continuation queue for freer monad."
  (:require [cats.core :refer [extract fmap]]
            [fell.eff :refer [#?@(:cljs [Pure Impure])]])
  #?(:clj (:import [clojure.lang PersistentQueue]
                   [fell.eff Pure Impure])))

(def empty-queue
  #?(:clj  PersistentQueue/EMPTY
     :cljs #queue []))

(defn singleton-queue [v]
  #?(:clj  (conj PersistentQueue/EMPTY v)
     :cljs #queue [v]))

(defn apply-queue
  "Call the continuation queue `queue` with `v`."
  [queue v]
  (let [eff ((peek queue) v)
        queue* (pop queue)]
    (if (seq queue*)
      (condp instance? eff
        Pure (recur queue* (extract eff))
        Impure (Impure. (.-request eff) (into (.-cont eff) queue*)))
      eff)))

(defn append-handler
  "Compose the continuation `queue` with the effect `handler`, returning a function."
  [queue handle]
  (comp handle (partial apply-queue queue)))

(defn weave [queue state handler]
  (singleton-queue (fn [x] (handler (fmap (constantly (apply-queue queue x)) state)))))
