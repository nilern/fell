(ns fell.core
  #?(:clj (:refer-clojure :exclude [send]))
  (:require [cats.protocols :refer [Contextual Extract -extract Context Monad]]
            [cats.core :refer [return bind]]))

(defn- singleton-queue [v]
  #?(:clj  (conj clojure.lang.PersistentQueue/EMPTY v)
     :cljs #queue [v]))

(declare context)

(deftype Pure [v]
  Contextual
  (-get-context [_] context)

  Extract
  (-extract [_] v))

(deftype Impure [request cont]
  Contextual
  (-get-context [_] context))

(def context
  (reify
    Context

    Monad
    (-mreturn [_ v] (Pure. v))
    (-mbind [_ mv f]
      (condp instance? mv
        Pure (f (-extract mv))
        Impure (Impure. (.-request mv) (conj (.-cont mv) f))))))

(defn- apply-queue [queue v]
  (let [mv ((peek queue) v)
        queue* (pop queue)]
    (if (seq queue*)
      (condp instance? mv
        Pure (recur queue* (-extract mv))
        Impure (Impure. (.-request mv) (into (.-cont mv) queue*)))
      mv)))

(defn append-handler [queue handle]
  (comp handle (partial apply-queue queue)))

(defn send [request]
  (Impure. request (singleton-queue ->Pure)))

(defn handle-relay [can-handle? ret handle mv]
  (condp instance? mv
    Pure (ret (-extract mv))
    Impure (let [request (.-request mv)
                 cont (append-handler (.-cont mv)
                                      (partial handle-relay can-handle? ret handle))]
             (if (can-handle? (.-request mv))
               (handle request cont)
               (Impure. request (singleton-queue cont))))))

(defn run-reader [mv env]
  (handle-relay #(= (first %) :reader/get) ->Pure (fn [[_] cont] (cont env)) mv))

(defn state-runner [label]
  (fn run-state [mv domain-state]
    (condp instance? mv
      Pure (Pure. [(-extract mv) domain-state])
      Impure (let [[tag subtag state* :as request] (.-request mv)
                   make-cont (fn [domain-state]
                               (append-handler (.-cont mv) #(run-state % domain-state)))]
               (if (= tag label)
                 (case subtag
                   :get ((make-cont domain-state) domain-state)
                   :set ((make-cont state*) nil))
                 (Impure. request (singleton-queue (make-cont domain-state))))))))

(defn lift [mv] (send [:lift/lift mv]))

(defn run-lift [ctx freer]
  (condp instance? freer
    Pure (return ctx (-extract freer))
    Impure (let [[tag mv] (.-request freer)]
             (case tag
               :lift/lift (bind mv (append-handler (.-cont freer) #(run-lift ctx %)))))))

(def run -extract)