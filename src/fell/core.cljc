(ns fell.core
  (:require [cats.core :refer [extract]]
            [cats.protocols :refer [Contextual Extract Context Monad]]
            [fell.queue :refer [singleton-queue]]))

(declare context)

(deftype Pure [v]
  Contextual
  (-get-context [_] context)

  Extract
  (-extract [_] v))

(def pure ->Pure)

(deftype Impure [request cont]
  Contextual
  (-get-context [_] context))

(def impure ->Impure)

(def context
  (reify
    Context

    Monad
    (-mreturn [_ v] (Pure. v))
    (-mbind [_ mv f]
      (condp instance? mv
        Pure (f (extract mv))
        Impure (impure (.-request mv) (conj (.-cont mv) f))))))

(defn default-queue? [queue]
  (and (= (count queue) 1)
       (identical? (peek queue) ->Pure)))

(defn- apply-queue [queue v]
  (let [mv ((peek queue) v)
        queue* (pop queue)]
    (if (seq queue*)
      (condp instance? mv
        Pure (recur queue* (extract mv))
        Impure (impure (.-request mv) (into (.-cont mv) queue*)))
      mv)))

(defn append-handler [queue handle]
  (comp handle (partial apply-queue queue)))

(defn request-eff [request-eff]
  (Impure. request-eff (singleton-queue ->Pure)))

(defn handle-relay [can-handle? ret handle eff]
  (condp instance? eff
    Pure (ret (extract eff))
    Impure (let [request (.-request eff)
                 cont (.-cont eff)
                 cont (append-handler cont (partial handle-relay can-handle? ret handle))]
             (if (can-handle? request)
               (handle request cont)
               (impure request (singleton-queue cont))))))

(def run extract)
