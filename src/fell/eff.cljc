(ns fell.eff
  "Implementation types for the freer monad Eff."
  (:require [cats.protocols :refer [Contextual Extract Context Monad]]))

#_(defprotocol FlatMap
  "Monadic bind without [[Context]]."
  (-flat-map [mv f]))

(declare context)

;; No effects
#_(deftype Pure [v]
  Contextual
  (-get-context [_] context)

  Extract
  (-extract [_] v)

  FlatMap
  (-flat-map [_ f] (f v)))

;; Effect and continuation queue
#_(deftype Impure [request cont]
  Contextual
  (-get-context [_] context)

  FlatMap
  (-flat-map [_ f] (Impure. request (conj cont f))))

#_(def context
  "A [[Context]] for Eff."
  (reify
    Context

    Monad
    (-mreturn [_ v] (Pure. v))
    (-mbind [_ mv f] (-flat-map mv f))))

(defprotocol Eff
  (run [self perform]))

(deftype Pure [v]
  Contextual
  (-get-context [_] context)

  Eff
  (run [_ _] v))

(deftype Bound [mv k]
  Contextual
  (-get-context [_] context)

  Eff
  (run [_ perform] (-> mv (run perform) k (run perform))))

(deftype Cue [cue]
  Contextual
  (-get-context [_] context)

  Eff
  (run [_ perform] (perform cue)))

(def context
  (reify
    Context

    Monad
    (-mreturn [_ v] (Pure. v))
    (-mbind [_ mv k] (Bound. mv k))))

(def request-eff ->Cue)
