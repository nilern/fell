(ns fell.eff
  (:require [cats.core :refer [extract]]
            [cats.protocols :refer [Contextual Extract Context Monad]]))

(defprotocol FlatMap
  "Monadic bind without [[Context]]."
  (-flat-map [mv f]))

(declare context)

;; No effects
(deftype Pure [v]
  Contextual
  (-get-context [_] context)

  Extract
  (-extract [_] v)

  FlatMap
  (-flat-map [_ f] (f v)))

;; Effect and continuation queue
(deftype Impure [request cont]
  Contextual
  (-get-context [_] context)

  FlatMap
  (-flat-map [_ f] (Impure. request (conj cont f))))

(def context
  "A [[Context]] for Eff."
  (reify
    Context

    Monad
    (-mreturn [_ v] (Pure. v))
    (-mbind [_ mv f] (-flat-map mv f))))
