(ns fell.eff
  "Implementation types for the freer monad Eff."
  (:require [cats.protocols :refer [Contextual Extract Context Functor Applicative Monad]]))

(defprotocol FFunctor
  (-ffmap [self f]))

(alter-meta! #'FFunctor assoc :private true)

(defprotocol FApplicative
  (-ffapply [self av]))

(alter-meta! #'FApplicative assoc :private true)

(defprotocol FlatMap
  (-flat-map [mv f]))

(alter-meta! #'FlatMap assoc :private true)

(defprotocol Effect
  (weave [self labeled cont state handler]))

(declare context)

;; No effects
(deftype Pure [v]
  Contextual
  (-get-context [_] context)

  Extract
  (-extract [_] v)

  FFunctor
  (-ffmap [_ f] (Pure. (f v)))

  FApplicative
  (-ffapply [_ av] (-ffmap av v))

  FlatMap
  (-flat-map [_ f] (f v)))

;; Effect and continuation queue
(deftype Impure [request cont]
  Contextual
  (-get-context [_] context)

  FFunctor
  (-ffmap [_ f] (Impure. request (conj cont (comp ->Pure f))))

  FApplicative
  (-ffapply [_ av] (Impure. request (conj cont (partial -ffmap av))))

  FlatMap
  (-flat-map [_ f] (Impure. request (conj cont f))))

(def context
  "A [[Context]] for Eff."
  (reify
    Context

    Functor
    (-fmap [_ f fv] (-ffmap fv f))

    Applicative
    (-pure [_ v] (Pure. v))
    (-fapply [_ af av] (-ffapply af av))

    Monad
    (-mreturn [_ v] (Pure. v))
    (-mbind [_ mv f] (-flat-map mv f))))
