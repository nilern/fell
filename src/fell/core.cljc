(ns fell.core
  "The Eff a.k.a. Freer Monad."
  (:require [cats.core :refer [extract]]
            [cats.protocols :refer [Contextual Extract Context Monad]]
            [fell.queue :refer [singleton-queue]]))

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

(def pure
  "Inject the argument in an Eff without any effects."
  ->Pure)

;; Effect and continuation queue
(deftype Impure [request cont]
  Contextual
  (-get-context [_] context)

  FlatMap
  (-flat-map [_ f] (Impure. request (conj cont f))))

(def impure
  "Create an Eff from a request and a continuation queue. You mostly only need this when implementing new effects."
  ->Impure)

(declare eff-trampoline)

(def context
  "A [[Context]] for Eff."
  (reify
    Context

    Monad
    (-mreturn [_ v] (Pure. v))
    (-mbind [_ mv f] (-flat-map mv f))))

(defn default-queue?
  "Is `queue` the default continuation queue? Can be used to detect requests in tail position."
  [queue]
  (and (= (count queue) 1)
       (identical? (peek queue) pure)))

(defn- apply-queue
  "Call the continuation queue `queue` with `v`."
  [queue v]
  (let [eff ((peek queue) v)
        queue* (pop queue)]
    (if (seq queue*)
      (condp instance? eff
        Pure (recur queue* (extract eff))
        Impure (impure (.-request eff) (into (.-cont eff) queue*)))
      eff)))

(defn append-handler
  "Compose the continuation `queue` with the effect `handler`, returning a function."
  [queue handle]
  (comp handle (partial apply-queue queue)))

(defn request-eff
  "Wrap the effect `request` into an Eff."
  [request]
  (Impure. request (singleton-queue pure)))

(defn handle-relay
  "A generic effect handler that calls `(ret (extract eff))` when `eff` has no effects and handles requests where
  `(= (can-handle? request) true)` with `(bounce handle request cont)`. Using this takes care of the boilerplate where
  the handler needs to be added to the continuation queue in case the effect being handled will appear again when
  the continuation is called and also calling [[eff-trampoline]] for [[Bounce]]. However, not every effect handler
  is simple enough to benefit from this function."
  [can-handle? ret handle eff]
  (condp instance? eff
    Pure (ret (extract eff))
    Impure (let [request (.-request eff)
                 cont (.-cont eff)
                 cont (append-handler cont (partial handle-relay can-handle? ret handle))]
             (if (can-handle? request)
               (bounce handle request cont)
               (impure request (singleton-queue cont))))))

(defn run
  "Run the Eff `eff` and return the contained value. Throw if `eff` has unhandled effects."
  [eff]
  (condp instance? eff
    Pure (extract eff)
    Impure (throw (#?(:clj RuntimeException., :cljs js/Error.) (str "unhandled effect " (pr-str (.-request eff)))))))
