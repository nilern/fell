(ns fell.core
  "The Eff a.k.a. Freer Monad."
  (:require [cats.core :refer [extract]]
            [fell.eff :as eff :refer [#?@(:cljs [Pure Impure]) ->Pure]]
            [fell.queue :as q :refer [singleton-queue]])
  #?(:clj (:import [fell.eff Pure Impure])))

(def pure
  "Inject the argument in an Eff without any effects."
  ->Pure)

(defn impure [request k]
  "Create an Eff from a request and a continuation.
  You mostly only need this when implementing new effects."
  (Impure. request (q/singleton-queue k)))

(defn request-eff
  "Wrap the effect `request` into an Eff."
  ^Impure [request]
  (Impure. request (singleton-queue pure)))

;; TODO: Improve `weave` nomenclature:
(defn weave [^Impure eff, state handler]
  (eff/weave (.-request eff) (partial q/apply-queue (.-cont eff)) state handler))

#_
(defn handle-relay
  "A generic effect handler that calls `(ret (extract eff))` when `eff` has
  no effects and handles requests tagged with `tag` by calling `(handle request cont)`.
  Using this takes care of the boilerplate where the handler needs to be
  added to the continuation queue in case the effect being handled will appear again when
  the continuation is called. However, not every effect handler is simple enough to be
  implemented with this function."
  [tag ret handle eff]
  (condp instance? eff
    Pure (ret (extract eff))
    Impure (let [^Impure eff eff
                 request (.-request eff)
                 cont (.-cont eff)
                 cont (append-handler cont (partial handle-relay tag ret handle))]
             (if (= (first request) tag)
               (handle request cont)
               (Impure. request (singleton-queue cont))))))

(defn run
  "Run the Eff `eff` and return the contained value. Throw if `eff` has unhandled effects."
  [eff]
  (condp instance? eff
    Pure (extract eff)
    Impure (let [^Impure eff eff]
             (throw (#?(:clj RuntimeException., :cljs js/Error.)
                      (str "unhandled effect " (pr-str (.-request eff))))))))
