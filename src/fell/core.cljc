(ns fell.core
  "The Eff a.k.a. Freer Monad."
  (:require [cats.core :refer [extract]]
            [fell.eff :as eff :refer [#?@(:cljs [Pure Impure]) ->Pure]]
            [fell.queue :as q :refer [singleton-queue]]
            [fell.continuation :as cont])
  #?(:clj (:import [fell.eff Pure Impure])))

(def pure
  "Inject the argument in an Eff without any effects."
  ->Pure)

(defn impure [request cont]
  "Create an Eff from a request and a continuation fn.
  You mostly only need this when implementing new effects."
  (Impure. request (q/singleton-queue cont)))

(defn request-eff
  "Wrap the effect `request` into an Eff."
  ^Impure [request]
  (Impure. request (singleton-queue pure)))

;; TODO: Improve `weave` nomenclature:
(defn weave
  "Weave `suspension` and `resume` into the Impure Eff `eff`."
  [^Impure eff, suspension resume]
  (let [[_ op :as request] (.-request eff)]
    (eff/weave op request (partial q/apply-queue (.-cont eff)) suspension resume)))

(defn first-order-weave
  "Weave `suspension` and `resume` into the first order effect `request` and continuation fn `cont`."
  [request cont suspension resume]
  (impure request (cont/weave cont suspension resume)))

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
