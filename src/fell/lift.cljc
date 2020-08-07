(ns fell.lift
  "Monad lifting effect.

  It is impossible to have more than one lifted monad."
  (:require [cats.core :refer [return bind extract]]
            [cats.data :refer [pair #?(:cljs Pair)]]
            [fell.eff :refer [Effect #?@(:cljs [Pure Impure])]]
            [fell.queue :as q]
            [fell.continuation :as cont]
            [fell.core :refer [impure request-eff]])
  #?(:clj (:import [cats.data Pair]
                   [fell.eff Pure Impure])))

(defrecord Lift [lifted-mv]
  Effect
  (weave [self k suspension handler] (impure self (cont/weave k suspension handler))))

(defn lift
  "Lift the monadic value `mv` into Eff."
  [mv]
  (request-eff (Lift. mv)))

(declare run)

(defn- resume-lift [^Pair suspension] (run (.-fst suspension) (.-snd suspension)))

(defn run
  "Handle the Lift effect in the Cats Monad determined by the [[cats.protocols.Context] `context`.
  All other effects must already be handled."
  [context eff]
  (condp instance? eff
    Pure (return context (extract eff))
    Impure (let [^Impure eff eff
                 request (.-request eff)
                 k (partial q/apply-queue (.-cont eff))]
             (condp instance? request
               Lift (bind (.-lifted_mv ^Lift request) (cont/weave k (pair context nil) resume-lift))
               (throw (#?(:clj RuntimeException., :cljs js/Error.)
                        (str "unhandled effect " (pr-str (.-request eff)))))))))
