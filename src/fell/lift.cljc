(ns fell.lift
  "Monad lifting effect.

  It is impossible to have more than one lifted monad."
  (:require [cats.core :refer [return extract]]
            [cats.protocols :refer [-mbind]]
            [cats.data :refer [pair #?(:cljs Pair)]]
            [fell.eff :refer [Effect #?@(:cljs [Pure Impure])]]
            [fell.queue :as q]
            [fell.continuation :as cont]
            [fell.core :refer [impure request-eff first-order-weave]])
  #?(:clj (:import [cats.data Pair]
                   [fell.eff Pure Impure])))

(defrecord Lift [lifted-mv]
  Effect
  (weave [_ labeled cont suspension resume] (first-order-weave labeled cont suspension resume)))

(defn make [label]
  (letfn [(lift [mv] (request-eff [label (Lift. mv)]))

          (resume [^Pair suspension] (run (.-snd suspension) (.-fst suspension)))

          (run [eff context]
            (condp instance? eff
              Pure (return context (extract eff))
              Impure (let [^Impure eff eff
                           [request-label op] (.-request eff)
                           k (partial q/apply-queue (.-cont eff))]
                       (if (= request-label label)
                         (condp instance? op
                           Lift (-mbind context
                                        (.-lifted_mv ^Lift op)
                                        (cont/weave k (pair context nil) resume)))
                         (throw (#?(:clj RuntimeException., :cljs js/Error.)
                                  (str "unhandled effect " (pr-str (.-request eff)))))))))]
    {:lift lift #_"Lift the monadic value `mv` into Eff."
     :run run})) #_"Handle the Lift effect in the Cats Monad determined by the [[cats.protocols.Context] `context`.
            All other effects must already be handled."
