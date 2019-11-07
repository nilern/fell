(ns fell.lift
  "Monad lifting effect.

  It is impossible to have more than one lifted monad, so there is no `make` function here."
  (:require [cats.core :refer [return bind extract]]
            [fell.eff :refer [#?@(:cljs [Pure Impure])]]
            [fell.queue :refer [append-handler]]
            [fell.core :refer [request-eff]])
  #?(:clj (:import [fell.eff Pure Impure])))

(defn lift
  "Lift the monadic value `mv` to an Eff, gaining the ability to add other effects on top of that monad."
  [mv]
  (request-eff [::lift mv]))

(defn run-lift
  "Run an Eff `eff` that has just the Lift effect using the Cats monad context `ctx`.
  Essentially like [[fell.core/run]] but you get to use our lifted monad."
  [ctx eff]
  (condp instance? eff
    Pure (return ctx (extract eff))
    Impure (let [[tag mv] (.-request eff)
                 cont (.-cont eff)]
             (case tag
               ::lift (bind mv (append-handler cont (partial run-lift ctx)))
               (throw (#?(:clj RuntimeException., :cljs js/Error.)
                        (str "unhandled effect " (pr-str (.-request eff)))))))))
