(ns fell.lift
  "Monad lifting effect.

  It is impossible to have more than one lifted monad, so there is no `make` function here."
  (:require [cats.core :refer [return bind extract]]
            [cats.data :refer [pair]]
            [fell.eff :refer [Effect #?@(:cljs [Pure Impure])]]
            [fell.queue :as q]
            [fell.core :refer [request-eff]])
  #?(:clj (:import [fell.eff Pure Impure])))

(defrecord Lift [lifted-mv]
  Effect
  (weave [self _ _] self))

(declare run-lift)

(defn- resume-lift [suspension] (run-lift (.-fst suspension) (.-snd suspension)))

(defn run-lift [ctx eff]
  (condp instance? eff
    Pure (return ctx (extract eff))
    Impure (let [request (.-request eff)
                 cont (.-cont eff)]
             (condp instance? request
               Lift (bind (.-lifted_mv request) (q/weave->fn cont (pair ctx nil) resume-lift))
               (throw (#?(:clj RuntimeException., :cljs js/Error.)
                        (str "unhandled effect " (pr-str (.-request eff)))))))))
