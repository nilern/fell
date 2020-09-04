(ns fell.state
  "State effect."
  (:refer-clojure :exclude [get set])
  (:require [cats.core :refer [extract]]
            [cats.data :refer [pair #?(:cljs Pair)]]
            [fell.eff :refer [Effect #?@(:cljs [Pure Impure])]]
            [fell.queue :as q]
            [fell.core :refer [pure impure request-eff first-order-weave]])
  #?(:clj (:import [cats.data Pair]
                   [fell.eff Pure Impure])))

(defrecord Get []
  Effect
  (weave [_ labeled cont suspension resume] (first-order-weave labeled cont suspension resume)))

(defrecord Set [new-value]
  Effect
  (weave [_ labeled cont suspension resume] (first-order-weave labeled cont suspension resume)))

(defn make [label]
  (let [get (request-eff [label (Get.)])]
    (letfn [(set [value*] (request-eff [label (Set. value*)]))

            (resume [^Pair suspension] (run (.-snd suspension) (.-fst suspension)))

            (run [eff state]
              (loop [state state, eff eff]
                (condp instance? eff
                  Pure (pure (pair state (extract eff)))
                  Impure (let [^Impure eff eff
                               [request-label op] (.-request eff)
                               k (partial q/apply-queue (.-cont eff))]
                           (if (= request-label label)
                             (condp instance? op
                               Get (recur state (k state))
                               Set (recur (.-new_value ^Set op) (k nil)))
                             (fell.core/weave eff (pair state nil) resume))))))]
      {:get get #_"An Eff that gets the State state value."
       :set set #_"`(set value*)` is an Eff that sets the State state value to `value*`."
       :run run}))) #_"Handle State effects in the Eff `eff` using `state` as the initial state value."
