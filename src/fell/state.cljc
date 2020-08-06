(ns fell.state
  "State effect."
  (:refer-clojure :exclude [get set])
  (:require [cats.core :refer [extract]]
            [cats.data :refer [pair #?(:cljs Pair)]]
            [fell.eff :refer [Effect weave #?@(:cljs [Pure Impure])]]
            [fell.queue :as q]
            [fell.core :refer [pure impure request-eff #?@(:cljs [Pure Impure])]])
  #?(:clj (:import [cats.data Pair]
                   [fell.eff Pure Impure])))

(declare run-state)

(defrecord Get []
  Effect
  (weave [self _ _] self))

(defrecord Set [new-value]
  Effect
  (weave [self _ _] self))

(def get
  "An Eff that gets the State state value."
  (request-eff (Get.)))

(defn set
  "`(set value*)` is an Eff that sets the State state value to `value*`."
  [value*]
  (request-eff (Set. value*)))

(defn- resume-state [^Pair suspension] (run-state (.-snd suspension) (.-fst suspension)))

(defn run-state
  "`(run-state eff state)` runs the State effect in Eff `eff` using `state` as the initial state value."
  [eff state]
  (loop [state state, eff eff]
    (condp instance? eff
      Pure (pure (pair state (extract eff)))
      Impure (let [^Impure eff eff
                   request (.-request eff)
                   k (partial q/apply-queue (.-cont eff))]
               (condp instance? request
                 Get (recur state (k state))
                 Set (recur (.-new_value ^Set request) (k nil))
                 (fell.core/weave eff (pair state nil) resume-state))))))
