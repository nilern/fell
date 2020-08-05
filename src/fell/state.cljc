(ns fell.state
  "State effect."
  (:refer-clojure :exclude [get set])
  (:require [cats.core :refer [extract fmap]]
            [cats.data :refer [pair]]
            [fell.eff :refer [Effect weave #?@(:cljs [Pure Impure])]]
            [fell.queue :as q :refer [apply-queue]]
            [fell.core :refer [pure impure request-eff #?@(:cljs [Pure Impure])]]
            [fell.queue :refer [singleton-queue]])
  #?(:clj (:import [fell.eff Pure Impure])))

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

(defn- resume-state [suspension] (run-state (.-snd suspension) (.-fst suspension)))

(defn run-state
  "`(run-state eff state)` runs the State effect in Eff `eff` using `state` as the initial state value."
  [eff state]
  (loop [state state, eff eff]
    (condp instance? eff
      Pure (pair state (extract eff))
      Impure (let [request (.-request eff)
                   cont (.-cont eff)]
               (condp instance? request
                 Get (recur state (apply-queue cont state))
                 Set (recur (.-new_value request) (apply-queue cont nil))
                 (let [suspension (pair state nil)]
                   (impure (weave request suspension resume-state)
                           (singleton-queue (q/weave cont suspension resume-state)))))))))
