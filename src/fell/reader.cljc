(ns fell.reader
  "Reader effect."
  (:require [cats.core :refer [mlet fmap]]
            [cats.data :refer [pair #?(:cljs Pair)]]
            [fell.core :refer [impure request-eff first-order-weave]]
            [fell.eff :refer [Effect #?@(:cljs [Pure Impure])]]
            [fell.queue :as q])
  #?(:clj (:import [cats.data Pair]
                   [fell.eff Pure Impure])))

(defrecord Ask []
  Effect
  (weave [self cont suspension resume] (first-order-weave self cont suspension resume)))

(defrecord Local [f body]
  Effect
  (weave [_ cont suspension resume]
    (impure (Local. f (resume (fmap (constantly body) suspension)))
            (comp resume (partial fmap cont)))))

(def ask
  "An Eff which gets the Reader value."
  (request-eff (Ask.)))

(defn local
  "An Eff which uses `(f old-reader-value)` as the Reader value in `body`."
  [f body]
  (request-eff (Local. f body)))

(declare run)

(defn- resume-reader [^Pair suspension] (run (.-snd suspension) (.-fst suspension)))

(defn run
  "Handle Reader effects in `eff` using `env` as the Reader value."
  [eff env]
  (loop [eff eff]
    (condp instance? eff
      Pure eff
      Impure (let [^Impure eff eff
                   request (.-request eff)
                   k (partial q/apply-queue (.-cont eff))]
               (condp instance? request
                 Ask (recur (k env))
                 Local (mlet [:let [^Local request request]
                              v (run (.-body request) ((.-f request) env))]
                         (run (k v) env))
                 (fell.core/weave eff (pair env nil) resume-reader))))))
