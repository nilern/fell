(ns fell.reader
  "Reader effect."
  (:require [cats.core :refer [mlet fmap]]
            [cats.data :refer [pair #?(:cljs Pair)]]
            [fell.core :refer [request-eff]]
            [fell.eff :refer [Effect #?@(:cljs [Pure Impure])]]
            [fell.queue :as q])
  #?(:clj (:import [cats.data Pair]
                   [fell.eff Pure Impure])))

(defrecord Ask []
  Effect
  (weave [self _ _] self))

(defrecord Local [f body]
  Effect
  (weave [_ suspension handler]
    (Local. f (handler (fmap (constantly body) suspension)))))

(def ask (request-eff (Ask.)))

(defn local [f body] (request-eff (Local. f body)))

(declare run-reader)

(defn- resume-reader [^Pair suspension] (run-reader (.-snd suspension) (.-fst suspension)))

(defn run-reader [eff env]
  (loop [eff eff]
    (condp instance? eff
      Pure eff
      Impure (let [^Impure eff eff
                   request (.-request eff)
                   k (partial q/apply-queue (.-cont eff))]
               (condp instance? request
                 Ask (recur (k env))
                 Local (mlet [:let [^Local request request]
                              v (run-reader (.-body request) ((.-f request) env))]
                         (run-reader (k v) env))
                 (fell.core/weave eff (pair env nil) resume-reader))))))
