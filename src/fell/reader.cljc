(ns fell.reader
  "Reader effect."
  (:require [cats.core :refer [mlet fmap]]
            [cats.data :refer [pair #?(:cljs Pair)]]
            [fell.core :refer [impure request-eff]]
            [fell.eff :refer [Effect #?@(:cljs [Pure Impure])]]
            [fell.queue :as q]
            [fell.continuation :as cont])
  #?(:clj (:import [cats.data Pair]
                   [fell.eff Pure Impure])))

(defrecord Ask []
  Effect
  (weave [self k suspension handler] (impure self (cont/weave k suspension handler))))

(defrecord Local [f body]
  Effect
  (weave [_ k suspension handler]
    (impure (Local. f (handler (fmap (constantly body) suspension)))
            (comp handler (partial fmap k)))))

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
