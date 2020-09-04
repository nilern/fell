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
  (weave [_ labeled cont suspension resume] (first-order-weave labeled cont suspension resume)))

(defrecord Local [f body]
  Effect
  (weave [_ [label] cont suspension resume]
    (impure [label (Local. f (resume (fmap (constantly body) suspension)))]
            (comp resume (partial fmap cont)))))

(defn make [label]
  (let [ask (request-eff [label (Ask.)])]
    (letfn [(local [f body] (request-eff [label (Local. f body)]))

            (resume [^Pair suspension] (run (.-snd suspension) (.-fst suspension)))

            (run [eff env]
              (loop [eff eff]
                (condp instance? eff
                  Pure eff
                  Impure (let [^Impure eff eff
                               [request-label op] (.-request eff)
                               k (partial q/apply-queue (.-cont eff))]
                           (if (= request-label label)
                             (condp instance? op
                               Ask (recur (k env))
                               Local (mlet [:let [^Local request op]
                                            v (run (.-body request) ((.-f request) env))]
                                       (run (k v) env)))
                             (fell.core/weave eff (pair env nil) resume))))))]
      {:ask ask #_"An Eff which gets the Reader value."
       :local local #_"An Eff which uses `(f old-reader-value)` as the Reader value in `body`."
       :run run}))) #_"Handle Reader effects in `eff` using `env` as the Reader value."
