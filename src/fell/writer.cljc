(ns fell.writer
  (:require [cats.core :refer [mlet fmap mempty mappend extract]]
            [cats.context :as ctx]
            [cats.data :refer [pair #?(:cljs Pair)]]
            [fell.core :refer [pure impure request-eff]]
            [fell.eff :refer [Effect #?@(:cljs [Pure Impure])]]
            [fell.queue :as q]
            [fell.continuation :as cont])
  #?(:clj (:import [cats.data Pair]
                   [fell.eff Pure Impure])))

(defrecord Tell [message]
  Effect
  (weave [self k suspension handler] (impure self (cont/weave k suspension handler))))

(defrecord Listen [body]
  Effect
  (weave [_ k suspension handler]
    (impure (Listen. (handler (fmap (constantly suspension) body)))
            (comp handler (partial fmap k)))))

(defrecord Pass [body]
  Effect
  (weave [_ k suspension handler]
    (impure (Pass. (handler (fmap (constantly suspension) body)))
            (q/singleton-queue (comp handler (partial fmap k))))))

(defn tell [message] (request-eff (Tell. message)))

(defn listen [message] (request-eff (Listen. message)))

(defn pass [message] (request-eff (Pass. message)))

(declare resume run)

(defn- resume* [output eff]
  (condp instance? eff
    Pure (pure (pair output (extract eff)))
    Impure (let [^Impure eff eff
                 request (.-request eff)
                 k (partial q/apply-queue (.-cont eff))]
             (condp instance? request
               Tell (recur (mappend output (.-message ^Tell request)) (k nil))
               Listen (mlet [^Pair result (run (ctx/infer output) (.-body ^Listen request))
                             :let [output* (.-fst result)]]
                        (resume* (mappend output output*) (k result)))
               Pass (mlet [^Pair result (run (ctx/infer output) (.-body ^Pass request))
                           :let [output* (.-fst result)
                                 ^Pair vs (.-snd result)
                                 f (.-fst vs)
                                 v (.-snd vs)]]
                      (resume* (mappend output (f output*)) (k v)))
               (fell.core/weave eff (pair output nil) resume)))))

(defn- resume [^Pair suspension] (resume* (.-fst suspension) (.-snd suspension)))

(defn run [ctx eff] (resume* (mempty ctx) eff))
