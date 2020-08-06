(ns fell.writer
  (:require [cats.core :refer [mlet fmap mempty mappend extract]]
            [cats.context :as ctx]
            [cats.data :refer [pair]]
            [fell.core :refer [pure request-eff]]
            [fell.eff :refer [Effect #?@(:cljs [Pure Impure])]]
            [fell.queue :as q])
  #?(:clj (:import [fell.eff Pure Impure])))

(defrecord Tell [message]
  Effect
  (weave [self _ _] self))

(defrecord Listen [body]
  Effect
  (weave [_ suspension handler]
    (Listen. (handler (fmap (constantly suspension) body)))))

(defrecord Pass [body]
  Effect
  (weave [_ suspension handler]
    (Pass. (handler (fmap (constantly suspension) body)))))

(defn tell [message] (request-eff (Tell. message)))

(defn listen [message] (request-eff (Listen. message)))

(defn pass [message] (request-eff (Pass. message)))

(declare resume run)

(defn- resume* [output eff]
  (condp instance? eff
    Pure (pure (pair output (extract eff)))
    Impure (let [request (.-request eff)
                 cont (.-cont eff)]
             (condp instance? request
               Tell (recur (mappend output (.-message request)) (q/apply-queue cont nil))
               Listen (mlet [result (run (ctx/infer output) (.-body request))
                             :let [output* (.-fst result)]]
                        (resume* (mappend output output*) (q/apply-queue cont result)))
               Pass (mlet [result (run (ctx/infer output) (.-body request))
                           :let [output* (.-fst result)
                                 f (.-fst (.-snd result))
                                 v (.-snd (.-snd result))]]
                      (resume* (mappend output (f output*)) (q/apply-queue cont v)))
               (fell.core/weave eff (pair output nil) resume)))))

(defn- resume [suspension] (resume* (.-fst suspension) (.-snd suspension)))

(defn run [ctx eff] (resume* (mempty ctx) eff))
