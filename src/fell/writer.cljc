(ns fell.writer
  "Writer effect."
  (:require [cats.core :refer [mlet fmap mempty mappend extract]]
            [cats.context :as ctx]
            [cats.data :refer [pair #?(:cljs Pair)]]
            [fell.core :refer [pure impure request-eff first-order-weave]]
            [fell.eff :refer [Effect #?@(:cljs [Pure Impure])]]
            [fell.queue :as q])
  #?(:clj (:import [cats.data Pair]
                   [fell.eff Pure Impure])))

(defrecord Tell [message]
  Effect
  (weave [_ labeled cont suspension resume] (first-order-weave labeled cont suspension resume)))

(defrecord Listen [body]
  Effect
  (weave [_ [label] cont suspension resume]
    (impure [label (Listen. (resume (fmap (constantly suspension) body)))]
            (comp resume (partial fmap cont)))))

(defrecord Pass [body]
  Effect
  (weave [_ [label] cont suspension resume]
    (impure [label (Pass. (resume (fmap (constantly suspension) body)))]
            (q/singleton-queue (comp resume (partial fmap cont))))))

(defn tell
  "An Eff which outputs `message`."
  [label message]
  (request-eff [label (Tell. message)]))

(defn listen
  "An Eff that pairs the result value of `body` with the Writer output from `body`."
  [label body]
  (request-eff [label (Listen. body)]))

(defn pass
  "An Eff which maps the first field of the [[cats.data.Pair]] result value of `body` over
  Writer messages from `body`."
  [label body]
  (request-eff [label (Pass. body)]))

(declare resume run)

(defn- resume* [label output eff]
  (condp instance? eff
    Pure (pure (pair output (extract eff)))
    Impure (let [^Impure eff eff
                 [request-label op] (.-request eff)
                 k (partial q/apply-queue (.-cont eff))]
             (if (= request-label label)
               (condp instance? op
                 Tell (recur label (mappend output (.-message ^Tell op)) (k nil))
                 Listen (mlet [^Pair result (run (.-body ^Listen op) label (ctx/infer output))
                               :let [output* (.-fst result)]]
                          (resume* label (mappend output output*) (k result)))
                 Pass (mlet [^Pair result (run (.-body ^Pass op) label (ctx/infer output))
                             :let [output* (.-fst result)
                                   ^Pair vs (.-snd result)
                                   f (.-fst vs)
                                   v (.-snd vs)]]
                        (resume* label (mappend output (f output*)) (k v))))
               (fell.core/weave eff (pair output nil) (partial resume label))))))

(defn- resume [label ^Pair suspension] (resume* label (.-fst suspension) (.-snd suspension)))

(defn run
  "Handle Writer effects in `body` using the Cats Monoid [[cats.protocols.Context]] `context`."
  [eff label context]
  (resume* label (mempty context) eff))
