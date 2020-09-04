(ns fell.error
  "Error effect."
  (:require [cats.core :refer [mlet fmap extract]]
            [cats.monad.either :refer [left right #?@(:cljs [Left Right])]]
            [fell.eff :refer [Effect #?@(:cljs [Pure Impure])]]
            [fell.queue :as q]
            [fell.core :refer [pure impure request-eff first-order-weave]])
  #?(:clj (:import [cats.monad.either Left Right]
                   [fell.eff Pure Impure])))

(defrecord Raise [error]
  Effect
  (weave [_ labeled cont suspension resume] (first-order-weave labeled cont suspension resume)))

(defrecord Handle [body on-error]
  Effect
  (weave [_ [label] cont suspension resume]
    (impure
      [label (Handle. (resume (fmap (constantly body) suspension))
                      (fn [error] (resume (fmap (constantly (on-error error)) suspension))))]
      (comp resume (partial fmap cont)))))

(defn raise
  "An Eff which raises `error`."
  [label error]
  (request-eff [label (Raise. error)]))

(defn handle
  "An Eff which handles Errors in `body` with `on-error` a fn from the error to an Eff."
  [label body on-error]
  (request-eff [label (Handle. body on-error)]))

(declare run)

(defn- resume [label suspension]
  (condp instance? suspension
    Left (pure suspension)
    Right (run (extract suspension) label)))

(defn run
  "Run the Error effect in the Eff `eff`."
  [eff label]
  (condp instance? eff
    Pure (pure (right (extract eff)))
    Impure (let [^Impure eff eff
                 [request-label op] (.-request eff)
                 k (partial q/apply-queue (.-cont eff))]
             (if (= request-label label)
               (condp instance? op
                 Raise (pure (left (.-error ^Raise op)))
                 Handle (mlet [:let [^Handle request op]
                               status (run (.-body request) label)]
                          (condp instance? status
                            Left (mlet [status (run ((.-on_error request) (extract status)) label)]
                                   (condp instance? status
                                     Left (pure status)
                                     Right (run (k (extract status)) label)))
                            Right (run (k (extract status)) label))))
               (fell.core/weave eff (right nil) (partial resume label))))))
