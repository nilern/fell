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
  (weave [self cont suspension resume] (first-order-weave self cont suspension resume)))

(defrecord Handle [body on-error]
  Effect
  (weave [_ cont suspension resume]
    (impure
      (Handle. (resume (fmap (constantly body) suspension))
               (fn [error] (resume (fmap (constantly (on-error error)) suspension))))
      (comp resume (partial fmap cont)))))

(defn raise
  "An Eff which raises `error`."
  [error]
  (request-eff (Raise. error)))

(defn handle
  "An Eff which handles Errors in `body` with `on-error` a fn from the error to an Eff."
  [body on-error]
  (request-eff (Handle. body on-error)))

(declare run)

(defn- resume-error [suspension]
  (condp instance? suspension
    Left (pure suspension)
    Right (run (extract suspension))))

(defn run
  "Run the Error effect in the Eff `eff`."
  [eff]
  (condp instance? eff
    Pure (pure (right (extract eff)))
    Impure (let [^Impure eff eff
                 request (.-request eff)
                 k (partial q/apply-queue (.-cont eff))]
             (condp instance? request
               Raise (pure (left (.-error ^Raise request)))
               Handle (mlet [:let [^Handle request request]
                             status (run (.-body request))]
                        (condp instance? status
                          Left (mlet [status (run ((.-on_error request) (extract status)))]
                                 (condp instance? status
                                   Left (pure status)
                                   Right (run (k (extract status)))))
                          Right (run (k (extract status)))))
               (fell.core/weave eff (right nil) resume-error)))))
