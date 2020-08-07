(ns fell.error
  "Error effect."
  (:require [cats.core :refer [mlet fmap extract]]
            [cats.monad.either :refer [left right #?@(:cljs [Left Right])]]
            [fell.eff :refer [Effect weave #?@(:cljs [Pure Impure])]]
            [fell.queue :as q]
            [fell.core :refer [pure impure request-eff]])
  #?(:clj (:import [cats.monad.either Left Right]
                   [fell.eff Pure Impure])))

(defrecord Raise [error]
  Effect
  (weave [self k suspension handler]
    (impure self (q/singleton-queue (q/weave-fn k suspension handler)))))

(defrecord Handle [body on-error]
  Effect
  (weave [_ k suspension handler]
    (impure
      (Handle. (handler (fmap (constantly body) suspension))
               (fn [error] (handler (fmap (constantly (on-error error)) suspension))))
      (q/singleton-queue (comp handler (partial fmap k))))))

(defn raise [error] (request-eff (Raise. error)))

(defn handle [body on-error] (request-eff (Handle. body on-error)))

(declare run-error)

(defn- resume-error [suspension]
  (condp instance? suspension
    Left (pure suspension)
    Right (run-error (extract suspension))))

(defn run-error [eff]
  (condp instance? eff
    Pure (pure (right (extract eff)))
    Impure (let [^Impure eff eff
                 request (.-request eff)
                 k (partial q/apply-queue (.-cont eff))]
             (condp instance? request
               Raise (pure (left (.-error ^Raise request)))
               Handle (mlet [:let [^Handle request request]
                             status (run-error (.-body request))]
                        (condp instance? status
                          Left (mlet [status (run-error ((.-on_error request) (extract status)))]
                                 (condp instance? status
                                   Left (pure status)
                                   Right (run-error (k (extract status)))))
                          Right (run-error (k (extract status)))))
               (fell.core/weave eff (right nil) resume-error)))))
