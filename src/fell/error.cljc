(ns fell.error
  "Error effect."
  (:require [cats.core :refer [mlet fmap extract]]
            [cats.monad.either :refer [left right #?@(:cljs [Left Right])]]
            [fell.eff :refer [Effect weave #?@(:cljs [Pure Impure])]]
            [fell.queue :as q]
            [fell.core :refer [pure request-eff]])
  #?(:clj (:import [cats.monad.either Left Right]
                   [fell.eff Pure Impure])))

(defrecord Raise [error]
  Effect
  (weave [self _ _] self))

(defrecord Handle [body on-error]
  Effect
  (weave [_ suspension handler]
    (Handle. (handler (fmap (constantly body) suspension))
             (fn [error] (handler (fmap (constantly (on-error error)) suspension))))))

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
    Impure (let [request (.-request eff)
                 cont (.-cont eff)]
             (condp instance? request
               Raise (pure (left (.-error request)))
               Handle (mlet [status (run-error (.-body request))]
                        (condp instance? status
                          Left (mlet [status (run-error ((.-on_error request) (extract status)))]
                                 (condp instance? status
                                   Left (pure status)
                                   Right (run-error (q/apply-queue cont (extract status)))))
                          Right (run-error (q/apply-queue cont (extract status)))))
               (fell.core/weave eff (right nil) resume-error)))))
