(ns fell.fiber
  (:require [fell.core :refer [pure impure append-handler request-eff eff-trampoline run
                               #?@(:cljs [Pure Impure Bounce])]]
            [fell.queue :refer [singleton-queue]])
  #?(:clj (:import [fell.core Pure Impure Bounce])))

;; ((() -> Eff (Fiber | r) a) -> Eff (Fiber | r) b) -> Eff (Fiber | r) ()
(defn suspend [f] (request-eff [::suspend f]))

;; (() -> a) -> Eff (Fiber | r) ()
(defn schedule [thunk] (request-eff [::schedule thunk]))

;; (() -> a) -> ()
(defn schedule! [thunk]
  #?(:clj  (future-call thunk)
     :cljs (.requestIdleCallback js/window (fn [_] (thunk))))
  nil)

;; Eff (Fiber | r) a => Eff r a
(defn run-fibers [eff]
  (condp instance? eff
    Pure eff
    Impure (let [request (.-request eff)
                 [tag & args] request]
             (case tag
               ::suspend (let [[f] args]
                           (recur (f (append-handler (.-cont eff) run-fibers))))
               ::schedule (let [[thunk] args]
                            (pure (schedule! thunk)))
               (impure request (singleton-queue (append-handler (.-cont eff) run-fibers)))))
    Bounce (recur (eff-trampoline eff))))

;; (() -> a) -> Eff (Fiber | r) ()
(def spawn schedule)

;; Eff (Fiber | r) ()
(def yield (suspend (fn [k] (schedule (fn [] (run (k nil)))))))
