(ns fell.trampoline
  #?(:clj (:refer-clojure :exclude [send]))
  (:require [fell.core :refer [default-queue? append-handler send run]]
            [fell.util :refer [singleton-queue]])
  (:import [fell.core Pure Impure]))

(def ^:private tail-position? default-queue?)

(defn tail-call [& args] (send (into [::tail-call] args)))

(defn tail-apply [& args] (send (into [::tail-apply] args)))

;; Eff (Trampoline | r) a -> Eff r a
(defn mtrampoline [eff]
  (condp instance? eff
    Pure eff
    Impure (let [request (.-request eff)
                 [tag & args] request]
             (case tag
               ::tail-call (let [[f & args] args]
                             (assert (tail-position? (.-cont eff)))
                             (recur (apply f args)))
               ::tail-apply (let [[f & args] args]
                              (assert (tail-position? (.-cont eff)))
                              (recur (apply apply f args))) ; WAT
               (Impure. request (singleton-queue (append-handler (.-cont eff) mtrampoline)))))))

(defmacro mloop [bindings & body]
  (let [params (take-nth 2 bindings)
        args (take-nth 2 (rest bindings))]
    `(letfn [(luup# [~@params] ~@body)
             (~'mrecur [& rec-args#] (tail-apply luup# rec-args#))]
       (mtrampoline (luup# ~@args)))))
