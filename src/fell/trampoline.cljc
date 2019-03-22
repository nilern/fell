(ns fell.trampoline
  #?(:clj (:refer-clojure :exclude [send]))
  (:require [fell.core :refer [default-queue? append-handler request-eff eff-trampoline run]]
            [fell.queue :refer [singleton-queue]])
  (:import [fell.core Pure Impure Bounce]))

(def ^:private tail-position? default-queue?)

(defn tail-call [& args] (request-eff (into [::tail-call] args)))

(defn tail-apply [& args] (request-eff (into [::tail-apply] args)))

;; Eff (Trampoline | r) a -> Eff r a
(defn mtrampoline [eff]
  (condp instance? eff
    Pure eff
    Impure (let [request-eff (.-request eff)
                 [tag & args] request-eff]
             (case tag
               ::tail-call (let [[f & args] args]
                             (assert (tail-position? (.-cont eff)))
                             (recur (apply f args)))
               ::tail-apply (let [[f & args] args]
                              (assert (tail-position? (.-cont eff)))
                              (recur (apply apply f args))) ; WAT
               (Impure. request-eff (singleton-queue (append-handler (.-cont eff) mtrampoline)))))
    Bounce (recur (eff-trampoline eff))))

(defmacro mloop [bindings & body]
  (let [params (take-nth 2 bindings)
        args (take-nth 2 (rest bindings))]
    `(letfn [(luup# [~@params] ~@body)
             (~'mrecur [~@params] (tail-call luup# ~@params))]
       (mtrampoline (luup# ~@args)))))
