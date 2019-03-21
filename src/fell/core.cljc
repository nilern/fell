(ns fell.core
  (:require [cats.core :refer [bind extract]]
            [cats.protocols :refer [Contextual Extract Context Monad]]
            [fell.queue :refer [singleton-queue]]))

(defprotocol FlatMap
  (-flat-map [mv f]))

(declare context)

(deftype Pure [v]
  Contextual
  (-get-context [_] context)

  Extract
  (-extract [_] v)

  FlatMap
  (-flat-map [_ f] (f v)))

(def pure ->Pure)

(deftype Impure [request cont]
  Contextual
  (-get-context [_] context)

  FlatMap
  (-flat-map [_ f] (Impure. request (conj cont f))))

(def impure ->Impure)

(deftype Bounce [thunk cont handlers]
  Contextual
  (-get-context [_] context)

  FlatMap
  (-flat-map [_ f] (Bounce. thunk (conj cont f) handlers)))

(defn bounce [thunk]
  (Bounce. thunk (singleton-queue pure) []))

(def context
  (reify
    Context

    Monad
    (-mreturn [_ v] (Pure. v))
    (-mbind [_ mv f] (-flat-map mv f))))

(defn default-queue? [queue]
  (and (= (count queue) 1)
       (identical? (peek queue) pure)))

(defn- apply-queue [queue v]
  (let [eff ((peek queue) v)
        queue* (pop queue)]
    (if (seq queue*)
      (condp instance? eff
        Pure (recur queue* (extract eff))
        Impure (impure (.-request eff) (into (.-cont eff) queue*))
        Bounce (Bounce. (.-thunk eff) (.-handlers eff) (into (.-cont eff) queue*)))
      eff)))

(defn append-handler [queue handle]
  (comp handle (partial apply-queue queue)))

(defn request-eff [request-eff]
  (Impure. request-eff (singleton-queue pure)))

(defn handle-relay [can-handle? ret handle eff]
  (condp instance? eff
    Pure (ret (extract eff))
    Impure (let [request (.-request eff)
                 cont (.-cont eff)
                 cont (append-handler cont (partial handle-relay can-handle? ret handle))]
             (if (can-handle? request)
               (bounce #(handle request cont))
               (impure request (singleton-queue cont))))
    Bounce (Bounce. (.-thunk eff) (.-cont eff)
                    (conj (.-handlers eff) (partial handle-relay can-handle? ret handle)))))

(defn run [eff]
  (condp instance? eff
    Pure (extract eff)
    Impure (throw (#?(:clj RuntimeException., :cljs Error.) (str "unhandled effect " (pr-str (.-request eff)))))
    ;; FIXME: Probably not even correct and seems to make GC scream on all cores:
    Bounce (let [eff* (reduce bind ((.-thunk eff)) (.-cont eff))
                 eff* (reduce (fn [eff handler] (handler eff)) eff* (.-handlers eff))]
             (recur eff*))))
