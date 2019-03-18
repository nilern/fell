(ns fell.cml)

(def ^:private empty-queue
  #?(:clj  clojure.lang.PersistentQueue/EMPTY
     :cljs #queue []))

(defn- cas!
  "Compare-and-swap; iff `@atom` is `old`, [[reset!]] it to `new`. Return `@atom`."
  [atom old new]
  (let [res (volatile! nil)]
    (swap! atom (fn [v]
                  (vreset! res v)
                  (if (identical? v old) new v)))
    @res))

(defprotocol Synchronizable
  (try-sync [self])
  (block [self resume state]))

(defprotocol Receivable
  (recv [self]))

(deftype ReceiveSuspension [resume state])

(deftype SendSuspension [value resume state])

(deftype Channel [recvq sendq]
  Receivable
  (recv [_]
    (reify
      Synchronizable
      (try-sync [_]
        (let [senders @sendq]
          (when (seq senders)
            (let [sender (peek senders)]
              (case (cas! (.-state sender) :waiting :synced)
                :waiting (do ((.-resume sender) nil)
                             (cas! sendq senders (pop senders))
                             (fn [] (.-value sender)))
                nil)))))

      (block [_ resume state]
        (swap! recvq conj (ReceiveSuspension. resume state))
        (loop []
          (let [senders @sendq]
            (when (seq senders)
              (let [sender (peek senders)]
                (case (cas! state :waiting :claimed)
                  :waiting (case (cas! (.-state sender) :waiting :synced)
                             :waiting (do (reset! state :synced)
                                          (cas! sendq senders (pop senders))
                                          ((.-resume sender) nil)
                                          (resume (.-value sender)))
                             :claimed (do (reset! state :waiting)
                                          (recur))
                             :synced (do (reset! state :waiting)
                                         (cas! sendq senders (pop senders))
                                         (recur)))
                  :synced nil)))))))))

(defn chan []
  (Channel. (atom empty-queue) (atom empty-queue)))
