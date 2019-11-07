(ns fell.error
  "Error effect."
  (:require [cats.monad.either :refer [left right]]
            [fell.core :refer [pure request-eff handle-relay]]))

(defn make
  "Given the request keyword `tag`, return {:raise [[raise]]-for-`tag`, :run [[run-error]]-for-`tag`}."
  [tag]
  {:raise (fn [err] (request-eff [tag err]))
   :run (fn [eff]
          (handle-relay tag
                        (comp pure right)
                        (fn [[_ err] _] (pure (left err)))
                        eff))})

(let [{:keys [raise run]} (make ::error)]
  (def raise
    "`(raise err)` is an Eff that raises an Error effect `err`."
    raise)

  (def run-error
    "`(run-error eff)` runs the Error effect in `eff`, returning [[cats.monad.either/left]]
    if there was an error and [[cats.monad.either/right]] otherwise."
    run))
