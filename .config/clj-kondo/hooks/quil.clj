(ns hooks.quil
  (:require [clj-kondo.hooks-api :as api]))

(defn defsketch
  [{:keys [node]}]
  (let [[var-name-node & body] (rest (:children node))

        _ (when-not (api/token-node? var-name-node)
            (throw (ex-info "Missing sketch name!" {})))

        new-node (api/list-node
                   (remove nil?
                           (list (api/token-node 'def)
                                 var-name-node
                                 (api/map-node body))))]
    {:node new-node}))
