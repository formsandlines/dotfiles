(ns hooks.instaparse
  (:require [clj-kondo.hooks-api :as api]))

(defn defparser
  [{:keys [:node]}]
  (let [[var-name-node grammar-node & opt-node] (rest (:children node))

        _ (when-not (api/token-node? var-name-node)
            (throw (ex-info "Missing parser name!" {})))

        _ (when-not (api/string-node? grammar-node)
            (throw (ex-info "Missing grammar specification or URI!" {})))
        
        new-node (api/list-node
                   (list (api/token-node 'def)
                         var-name-node
                         grammar-node
                         (api/map-node opt-node)))]

        {:node new-node}))
