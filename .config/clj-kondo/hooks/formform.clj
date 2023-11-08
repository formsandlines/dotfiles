(ns hooks.formform
  (:require [clj-kondo.hooks-api :as api]))

(defn defoperator
  [{:keys [node]}]
  (if (= (api/sexpr node) '(defoperator k args interpretation nil))
    {:node node}
    (let [[key-node args-node intpr-node] (rest (:children node))

          _ (when-not (or (api/keyword-node? key-node)
                          (api/token-node? key-node))
              (throw (ex-info "Missing operator keyword!" {})))

          _ (when-not (api/vector-node? args-node)
              (throw (ex-info "Missing argument vector!" {})))

          new-node (api/list-node
                     (list (api/token-node 'defmethod)
                           (api/token-node 'formform.expr.symexpr/interpret-op)
                           key-node
                           args-node
                           intpr-node))]

      {:node new-node})))
