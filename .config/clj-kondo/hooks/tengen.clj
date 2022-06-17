(ns hooks.tengen
  (:require [clj-kondo.hooks-api :as api]))

(defn def-cmptfn
  [{:keys [:node]}]
  (let [[var-name-node & more] (rest (:children node))

        _ (when-not (api/token-node? var-name-node)
            (throw (ex-info "Missing function name!" {})))

        [docstring-node
         params-node & body] (if (api/string-node? (first more))
                               more
                               (cons nil more))

        _ (when-not (api/vector-node? params-node)
            (throw (ex-info "Missing function parameters!" {})))

        let-node (let [body-map (->> body
                                     (partition 2)
                                     (map (fn [[k v]] [(api/sexpr k) v]))
                                     (into {}))
                       _ (when-not (== (count body) (* (count body-map) 2))
                           (throw (ex-info "Missing key or value argument!" {})))
                       _ (when-not (every? keyword? (keys body-map))
                           (throw (ex-info "Arguments must be key-value pairs!" {})))

                       bindings (api/vector-node
                                  (concat 
                                    ;; bind “magic bindings” before others
                                    (mapv api/token-node
                                          ['this-mounting? nil
                                           'this-cmpt nil])
                                    (:children (:let-mount body-map))
                                    (:children (:let-render body-map))))
                       exprs (remove nil? 
                                     ;; to prevent “unused bindings” warning
                                     [(api/vector-node
                                        [(api/token-node 'this-mounting?)
                                         (api/token-node 'this-cmpt)])
                                      (:render body-map)
                                      (:post-render body-map)
                                      (:unmount body-map)])]
                   (api/list-node
                     (list* (api/token-node 'let)
                            bindings
                            exprs)))

        fn-node (api/list-node
                  (list (api/token-node 'fn)
                        params-node
                        let-node))

        new-node (api/list-node
                   (remove nil?
                           (list (api/token-node 'def)
                                 var-name-node
                                 docstring-node
                                 fn-node)))]
    {:node new-node}))

