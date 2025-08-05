(ns hooks.formform
  (:require [clj-kondo.hooks-api :as api]
            [clojure.string :as string]))

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

(defn kebab->camel
  "Converts a kebab-case string to camelCase.
   Example: 'hello-world' -> 'helloWorld'"
  ([kebab-str capitalize?]
   (if (or (nil? kebab-str) (empty? kebab-str))
     ""
     (let [parts (clojure.string/split kebab-str #"-")]
       (if capitalize?
         (apply str (map clojure.string/capitalize parts))
         (apply str (first parts) 
                (map clojure.string/capitalize (rest parts)))))))
  ([kebab-str] (kebab->camel kebab-str false)))

(defn make-defx-linter
  [rec-prefix protocol-sym]
  (fn [{:keys [node]}]
    (let [[key-node fields-node docstr-node? & r] (rest (:children node))
          docstr-node (when (api/string-node? docstr-node?)
                        docstr-node?)
          method-nodes (if docstr-node
                         r
                         (cons docstr-node? r))

          _ (when-not (api/keyword-node? key-node)
              (throw (ex-info "Missing identifier keyword!" {})))

          _ (when-not (api/vector-node? fields-node)
              (throw (ex-info "Missing fields vector!" {})))

          rec-sym (symbol (str rec-prefix "-" (-> key-node
                                                  api/sexpr
                                                  name
                                                  (kebab->camel true))))

          new-node (api/list-node
                    (apply list
                           (api/token-node 'defrecord)
                           (api/token-node rec-sym)
                           fields-node
                           (api/token-node protocol-sym)
                           method-nodes))]
      {:node new-node})))

(def defini (make-defx-linter "Ini" 'formform.emul.interfaces.Ini))
(def defumwelt (make-defx-linter "Umwelt" 'formform.emul.interfaces.Umwelt))
(def defrule (make-defx-linter "Rule" 'formform.emul.interfaces.Rule))
(def defspecies (make-defx-linter "Species" 'formform.emul.interfaces.Specifier))
