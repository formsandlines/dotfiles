#!/usr/bin/env bb

;; Credits to @borkdude (“Show Me Your REPL #1”):
;; https://youtu.be/AYKIR1oh62Y?t=247
(require '[clojure.repl])

(defmacro random-doc []
  (let [sym (-> (ns-publics 'clojure.core) keys rand-nth)]
    (if (:doc (meta (resolve sym)))
      `(clojure.repl/doc ~sym)
      `(random-doc))))

(random-doc)
