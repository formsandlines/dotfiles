;;; -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("eab4154bd3b8ec5cf90a5a2a14545ba3dc5dd7cf99fcdce013d10475c08ea56d" "7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98" default))
 '(ignored-local-variable-values
   '((eval progn
	   (make-variable-buffer-local 'cider-jack-in-nrepl-middlewares)
	   (add-to-list 'cider-jack-in-nrepl-middlewares "shadow.cljs.devtools.server.nrepl/middleware"))
     (elisp-lint-indent-specs
      (if-let* . 2)
      (when-let* . 1)
      (let* . defun)
      (nrepl-dbind-response . 2)
      (cider-save-marker . 1)
      (cider-propertize-region . 1)
      (cider-map-repls . 1)
      (cider--jack-in . 1)
      (cider--make-result-overlay . 1)
      (insert-label . defun)
      (insert-align-label . defun)
      (insert-rect . defun)
      (cl-defun . 2)
      (with-parsed-tramp-file-name . 2)
      (thread-first . 0)
      (thread-last . 0)
      (transient-define-prefix . defmacro)
      (transient-define-suffix . defmacro))
     (checkdoc-package-keywords-flag)
     (eval font-lock-add-keywords nil
	   `((,(concat "("
		       (regexp-opt
			'("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
			t)
		       "\\_>")
	      1 'font-lock-variable-name-face)))))
 '(package-selected-packages
   '(gnuplot corfu orderless vertico treesit-auto markdown-mode ajsc ajrepl janet-ts-mode racket-mode geiser-chicken cider smartparens flycheck-rjan flycheck-janet marginalia popper yasnippet diff-hl meow hydra which-key separedit symex flycheck-color-mode-line vc-use-package beacon org-transclusion eldoc lispyville evil-commentary magit clj-refactor diminish company flycheck-clj-kondo flycheck lua-mode haskell-mode rainbow-mode org-appear evil-surround gruvbox-theme evil))
 '(package-vc-selected-packages
   '((ajsc :vc-backend Git :url "https://github.com/sogaiu/a-janet-spork-client")
     (ajrepl :vc-backend Git :url "https://github.com/sogaiu/ajrepl")
     (janet-ts-mode :vc-backend Git :url "https://github.com/sogaiu/janet-ts-mode")
     (flycheck-rjan :vc-backend Git :url "https://github.com/sogaiu/flycheck-rjan")
     (flycheck-janet :vc-backend Git :url "https://github.com/sogaiu/flycheck-janet")
     (vc-use-package :vc-backend Git :url "https://github.com/slotThe/vc-use-package")
     (org-excalidraw :vc-backend Git :url "https://github.com/ifeitao/org-excalidraw"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
