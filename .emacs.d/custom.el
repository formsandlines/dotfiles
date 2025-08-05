;;; -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7613ef56a3aebbec29618a689e47876a72023bbd1b8393efc51c38f5ed3f33d1" "c7a926ad0e1ca4272c90fce2e1ffa7760494083356f6bb6d72481b879afce1f2" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "84b3c4fa1bbccd01a173839b7eebc226105fafd6b108f8400995eb79c67c9adf" "aa688776604bbddbaba9e0c0d77e8eb5f88d94308f223d1962b6e6b902add6a0" "0c5d7ffa7cdf8b889342d2cd38f6ddd2cc716561048feca89b17fda56677d6b8" "7776ba149258df15039b1f0aba4b180d95069b2589bc7d6570a833f05fdf7b6d" "b216e9b72dc8c2b702e4fcfd3c0af2d73c87eba46fd4db824ddb50863447d6a9" "0664443859604a53d2257701f034459edf8eab3cc2be50c7d8ae36740fe35578" "355e3439089e3b37bb143afc0a60ce091533fe467db2ab0f2ae34d13be7a47c5" "ccff17f0cb616e239e2de4bd78f0b2e8f1f49291aa43c50845e250203be27a95" "ed1b7b4db911724b2767d4b6ad240f5f238a6c07e98fff8823debcfb2f7d820a" "159a29ab0ec5ba4e2811eddd9756aa779b23467723dcbdd223929fbf2dde8954" "841b6a0350ae5029d6410d27cc036b9f35d3bf657de1c08af0b7cbe3974d19ac" "263e3a9286c7ab0c4f57f5d537033c8a5943e69d142e747723181ab9b12a5855" "694dbeb8f98dddfb603a2fe0c04101f3fe457ee49bf90a6a581271e7f9c580c8" "5efa59da0b446dd939749e86fdf414ef2b666f80243999633d9e2e4fd22fd37c" "df1ed4aa97d838117dbda6b2d84b70af924b0380486c380afb961ded8a41c386" "9f27d5afd6d78b40bf1290c10722818e0b90f141fc3758d3c2d284ccb565de15" "ffdf8617d6e0f1264e5879d3ac919d0f1d8c91d38f2c769e4fa633ddbab248bf" "c20358be3b98db42aeea2b437d8e683177a96015b692e90e6b7a78642624b939" "6ccb6eb66c70661934a94f395d755a84f3306732271c55d41a501757e4c39fcb" "317754d03bb6d85b5a598480e1bbee211335bbf496d441af4992bbf1e777579e" "546862540e7b7d758a64b328bf3ceec7ae98dd87d80551496b45485ec26e05e5" "28d91e89883df5dd87c7da27f6a15e8e41bb92a0c1341eaa9f397ed67b10b21d" "82f1e895a3fb1f4b99efc81e9d732c850f55653689e9492b4eb1be292b4826c3" "9d01a8af1bdd5c79b136dc5eb23b90d53675c3f4cb938dc15c4d8bc98d2bb86e" "4343cbc036f09361b2912119c63573433df725f599bfbdc16fb97f1e4847a08b" "3eef8f2dfc976ac0781b54aa80d12866e211f2443b03c09232678ae4176d73cf" "871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8" "eab4154bd3b8ec5cf90a5a2a14545ba3dc5dd7cf99fcdce013d10475c08ea56d" "7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98" default))
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
 '(org-agenda-files '("~/.dotfiles/.emacs.d/init.org"))
 '(package-selected-packages
   '(denote package-utils adaptive-wrap elpher clay racket-mode fontaine babashka breadcrumb vterm-toggle vterm modus-themes sublime-themes ef-themes lsp-ui lsp-mode meow-tree-sitter rotate css emacs-obsidian-excalidraw citar-org-roam citar-embark embark nerd-icons citar citeproc gnuplot-mode org-roam-ui emacsql-sqlite-builtin rainbow-mode emacsql-sqlite emacsql org-roam gnuplot corfu orderless treesit-auto markdown-mode ajsc ajrepl janet-ts-mode geiser-chicken cider smartparens flycheck-rjan flycheck-janet marginalia popper diff-hl meow hydra which-key separedit flycheck-color-mode-line vc-use-package beacon org-transclusion eldoc lispyville evil-commentary magit clj-refactor diminish company flycheck-clj-kondo flycheck lua-mode haskell-mode org-appear evil-surround gruvbox-theme evil))
 '(package-vc-selected-packages
   '((vc-use-package :vc-backend Git :url "https://github.com/slotThe/vc-use-package")
     (ajrepl :vc-backend Git :url "https://github.com/sogaiu/ajrepl")
     (janet-ts-mode :vc-backend Git :url "https://github.com/sogaiu/janet-ts-mode")
     (emacs-obsidian-excalidraw :vc-backend Git :url "https://github.com/hsingko/emacs-obsidian-excalidraw")
     (ajsc :vc-backend Git :url "https://github.com/sogaiu/a-janet-spork-client")
     (flycheck-rjan :vc-backend Git :url "https://github.com/sogaiu/flycheck-rjan")
     (flycheck-janet :vc-backend Git :url "https://github.com/sogaiu/flycheck-janet")
     (org-excalidraw :vc-backend Git :url "https://github.com/ifeitao/org-excalidraw")))
 '(safe-local-variable-values
   '((ph/clerk-watch-paths "src")
     (ph/clerk-watch-paths quote
			   ("src")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
