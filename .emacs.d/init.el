;;; -*- lexical-binding: t; -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq package-native-compile t)

(setq package-archives 
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")
	("elpa-devel" . "https://elpa.gnu.org/devel/")))

;;; Bootstrap use-package:
;; (package-initialize)                ; ? still needed
;; (setq use-package-always-pin "nongnu") ; ? is this important
;; (setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package)) ; ? needed

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

(defvar use-package-selected-packages '(use-package)
  "Packages pulled in by use-package.")
(defun use-package-autoremove ()
  "Autoremove packages not used by use-package."
  (interactive)
  (let ((package-selected-packages use-package-selected-packages))
    (package-autoremove)))

(eval-and-compile
  (define-advice use-package-handler/:ensure (:around (fn name-symbol keyword args rest state) select)
    (let ((items (funcall fn name-symbol keyword args rest state)))
      (dolist (ensure args items)
        (let ((package
	       (or (and (eq ensure t) (use-package-as-symbol name-symbol))
                   ensure)))
          (when package
            (when (consp package)
	      (setq package (car package)))
            (push `(add-to-list 'use-package-selected-packages ',package) items))))))
  (define-advice use-package-handler/:vc (:around (fn name-symbol keyword args rest state) select)
    (let ((items (funcall fn name-symbol keyword args rest state)))
      (dolist (ensure args items)
        (let ((package
	       (or (and (eq ensure t) (use-package-as-symbol name-symbol))
                   ensure)))
          (when package
            (when (consp package)
	      (setq package (car package)))
            (push `(add-to-list 'use-package-selected-packages ',package) items)))))) 
  (define-advice use-package-handler/:quelpa (:around (fn name-symbol keyword args rest state) select)
    (let ((package (pcase (car args)
                     ((pred symbolp) (car args))
                     ((pred listp) (car (car args))))))
      (cons `(add-to-list 'use-package-selected-packages ',package)
            (funcall fn name-symbol keyword args rest state)))))

(require 'org-tempo)
;; (require 'eldoc)

(setq user-full-name "Peter Hofmann"
      user-mail-address "peter.hofmann@formsandlines.eu")

;;; There are some warnings I really can’t do anything about and they
;;; are annoying, so keep quiet
(setq warning-minimum-level :emergency)

;;; Display relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-widen t)
(setq display-line-numbers-width-start t)
(setq display-line-numbers-width 3)

;;; Display column number in modeline
(column-number-mode 1)

;;; Highlight current line
(global-hl-line-mode 1)

;;; Disable tab insertion for indentation
(setq indent-tabs-mode nil)

;;; Set max char count for automatic line breaks
(setq-default fill-column 80) ; ! FIXME: still 70?
;;; Display vertical line at char limit
(global-display-fill-column-indicator-mode 1)
(setq display-fill-column-indicator-character 9474)

(setq repeat-mode t)
(setq repeat-exit-key "<return>")

(setq sentence-end-double-space nil)

(setq next-screen-context-lines 4) ;; was 2

(setq blink-cursor-mode nil)

;;; Remember and get back to recently opened files
(recentf-mode 1)

;;; Remember and restore the last cursor location of opened files
(save-place-mode 1)
;;; Save and restore the state of Emacs from one session to another
(if (display-graphic-p)
    (desktop-save-mode 1)
  ;;; prevent saving/restoring the desktop in terminal mode
  (desktop-save-mode 0))

;;; Don’t pop up UI dialogs when prompting
(setq use-dialog-box nil)

;;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
;;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;;; Disabled by default, but I find them useful and not confusing:
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; No backup files (foo.txt~):
(setq make-backup-files nil)

;;; Follow symlinks for version control:
(setq vc-follow-symlinks t)

;;; Record state of window configuration to undo/redo:
;;; - C-c <left> to undo window configuration
;;; - C-c <right> to redo
(winner-mode 1)

;;; Enable commands to move windows:
(windmove-mode 1)

(setq initial-major-mode 'lisp-interaction-mode)

;;;###autoload
(defun ph/buf-move-up ()
  "Swap the current buffer and the buffer above the split.
If there is no split, ie now window above the current one, an
error is signaled."
  ;;  "Switches between the current buffer, and the buffer above the
  ;;  split, if possible."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'up))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No window above this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun ph/buf-move-down ()
  "Swap the current buffer and the buffer under the split.
If there is no split, ie now window under the current one, an
error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'down))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (or (null other-win) 
            (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
        (error "No window under this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun ph/buf-move-left ()
  "Swap the current buffer and the buffer on the left of the split.
If there is no split, ie now window on the left of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'left))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No left split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun ph/buf-move-right ()
  "Swap the current buffer and the buffer on the right of the split.
If there is no split, ie now window on the right of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'right))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No right split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;; I like to scroll line-by-line
(defun ph/scroll-one-line-up () (interactive) (scroll-up 1))
(defun ph/scroll-one-line-down () (interactive) (scroll-down 1))

;;; For some reason these conflict with meow-kill:
;; (global-set-key (kbd "C-j") 'ph/scroll-one-line-up)
;; (global-set-key (kbd "C-k") 'ph/scroll-one-line-down)

(defun ph/window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun ph/scroll-up-half ()
  (interactive)
  (scroll-up (ph/window-half-height)))

(defun ph/scroll-down-half ()         
  (interactive)                    
  (scroll-down (ph/window-half-height)))

;; (global-set-key (kbd "C-j") 'ph/scroll-up-half)
;; (global-set-key (kbd "C-k") 'ph/scroll-down-half)

;; (add-hook 'org-mode-hook
;; 	  (lambda ()
;; 	    (define-key org-mode-map (kbd "C-j") 'ph/scroll-one-line-up)))
;; (add-hook 'org-mode-hook
;; 	  (lambda ()
;; 	    (define-key org-mode-map (kbd "C-k") 'ph/scroll-one-line-down)))

;; (defun my-org/insert-heading-above ()
;;   "Insert a heading above the current one and activate Evil insert mode."
;;   (interactive)
;;   (if (org-at-heading-p)
;;       (evil-first-non-blank)
;;     (org-up-element))
;;   (org-insert-heading)
;;   (evil-insert-state))

;; (evil-define-key 'normal org-mode-map (kbd "C-S-<return>")
;;  'my-org/insert-heading-above)


(defun ph/describe-keybinding (keybinding)
  (interactive "sEnter keybinding: ")
  (describe-key (kbd keybinding)))

;;; Use if a keybinding in minibuffer is not accessible from the system:
;; (setq enable-recursive-minibuffers t)  ; <-- set to nil after use!
;; (define-key minibuffer-mode-map (kbd "C-M-k") 'describe-keybinding)

;; (global-set-key (kbd "C-c C-r") 'recentf-open-files)
;; (global-set-key (kbd "C-c r") 'recentf-open)

;;; because M-x is hard to reach on my keyboard:
(keymap-global-set "C-\\" #'execute-extended-command)
(keymap-global-set "C-|" #'execute-extended-command-for-buffer)
(keymap-global-set "M-+" #'toggle-input-method) ;; replacement for C-\

(global-set-key [remap list-buffers] 'ibuffer)

;;; because C-M-d activates the dictionary in MacOS (hard to change):
(keymap-global-set "C-M-'" #'down-list) 

(defun ph/visit-config ()
  "Opens my init.org file."
  (interactive)
  (find-file (locate-user-emacs-file "init.org")))

(defun ph/reload-config ()
  (interactive)
  (org-babel-load-file (locate-user-emacs-file "init.org")))

(defun ph/visit-theme ()
  "Opens my theme file."
  (interactive)
  (find-file (locate-user-emacs-file "themes/pmacs-theme.el")))

(keymap-global-set "C-c s s" #'ph/visit-config)
(keymap-global-set "C-c s r" #'ph/reload-config)

(keymap-global-set "C-c s t" #'ph/visit-theme)

(keymap-global-set "C-c b n" (lambda ()
			     (interactive)
			     (find-file "~/Documents/emacs-notes.org")))

(keymap-global-set "C-c b b" #'scratch-buffer)

(setq inhibit-startup-message t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; e.g. to show the `λ' symbol when typing `lambda'
(global-prettify-symbols-mode 1)

;;; macOS titlebar decoration
;;; - see https://xenodium.com/my-emacs-eye-candy/
;;; - doesn’t seem to work (no such variables?)
;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))

;;; better mouse wheel scrolling
;;; https://stackoverflow.com/a/26053341
(setq mouse-wheel-scroll-amount '(0.07))
;;; https://stackoverflow.com/a/445881
(setq mouse-wheel-progressive-speed nil)

;; Remove “pause” when changing directions
(setq isearch-repeat-on-direction-change t)
;; Do not wrap isearch when reaching the end of a buffer
(setq isearch-wrap-pause nil)
;; Max length of search ring (default: 16)
;; - cycle using `M-p' and `M-n'
(setq search-ring-max 4)
;; Do not allow scrolling commands to exit search
(setq isearch-allow-scroll t)

;; (setq isearch-forward-thing-at-point '(region url symbol sexp))

(keymap-set Info-mode-map "M-{" #'Info-search-backward)
(keymap-set Info-mode-map "M-}" #'Info-search-next)

;; `C-M-d' triggers dictionary on MacOS, so `isearch-del-char` needs to be
;; rebound. Also, it is incredibly annoying that `DEL' also returns to the
;; previous search item, so:
(keymap-set isearch-mode-map "DEL" #'isearch-del-char)
(keymap-set isearch-mode-map "C-p" #'isearch-delete-char)
;; `M-s M-<' and `M-s M->' are just too annoying to type, so:
(keymap-set isearch-mode-map "C->" #'isearch-end-of-buffer)
(keymap-set isearch-mode-map "C-<" #'isearch-beginning-of-buffer)

(use-package diminish
  :ensure t)

;;; 
(use-package vc-use-package
  :ensure t)

(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :config
  ;; (setq which-key-side-window-location 'bottom)
  ;; (setq which-key-sort-order #'which-key-key-order-alpha)
  ;; (setq which-key-sort-uppercase-first nil)
  ;; (setq which-key-add-column-padding 1)
  ;; (setq which-key-max-display-columns nil)
  ;; (setq which-key-min-display-lines 6)
  ;; (setq which-key-side-window-slot -10)
  ;; (setq which-key-side-window-max-height 0.25)
  ;; (setq which-key-idle-delay 0.8)
  ;; (setq which-key-max-description-length 25)
  ;; (setq which-key-allow-imprecise-window-fit t)
  ;; (setq which-key-separator " → ")
  )

(use-package hydra
  :ensure t
  :diminish
  :config
  (defhydra hydra-zoom ()
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")
    
    ("SPC" nil "cancel"))

  (defhydra hydra-view ()
    "view"
    ;; command names strangely reversed
    ("j" ph/scroll-one-line-up "down")
    ("k" ph/scroll-one-line-down "up")
    ("v" ph/scroll-up-half "half pg down")
    ("V" ph/scroll-down-half "half pg up")
    ("d" scroll-up-command "page down")
    ("u" scroll-down-command "page up")
    
    ("J" end-of-buffer "buffer end")
    ("K" beginning-of-buffer "buffer start")

    ("c" recenter "view center")
    ("t" (lambda () (interactive) (recenter 0)) "view top")
    ("b" (lambda () (interactive) (recenter -1)) "view bottom") ;; doesn’t work
    
    ("?" (hydra-set-property 'hydra-view :verbosity 1) :exit nil)
    ("SPC" nil "cancel"))
  ;; wrapper to hide minibuffer help since it makes movement bouncy
  (defun hydra-view-silent ()
    (interactive)
    (hydra-set-property 'hydra-view :verbosity 0)
    (hydra-view/body))

  (defhydra hydra-org (:color pink)
    "Org Mode movement"
    ;; command names strangely reversed
    ("k" org-previous-visible-heading "prev heading")
    ("K" org-backward-heading-same-level "backward heading")
    ("J" org-forward-heading-same-level "forward heading")
    ("j" org-next-visible-heading "next heading")
    
    ("u" outline-up-heading "up heading")
    ("o" org-cycle "cycle headings")
    ("c" org-shifttab "cycle all")
    ("a" org-fold-show-all "show all")
    ("s" org-kill-note-or-show-branches "unfold subtree")

    ("H" org-previous-item "prev item")
    ("h" org-backward-element "backward element")
    ("l" org-forward-element "forward element")
    ("L" org-next-item "next item")

    ("f" org-narrow-to-subtree "focus subtree")
    ("d" widen "defocus subtree")

    ("?" (hydra-set-property 'hydra-org :verbosity 1) :exit nil)
    ("SPC" nil "cancel"))
  ;; wrapper to hide minibuffer help since it makes movement bouncy
  (defun hydra-org-silent ()
    (interactive)
    (hydra-set-property 'hydra-org :verbosity 0)
    (hydra-org/body))

  (defhydra hydra-window ()
    "window"
    ;; Window buffer
    ("b" switch-to-buffer)
    ("f" find-file)
    ;; Window commands
    ("c" delete-window)        ;; C-x 0
    ("d" delete-other-windows :color blue) ;; C-x 1
    ("s" split-window-below)   ;; C-x 2
    ("v" split-window-right)   ;; C-x 3
    ("w" other-window)	  ;; C-x o
    ("=" balance-windows)
    ;; Move to windows
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)
    ;; Move windows
    ("H" ph/buf-move-left)
    ("J" ph/buf-move-down)
    ("K" ph/buf-move-up)
    ("L" ph/buf-move-right)
    ;; Resize windows
    ("C-h" shrink-window-horizontally)
    ("C-l" enlarge-window-horizontally)
    ("C-k" enlarge-window)
    ("C-j" shrink-window)

    ("SPC" nil "cancel"))
  )

;; Let 'a' in 'normal' mode behave like 'a' in Vi:
;; - https://github.com/meow-edit/meow/discussions/497#discussioncomment-6713192
;; - unused for now, since it somehow doesn’t work with my clj-refactor
;;   hook to disable 'cljr-slash'
(defun ph/meow-append ()
  "Move to the end of selection, switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (if (not (region-active-p))
        (when (and (not (use-region-p))
                   (< (point) (point-max)))
          (forward-char 1))
      (meow--direction-forward)
      (meow--cancel-selection))
    (meow--switch-state 'insert)))

(defun ph/meow-line-append ()
  "Appends to the end of the current line."
  (interactive)
  (meow-end-of-thing (meow-line 1))
  (meow-append))

(defun ph/meow-line-insert ()
  "Inserts at the beginning (indentation) of the current line."
  (interactive)
  (meow-beginning-of-thing (meow-line 1))
  (meow-insert))

(defun ph/meow-join-with ()
  "Joins current line with line below."
  (interactive)
  (meow-join -1)
  (meow-kill))

(defun ph/meow-split-at ()
  "Splits current line at point."
  (interactive)
  (electric-newline-and-maybe-indent))

;; (defun ph/meow-search-backwards ()
;;   "Searches backwards."
;;   (interactive)
;;   (meow-search (negative-argument -1)))

(defun ph/meow-insert-exit ()
  "Switch to previous state."
  (interactive)
  (cond
   ((meow-keypad-mode-p)
    (meow--exit-keypad-state))
   ((and (meow-insert-mode-p)
         (eq meow--beacon-defining-kbd-macro 'quick))
    (setq meow--beacon-defining-kbd-macro nil)
    (meow-beacon-insert-exit))
   ((meow-insert-mode-p)
    (when overwrite-mode
      (overwrite-mode -1))
    (meow--switch-state 'normal))))

(defun ph/meow-eval-buffer (&optional buffer undef-all)
  "Conditionally evokes eval-buffer commands specific to the active
major mode or the general command if none applies."
  (interactive (list (current-buffer) (equal current-prefix-arg '(4))))
  (cond
   ((eq major-mode 'clojure-mode)
    (cider-eval-buffer buffer nil undef-all))
   ((eq major-mode 'janet-ts-mode)
    (ajrepl-send-buffer))
   (t (eval-buffer buffer))))

(defun ph/meow-eval-region (start end)
  "Conditionally evokes eval-region commands specific to the active
major mode or the general command if none applies."
  (interactive "r")
  (cond
   ((eq major-mode 'janet-ts-mode)
    (ajrepl-send-region start end))
   ((t (eval-region start end)))))

(defun ph/meow-eval-dwim (&optional start end)
  "Calls `ph/meow-eval-region' if a region is active, otherwise
calls `meow-eval-last-exp'."
  (interactive "r")
  (if (region-active-p)
      (ph/meow-eval-region start end)
    (meow-eval-last-exp)))

(defun ph/meow-change-save ()
  "Calls `meow-change-save' if a region is active, otherwise calls
`meow-change'."
  (interactive)
  (if (region-active-p)
      (meow-change-save)
    (meow-change)))

(defun ph/meow-search-reverse ()
  "Reverses the search direction from `meow-search' (like `-n')."
  (interactive)
  (meow-search -1))

;; prefix /
(defconst ph/meow-prefix-slash
  (list
   ;;; COMMENTS
   '("//" . meow-comment)		; nf -> nc -> /g
   
   ;;; MACROS
   '("/M" . meow-start-kmacro-or-insert-counter)
   '("/m" . meow-start-kmacro)
   '("/n" . meow-end-or-call-kmacro)
   
   ;;; REFERENCES
   '("/f" . xref-find-definitions)
   '("/F" . xref-go-back)
   '("/r" . xref-find-references)
   '("/a" . xref-find-apropos)
   '("/j" . eldoc)
   '("/R" . eglot-rename)

   ;;; SYSTEM CLIPBOARD
   '("/cc" . meow-clipboard-save)
   '("/cx" . meow-clipboard-kill)
   '("/cv" . meow-clipboard-yank)

   ;;; WORDS
   '("/lu" . upcase-dwim)
   '("/ll" . downcase-dwim)
   '("/lc" . capitalize-dwim)
   
   ;;; NUMBERS
   '("/+" . ph/increment-number-at-point)
   '("/-" . ph/decrement-number-at-point)

   ;;; WRAPPING
   '("/ww" . ph/wrap-with-char)
   '("/wc" . ph/change-wrapped-char)
   '("/wd" . ph/remove-surrounding)

   ;;; INDENTATION
   '("/ TAB" . org-indent-item)  ; org-mode replaces <tab>
   '("/ <backtab>" . org-outdent-item)  ; org-mode replaces <backtab>

   ;;; GOTO 
   '("/ge" . end-of-buffer)
   '("/G" . end-of-buffer)
   '("/gj" . end-of-buffer)
   '("/gk" . beginning-of-buffer)
   '("/gg" . beginning-of-buffer)
   '("/gl" . meow-goto-line)
   '("/gc" . move-to-column)
   '("/gp" . goto-char)

   ;;; SEARCH
   '("/v" . meow-visit)	        ; / -> ? -> / -> ns -> /s -> /v
   '("/s" . query-replace-regexp)

   ;;; EVAL
   '("/e" . ph/meow-eval-dwim)  ; just C-x C-e or ph/meow-eval-region
   '("/b" . ph/meow-eval-buffer)
   ;; '("/r" . ph/meow-eval-region)
   '("/d" . "C-M-x")  ; = eval-defun & friends
   
   ;; '("/j" . ph/meow-join-with)
   ;; '("/k" . ph/meow-split-at)
   
   ;; '("/c" . kill-ring-save)
   ;; '("/p" . yank)
   ))

;; prefix ; -> \
(defconst ph/meow-prefix-backslash
  (list
   ;;; BUFFER
   '("\\\\" . switch-to-buffer)
   '("\\|" . ibuffer)
   '("\\q" . meow-quit)
   '("\\w" . save-buffer)
   '("\\W" . save-some-buffers)
   '("\\r" . meow-query-replace-regexp)
   
   ;;; PROJECT
   '("\\f" . project-find-file)
   '("\\b" . project-switch-to-buffer)
   '("\\p" . project-switch-project)
   '("\\d" . project-find-dir)
   '("\\k" . project-kill-buffers)
   ;; '("\\g" . project-find-regexp)
   ;; '("\\r" . project-query-replace-regexp)
   '("\\/" . project-shell)
   '("\\g" . magit-status)))

(defconst ph/meow-common
  (list
   ;; '("M-c" . meow-clipboard-save) ;; was kill-ring-save
   ;; '("M-x" . meow-clipboard-kill)
   ;; '("M-v" . meow-clipboard-yank) ;; was yank
   ))

(defconst ph/meow-normal
  (list
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '(";" . meow-reverse)		; ' -> ;


   '("k" . meow-prev)
   '("j" . meow-next)
   '("h" . meow-left)
   '("l" . meow-right)

   '("v" . ph/scroll-up-half)
   '("V" . ph/scroll-down-half)

   '("n" . meow-search)		; y -> / -> ` -> / -> n
   '("N" . ph/meow-search-reverse)


   '("K" . meow-prev-expand)
   '("J" . meow-next-expand)
   '("H" . meow-left-expand)
   '("L" . meow-right-expand)

   '("u" . meow-back-word)
   '("U" . meow-back-symbol)
   '("o" . meow-next-word)
   '("O" . meow-next-symbol)

   '("w" . meow-mark-word)		; a -> w
   '("W" . meow-mark-symbol)		; A -> W
   '("e" . meow-line)			; s -> e
   '("E" . ph/meow-line-append)
   '("q" . meow-block)		; w -> q
   '("Q" . meow-to-block)
   '("s" . meow-join)			; q -> a -> h -> a
   '("S" . ph/meow-line-insert)	; H -> A
   '("g" . meow-grab)			; g -> h (see undo) -> g
   '("G" . meow-pop-grab)		; G -> H -> G
   '("m" . meow-swap-grab)
   '("M" . meow-sync-grab)
   '("~" . meow-cancel-selection)	; p -> [ -> t -> h -> H -> ~
   '("`" . meow-pop-selection)	; P -> { -> T -> H -> h -> `
   '("t" . meow-transpose-sexp)
   '("T" . transpose-lines)

   '("F" . meow-till)			; x -> t -> F
   '("f" . meow-find)			; z -> f

   '("[" . meow-beginning-of-thing)	; , -> [
   '("]" . meow-end-of-thing)		; . -> ]
   '("," . meow-inner-of-thing)	; < -> ,
   '("." . meow-bounds-of-thing)	; > -> .

   '("{" . backward-paragraph)
   '("}" . forward-paragraph)


   '("d" . meow-kill)
   '("D" . meow-kill-whole-line)
   '("S-<backspace>" . ph/kill-whole-line-move-prev)
   '("r" . ph/meow-change-save)	; f -> c -> r
   '("R" . meow-replace)
   '("x" . meow-delete)		; t -> x
   '("c" . meow-save)			; c -> y -> t -> ` -> c
   '("p" . meow-yank)			; v -> p
   '("P" . meow-yank-pop)		; V -> P

   '("i" . meow-insert)		; e -> s -> a -> i
   '("I" . meow-open-above)		; S -> R -> S -> I
   '("a" . meow-append)		; Vi-style append -> normal append
   '("A" . meow-open-below)		; E -> S -> A

   '("y" . undo-only)			; h -> g -> z -> b
   '("Y" . undo-redo)			; H -> G -> Z -> B

   '("z" . open-line)			; b -> z
   '("Z" . split-line)		; B -> Z

   '("=" . meow-indent)
   '("X" . ph/meow-join-with)

   ;; '("[" . indent-rigidly-left-to-tab-stop)
   ;; '("]" . indent-rigidly-right-to-tab-stop)


   '("-" . negative-argument)
   '("'" . repeat)			; dot-mode-execute
   '("\"" . meow-end-or-call-kmacro)    
   '("C-]" . meow-paren-mode) ;; ? -> C-]
   '("C-;" . meow-symex-mode)
   '("C-:" . meow-table-mode)
   '("§" . cider-doc) ;; ! replace with generic selector

   ;; ignore escape
   '("<escape>" . ignore)))

(use-package meow
  :ensure t
  :demand t
  :after (hydra clj-refactor symex)
  :config
  (meow-global-mode 1)
  (meow-setup-indicator)

  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (setq meow-keypad-leader-dispatch "C-c")

  ;;; Prevent 'C-[' from triggering 'ESC' prefix-keymaps:
  ;;; see:
  ;;; https://github.com/meow-edit/meow/discussions/255#discussioncomment-2862406
  (define-key input-decode-map [?\C-\[] [C-\[])
  (define-key global-map [C-\[] [?\C-\M-§])
  
  ;;; Prevent 'C-i' and 'C-I' from acting as 'TAB' and 'S-TAB':
  ;; (define-key input-decode-map [?\C-i] [C-i])
  ;; (define-key input-decode-map [?\C-\S-i] [C-S-i])

  (add-hook 'meow-paren-mode-hook
	    (lambda () (keymap-unset clj-refactor-map "/")))
  (add-hook 'meow-symex-mode-hook
	    (lambda () (keymap-unset clj-refactor-map "/")))
  (add-hook 'meow-normal-mode-hook
	    (lambda () (keymap-unset clj-refactor-map "/")))
  (add-hook 'meow-insert-mode-hook
	    (lambda () (keymap-set clj-refactor-map "/" #'cljr-slash)))
  ;;
  )

(use-package meow
  :config
  (meow-thing-register 'elisp-quoted
		       '(regexp "`" "`\\|'")
		       '(regexp "`" "`\\|'"))

  (meow-thing-register 'quoted
		       '(regexp "‘" "‘\\|’")
		       '(regexp "‘" "‘\\|’"))

  (meow-thing-register 'angle
		       '(pair ("<") (">"))
		       '(pair ("<") (">")))

  (setq meow-char-thing-table
	'((?f . round)
	  (?d . square)
	  (?s . curly)
	  (?a . angle)
	  (?n . quoted)
	  (?m . elisp-quoted)
	  (?r . string)
	  (?w . paragraph)
	  (?e . line)
	  (?q . buffer)))
  ;;
  )

(use-package meow
  :config

  (setq meow-paren-keymap (make-keymap))
  (meow-define-state paren
    "meow state for structural editing"
    :lighter " [P]"
    :keymap meow-paren-keymap)

  ;; meow-define-state creates the variable
  (setq meow-cursor-type-paren 'hollow)

  (apply 'meow-define-keys 'paren ph/meow-prefix-slash)  
  (apply 'meow-define-keys 'paren ph/meow-prefix-backslash)  
  (apply 'meow-define-keys 'paren ph/meow-common)

  (meow-define-keys 'paren
    ;; general meow keys:
    '("0" . meow-expand-0)
    '("1" . meow-expand-1)
    '("2" . meow-expand-2)
    '("3" . meow-expand-3)
    '("4" . meow-expand-4)
    '("5" . meow-expand-5)
    '("6" . meow-expand-6)
    '("7" . meow-expand-7)
    '("8" . meow-expand-8)
    '("9" . meow-expand-9)

    '("SPC" . meow-keypad)
    '("C-M-§" . meow-normal-mode)
    '("C-;" . meow-symex-mode)

    '("p" . meow-yank)
    '("P" . meow-yank-pop)
    '("y" . undo-only)
    '("Y" . undo-redo)
    ;; '("c" . meow-save)
    
    '("v" . ph/scroll-up-half)
    '("V" . ph/scroll-down-half)

    '("-" . negative-argument)
    '("'" . repeat)
    '("~" . meow-cancel-selection)
    ;; '("`" . meow-pop-selection) ;; doesn’t work with smartparens
    '(";" . meow-reverse)
    
    '("i" . meow-insert)
    '("I" . meow-open-above)
    '("a" . meow-append)
    '("A" . meow-open-below)
    
    '("r" . ph/meow-change-save)
    '("R" . meow-replace)
    
    ;; '("d" . meow-kill)

    '("n" . meow-search)
    '("F" . meow-till)
    '("f" . meow-find)
    
    '("§" . cider-doc) ;; ! replace with generic selector

    ;; '("=" . meow-indent)

    ;; paren specific:

    '("<backspace>" . sp-backward-unwrap-sexp)
    '("<escape>" . ignore)
    
    '("s" . sp-beginning-of-sexp)
    '("e" . sp-end-of-sexp)
    '("$" . ph/sp-innermost)
    '("%" . ph/sp-outermost)
    
    '("`" . exchange-point-and-mark)

    '("h" . sp-backward-sexp)
    '("H" . sp-backward-symbol)
    '("l" . sp-forward-sexp)
    '("L" . sp-forward-symbol)
    
    '("k" . sp-down-sexp)
    '("K" . sp-backward-down-sexp)
    '("j" . sp-up-sexp)
    '("J" . sp-backward-up-sexp)
    
    '("d" . ph/sp-kill-sexp-or-region)
    '("D" . sp-kill-hybrid-sexp)
    '("c" . ph/sp-copy-sexp-or-region)
    ;; '("r" . sp-change-inner)
    
    '("w" . sp-mark-sexp)
    '("W" . sp-rewrap-sexp)

    '("m" . sp-raise-sexp)
    '("M" . sp-splice-sexp)
    
    '("t" . sp-transpose-sexp)
    '("T" . sp-convolute-sexp)
    '("x" . sp-split-sexp)
    '("X" . sp-join-sexp)
    
    '("O" . sp-next-sexp)
    '("o" . sp-select-next-thing)
    '("U" . sp-previous-sexp)
    '("u" . sp-select-previous-thing)

    '("." . sp-forward-slurp-sexp)
    '(">" . sp-forward-barf-sexp)
    '("<" . sp-backward-barf-sexp)
    '("," . sp-backward-slurp-sexp)

    '("q" . sp-unwrap-sexp)
    '("Q" . sp-backward-unwrap-sexp)

    ;; smartparens automatically wraps if bracket is typed, so no binding
    ;; '("[" . sp-select-previous-thing)
    ;; '("{" . sp-select-previous-thing-exchange)
    ;; '("]" . sp-select-next-thing)
    ;; '("}" . sp-select-next-thing-exchange)

    '("=" . sp-indent-defun)

    '("//" . sp-comment)
    '("/?" . meow-comment)
    '("/d" . (lambda () (interactive) (sp-wrap-with-pair "[")))
    '("/s" . (lambda () (interactive) (sp-wrap-with-pair "{")))
    '("/f" . (lambda () (interactive) (sp-wrap-with-pair "("))))

  ;;
  )

(use-package meow
  :config

  (setq meow-symex-keymap (make-keymap))
  (meow-define-state symex
    "meow state for structural editing with symex"
    :lighter " [S]"
    :keymap meow-symex-keymap
    (if meow-symex-mode
	(run-hooks 'meow-symex-mode-enable-hook)))

  (add-hook 'meow-symex-mode-enable-hook
	    (lambda ()
              (symex-select-nearest-in-line)
              (symex--adjust-point)
              ;; (symex-initialize)
              ))

  (add-hook 'meow-normal-mode-hook
	    (lambda ()
	      (when (and meow-normal-mode
			 (symex--overlay-active-p))
		(symex--delete-overlay))))

  (add-hook 'meow-insert-mode-hook
	    (lambda ()
	      (when (and meow-insert-mode
			 (symex--overlay-active-p))
		(symex--delete-overlay))))

  (setq meow-cursor-type-symex 'hollow)

  (apply 'meow-define-keys 'symex ph/meow-prefix-slash)  
  (apply 'meow-define-keys 'symex ph/meow-prefix-backslash)  
  (apply 'meow-define-keys 'symex ph/meow-common)

  (meow-define-keys 'symex
    ;; GENERAL MEOW KEYS
    '("0" . meow-expand-0)
    '("1" . meow-expand-1)
    '("2" . meow-expand-2)
    '("3" . meow-expand-3)
    '("4" . meow-expand-4)
    '("5" . meow-expand-5)
    '("6" . meow-expand-6)
    '("7" . meow-expand-7)
    '("8" . meow-expand-8)
    '("9" . meow-expand-9)

    '("SPC" . meow-keypad)
    '("C-M-§" . meow-normal-mode)

    ;; '("p" . meow-yank) ;; -> symex
    ;; '("P" . meow-yank-pop) ;; -> symex
    '("y" . undo-only)
    '("Y" . undo-redo)
    ;; '("c" . meow-save)
    
    '("v" . ph/scroll-up-half)
    '("V" . ph/scroll-down-half)

    ;; '("-" . negative-argument) ;; -> symex-splice
    '("'" . repeat)
    ;; '("~" . meow-cancel-selection) ;; -> useless here
    ;; '("`" . meow-pop-selection) ;; -> useless here
    ;; '(";" . meow-reverse) ;; -> useless here

    ;; symex has its own insert state (?)
    ;; '("i" . meow-insert)
    ;; '("I" . meow-open-above)
    ;; '("a" . meow-append)
    ;; '("A" . meow-open-below)
    ;; '("r" . ph/meow-change-save)
    ;; '("R" . meow-replace)
    
    ;; '("n" . meow-search)
    ;; '("F" . meow-till)
    ;; '("f" . meow-find)
    
    '("§" . cider-doc) ;; ! replace with generic selector

    ;; '("=" . meow-indent)


    ;; SYMEX SPECIFIC

    ;; '("<backspace>" . sp-backward-unwrap-sexp)
    ;; '("<escape>" . ignore)
    
    '("(" . symex-create-round)
    '("[" . symex-create-square)
    '("{" . symex-create-curly)
    ;; '("<" . symex-create-angled)
    
    '("h" . symex-go-backward)
    '("k" . symex-go-up)
    '("j" . symex-go-down)
    '("l" . symex-go-forward)

    '("gj" . symex-next-visual-line)
    '("gk" . symex-previous-visual-line)
    '("L" . symex-traverse-forward) ;; f -> o -> L
    '("H" . symex-traverse-backward) ;; b -> u -> H
    ;; '("C-f" . symex-traverse-forward-more)
    ;; '("C-b" . symex-traverse-backward-more)
    ;; '("O" . symex-traverse-forward-skip) ;; F -> O
    ;; '("U" . symex-traverse-backward-skip) ;; B -> U
    '("u" . symex-leap-backward) ;; C-h -> u
    '("o" . symex-leap-forward) ;; C-l -> o
    '("U" . symex-soar-backward) ;; C-M-h -> U
    '("O" . symex-soar-forward) ;; C-M-l -> O
    '("K" . symex-climb-branch) ;; C-k -> K
    '("J" . symex-descend-branch) ;; C-j -> J
    
    '("c" . symex-yank) ;; y -> c
    ;; '("C" . symex-yank-remaining) ;; Y -> C (doesn’t work)
    '("p" . ph/symex-paste-after) ;; p -> P -> p
    '("P" . ph/symex-paste-before) ;; P -> p -> P
    '("d" . symex-delete) ;; x -> d
    ;; '("D" . symex-delete-backwards) ;; X -> D
    ;; '("D" . symex-delete-remaining) ;; D -> ? (doesn’t work)
    '("r" . ph/symex-change) ;; c -> r
    ;; '("R" . ph/symex-change-remaining) ;; C -> R (doesn’t work)
    '("R" . ph/symex-replace) ;; same as change? s -> R
    '("q" . symex-change-delimiter) ;; S -> /W -> q
    '("D" . symex-clear) ;; C-- -> D
    
    '("T" . symex-shift-backward) ;; H -> T
    '("t" . symex-shift-forward) ;; L -> t
    ;; '("M-H" . symex-shift-backward-most)
    ;; '("M-L" . symex-shift-forward-most)
    '("N" . paredit-raise-sexp) ;; K -> m -> M

    '("," . symex-capture-backward) ;; C-( / C-S-h -> ,
    '("<" . symex-emit-backward) ;; C-{ / C-S-j -> <
    '(">" . symex-emit-forward) ;; C-} / C-S-k -> >
    '("." . symex-capture-forward) ;; C-) / C-S-l -> .
    '("z" . symex-swallow)
    '("Z" . symex-swallow-tail)
    
    '("/e" . ph/symex-evaluate) ;; e -> /e
    '("/E" . symex-evaluate-remaining) ;; E -> /E
    ;; '("C-M-e" . symex-evaluate-pretty)
    '("/d" . symex-evaluate-definition) ;; d -> C-d -> /d
    ;; '("M-e" . symex-eval-recursive)
    ;; '("T" . symex-evaluate-thunk)
    '(":" . eval-expression)
    
    ;; '("t" . symex-switch-to-scratch-buffer)
    ;; '("M" . symex-switch-to-messages-buffer)
    ;; '("C-r" . symex-repl) ;; r -> C-r
    ;; '("C-R" . symex-run) ;; R -> C-R
    
    '("|" . symex-split)
    '("&" . symex-join)
    '("-" . symex-splice) ;; M ?
    '(")" . symex-wrap-round)
    '("]" . symex-wrap-square)
    '("}" . symex-wrap-curly)
    ;; '(">" . symex-wrap-angled)
    '("`" . symex-cycle-quote)
    '("~" . symex-cycle-unquote)
    ;; '("`" . symex-add-quoting-level)
    ;; '("C-`" . symex-remove-quoting-level)
    
    '("E" . ph/symex-open-line-after) ;; o -> b -> n -> E
    '("S" . ph/symex-open-line-before) ;; O -> B -> N -> S
    '("C-{" . symex-insert-newline) ;; n -> C-{
    '("C-}" . symex-append-newline) ;; C-S-o -> C-}
    '("X" . symex-join-lines) ;; J -> X
    ;; '("M-J" . symex-collapse)
    '("M-<" . symex-collapse)
    '("M->" . symex-unfurl)
    ;; '("C-M-<" . symex-collapse-remaining)
    ;; '("C-M->" . symex-unfurl-remaining)
    '("x" . symex-join-lines-backwards) ;; N -> x
    
    '("s" . symex-goto-first) ;; 0 / M-h -> s
    '("e" . symex-goto-last) ;; $ / M-l -> e
    '("n" . symex-goto-lowest) ;; M-j -> S -> n
    '("m" . symex-goto-highest) ;; M-k -> E -> m
    
    '("=" . symex-tidy)
    '("<tab>" . symex-tidy)
    ;; '("C-=" . symex-tidy-remaining)
    ;; '("C-<tab>" . symex-tidy-remaining)
    ;; '("M-=" . symex-tidy-proper)
    ;; '("M-<tab>" . symex-tidy-proper)
    
    '("A" . ph/symex-append-after)
    '("a" . ph/symex-insert-at-end)
    '("i" . ph/symex-insert-at-beginning)
    '("I" . ph/symex-insert-before)
    '("w" . ph/symex-wrap)
    '("W" . ph/symex-wrap-and-append)
    
    ;; '("g" . evil-jump-to-tag) ;; -> prefix command
    ;; '("G" . evil-jump-backward) ;; -> prefix command
    
    '(";" . symex-comment)
    ;; '("M-;" . symex-comment-remaining) ;; -> doesn’t work
    ;; '("C-;" . symex-eval-print)
    
    ;; canonical action
    ;; '("s-;" . symex-evaluate)
    
    ;; configuration
    ;; '("H-h" . symex--toggle-highlight)
    
    ;; '("C-e" . symex--scroll-down)
    ;; '("C-y" . symex--scroll-up)
    
    ;; standard exits
    ;; '("?" . symex-describe)
    ;; '("<return>" . symex-enter-lower)
    ;; '("<escape>" . symex-escape-higher)
    ;; '("C-g" . symex-escape-higher)


    )
  
  ;;
  )

(defun ph/meow-org-table-field-insert (&optional n)
  "Inserts text from insert mode before the contents of table field."
  (interactive "p")
  ;; if on the 1. char in field, org moves to previous field
  ;; so we need to go from next char
  (meow-right)
  (org-table-beginning-of-field n)
  (meow-insert))

(defun ph/meow-org-table-field-append (&optional n)
  "Appends text from insert mode to the contents of table field."
  (interactive "p")
  (org-table-end-of-field n)
  (meow-append))

(defun ph/meow-org-table-kill ()
  "If no region is active, kills org-table row, otherwise uses `meow-kill'."
  (interactive)
  (if (region-active-p)
      (meow-kill)
      (org-table-kill-row)))

(defun ph/org-table-cut-region ()
  "Same as the original, but does not move the cursor."
  (interactive)
  (let ((beg (point))) ;; `save-excursion' doesn’t seem to work here
    (call-interactively 'org-table-cut-region)
    (goto-char beg)))

(defun ph/org-table-copy-region ()
  "Same as the original, but does not move the cursor."
  (interactive)
  (save-excursion
    (call-interactively 'org-table-copy-region)))

(defun ph/meow-org-table-field-replace ()
  "Copies table field content before replacing it with the insertion."
  (interactive)
  (ph/org-table-cut-region)
  (meow-insert))

(use-package meow
  :config

  (setq meow-table-keymap (make-keymap))
  (meow-define-state table
    "meow state for editing tables in Org mode"
    :lighter " [T]"
    :keymap meow-table-keymap)

  (setq meow-cursor-type-table 'hollow)

  (apply 'meow-define-keys 'table ph/meow-prefix-slash)  
  (apply 'meow-define-keys 'table ph/meow-prefix-backslash)  
  (apply 'meow-define-keys 'table ph/meow-common)
  (apply 'meow-define-keys 'table ph/meow-normal)
  
  (meow-define-keys 'table
    '("SPC" . meow-keypad)
    '("C-M-§" . meow-normal-mode)

    '("K" . org-table-move-row-up)
    '("J" . org-table-move-row-down)
    '("H" . org-table-move-column-left)
    '("L" . org-table-move-column-right)

    '("U" . org-shifttab)
    '("O" . org-cycle)

    ;; '("S" . org-table-beginning-of-field) ;; U/O move to beginning
    '("E" . org-table-end-of-field)

    '("I" . ph/meow-org-table-field-insert)
    '("A" . ph/meow-org-table-field-append)

    '("s" . org-table-insert-row) ;; above
    '("d" . ph/meow-org-table-kill) ;; current
    '("S" . org-table-insert-column) ;; to the left
    '("D" . org-table-delete-column) ;; current

    ;; '("v" . org-table-copy-down) ;; already on S-RET
    
    '(":" . ph/meow-org-table-field-replace)

    ;; '("D" . org-table-blank-field)
    '("X" . ph/org-table-cut-region)
    '("C" . ph/org-table-copy-region)
    '("P" . org-table-paste-rectangle)
    
    '("q" . org-table-wrap-region)


    '("?" . org-table-field-info)

    )

  ;;
  )

(use-package meow
  :config

  ;;; INSERT STATE ;;;

  (meow-define-keys 'insert
    '("H-SPC" . meow-keypad)
    '("C-M-§" . meow-insert-exit)
    '("S-<backspace>" . ph/kill-whole-line-move-prev)
    '("C-;" . meow-symex-mode)
    '("C-]" . meow-paren-mode) ;; temporary workaround
    '("C-:" . meow-table-mode)
    '("C-y" . meow-yank))
  
  (apply 'meow-define-keys 'insert ph/meow-common)

  ;;; MOTION STATE OVERWRITES ;;;

  (meow-motion-overwrite-define-key
   '("k" . meow-prev)
   '("j" . meow-next)
   '("h" . meow-left)
   '("l" . meow-right)
   '("v" . ph/scroll-up-half)
   '("V" . ph/scroll-down-half)
   '("{" . backward-paragraph)
   '("}" . forward-paragraph)
   '("<escape>" . ignore))
  
  (apply 'meow-motion-overwrite-define-key ph/meow-prefix-backslash)
  (apply 'meow-motion-overwrite-define-key ph/meow-common)

  ;;; BEACON STATE ;;;
  
  (meow-define-keys 'beacon
    '("/m" . meow-beacon-start)
    '("/e" . meow-beacon-apply-kmacro)  
    '("<escape>" . ignore))

  ;;; NORMAL STATE ;;;

  (apply 'meow-define-keys 'normal ph/meow-prefix-slash)
  (apply 'meow-define-keys 'normal ph/meow-prefix-backslash)
  (apply 'meow-define-keys 'normal ph/meow-common)
  
  ;; The actual normal-mode bindings:
  (apply 'meow-define-keys 'normal ph/meow-normal)
  ;;
  )

(use-package meow
  :config

  (meow-leader-define-key
   ;; Hydras
   '("w" . hydra-window/body)
   '("v" . hydra-view-silent)
   '("z" . hydra-zoom/body)
   '("o" . hydra-org-silent)

   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)

   ;; SPC j/k/l/h will run the original command in MOTION state.
   ;; '("j" . "H-j")
   ;; '("k" . "H-k")
   ;; '("l" . "H-l")
   ;; '("h" . "H-h")
   ;; '("v" . "H-v")
   ;; '("V" . "H-V")
   ;; '("{" . "H-{")
   ;; '("}" . "H-}")
   )
  ;;
  )

(use-package magit
  :ensure t)

(use-package diff-hl
  :after (magit dired)
  :ensure t
  :diminish
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(use-package org
  :config
  (setq org-id-link-to-org-use-id 'use-existing)

  ;;; Hide emphasis marker characters
  (setq org-use-speed-commands t)

  ;;; Enable org-indent-mode on startup
  (setq org-startup-indented t)

  (setq org-hide-emphasis-markers t)
  ;;; Show entities as UTF8 characters
  (setq org-pretty-entities t)

  ;; (setq org-startup-with-latex-preview t)

  ;; (setq org-edit-src-content-indentation 0)
  (setq org-src-preserve-indentation t)

  (setq org-directory "~/org")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  
  (setq org-src-window-setup 'current-window)

  ;; Automatically insert line breaks at char limit
  (add-hook 'org-mode-hook #'turn-on-auto-fill)

  ;; Emacs overwrites the bindings C-<tab> and C-c <tab> on Mac with
  ;; `mac-next-tab-or-toggle-tab-bar` and `mac-previous-tab-or-toggle-tab-bar`,
  ;; but I don’t really use the tab bar.
  (keymap-set org-mode-map "C-c <tab>" #'org-fold-show-children)
  (keymap-set org-mode-map "C-<tab>" #'org-kill-note-or-show-branches)
  (keymap-set org-mode-map "C-M-<tab>" #'org-fold-show-all)

  ;; (add-to-list 'display-buffer-alist
  ;; 	       '("^\\*Org Src" display-buffer-at-bottom
  ;; 		 (window-height . 0.5)))
)

(defun ph/org-insert-child-heading ()
  "Inserts a child heading from the current heading node."
  (interactive)
  (org-insert-heading-respect-content)
  (org-do-demote))

(defun ph/meow-org-insert-heading ()
  (interactive)
  (org-insert-heading-respect-content)
  (meow-insert))

(defun ph/meow-org-insert-child-heading ()
  (interactive)
  (ph/org-insert-child-heading)
  (meow-insert))


;;; TODO maybe extract a more general function to make these DRYer:

(defun ph/meow-org-add-list-item ()
  (interactive)
  (org-end-of-line)
  (org-insert-item)
  (meow-append))

(defun ph/meow-org-add-above-list-item ()
  (interactive)
  (org-beginning-of-line)
  (org-insert-item)
  (meow-insert))

(defun ph/meow-org-add-lower-list-item ()
  (interactive)
  (org-end-of-line)
  (org-insert-item)
  (org-indent-item)  
  (meow-append))

(defun ph/meow-org-add-todo-item ()
  (interactive)
  (org-end-of-line)
  (org-insert-item t)
  (meow-append))

(defun ph/meow-org-add-above-todo-item ()
  (interactive)
  (org-beginning-of-line)
  (org-insert-item t)
  (meow-insert))

(defun ph/meow-org-add-lower-todo-item ()
  (interactive)
  (org-end-of-line)
  (org-insert-item t)
  (org-indent-item)  
  (meow-append))

(use-package org
  :config
  ;; Global bindings as recommended by the org manual:
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)

  ;; Local bindings:
  (keymap-set org-mode-map "C-c j" #'ph/meow-org-insert-heading)
  (keymap-set org-mode-map "C-c J" #'ph/meow-org-insert-child-heading)
  (keymap-set org-mode-map "C-c M-j" #'ph/org-insert-child-heading)
  
  (keymap-set org-mode-map "C-c i" #'ph/meow-org-add-list-item)
  (keymap-set org-mode-map "C-c I" #'ph/meow-org-add-above-list-item)
  (keymap-set org-mode-map "C-c M-i" #'ph/meow-org-add-lower-list-item)
  (keymap-set org-mode-map "C-c x" #'ph/meow-org-add-todo-item)
  (keymap-set org-mode-map "C-c X" #'ph/meow-org-add-above-todo-item)
  (keymap-set org-mode-map "C-c M-x" #'ph/meow-org-add-lower-todo-item)

  ;; meow somehow messes up the `C-c SPC' mapping, so I have to rebind it:
  (keymap-set org-mode-map "C-c d" #'org-table-blank-field))

(use-package org
  :config
  ;; Enable following links in TheBrain
  (org-link-set-parameters
   "brain"
   :follow (lambda (path) (ph/macos-open (concat "brain:" path)))
   ;; :store #'org-brain-store-link
   ))

(defun ph/org-id-store-create ()
  (interactive)
  (org-id-get-create)
  (call-interactively 'org-store-link))

(defun ph/org-capture-fw-ref ()
  (interactive)
  (org-capture nil "f"))

(keymap-global-set "C-c f" #'ph/org-capture-fw-ref)

(use-package org
  :config

  (setq org-capture-templates
	'(("t" "Task" entry (file+headline "" "Tasks")
	   "* TODO %?\n  %u\n  %a")
	  ("f" "FW Reference" entry (file "~/org/fw-refs.org")
	   "* %?\n:PROPERTIES:\n:AUTHOR: %n\n:CREATED: %U\n:LAST_EDITED: nil\n:CAPTURED: %T\n:ORIGIN_CAPTURED: %a\n:END:\n"
	   :before-finalize ph/org-id-store-create))))

(use-package org
  :config
  (defun current-fill-column ()
    "Return the fill-column to use for this line.
Subtracts right margin and org indentation level from fill-column"
    (let ((indent-level (if (bound-and-true-p org-indent-mode)
			    (* org-indent-indentation-per-level
			       (org-current-level))
			  0))
	  (margin (or (get-text-property (point) 'right-margin) 0)))
      (- fill-column indent-level margin))))

(use-package org-appear
  :ensure t
  :diminish
  :after org
  :hook org-mode
  :config
  (setq org-appear-autoentities t)
  (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t))

;;; Somehow the fringe in target buffer does not show up.
;;; Workaround, source:
;;; - https://github.com/nobiot/org-transclusion/issues/201#issue-1868665106
;;; - UPDATE: seems to mess with indentation -> unusable
;; (defun org-transclusion-content-insert-add-overlay (beg end)
;;   "Add fringe after transclusion."
;;   (overlay-put (text-clone-make-overlay beg end (current-buffer))
;; 	       'line-prefix
;; 	       (org-transclusion-propertize-transclusion))
;;   (overlay-put (text-clone-make-overlay beg end (current-buffer))
;; 	       'wrap-prefix
;; 	       (org-transclusion-propertize-transclusion)))

(use-package org-transclusion
  :pin elpa-devel
  :ensure t
  :after org
  :config
  (add-to-list 'org-transclusion-extensions 'org-transclusion-indent-mode)
  (require 'org-transclusion-indent-mode)

  (keymap-set org-mode-map "C-c t a" #'org-transclusion-add)
  (keymap-set org-mode-map "C-c t m" #'org-transclusion-mode)

  (keymap-set org-transclusion-map "C-c t e" #'org-transclusion-live-sync-start)
  (keymap-set org-transclusion-map "C-c t g" #'org-transclusion-refresh)
  (keymap-set org-transclusion-map "C-c t d" #'org-transclusion-remove)
  (keymap-set org-transclusion-map "C-c t C-d" #'org-transclusion-detach)
  (keymap-set org-transclusion-map "C-c t P" #'org-transclusion-promote-subtree)
  (keymap-set org-transclusion-map "C-c t D" #'org-transclusion-demote-subtree)
  (keymap-set org-transclusion-map "C-c t o" #'org-transclusion-open-source)
  (keymap-set org-transclusion-map "C-c t O" #'org-transclusion-move-to-source)
  
  (keymap-set org-transclusion-live-sync-map "C-c t C-c C-c"
	      #'org-transclusion-live-sync-exit)
  (keymap-set org-transclusion-live-sync-map "C-c t C-y"
	      #'org-transclusion-live-sync-paste)

  ;; (add-hook 'org-transclusion-after-add-functions
  ;; 	    #'org-transclusion-content-insert-add-overlay)
  )

(use-package yasnippet
  :ensure t
  :diminish
  :config
  (yas-global-mode 1))

(use-package rainbow-mode
  :ensure t
  :diminish
  :hook org-mode prog-mode)

(use-package beacon
  :ensure t
  :diminish
  :config
  (beacon-mode 1))

(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle)
         ("M-§"   . popper-cycle) ;; was handle-switch-frame
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
	  "\\*cider-doc\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)
  :config
  )

(use-package savehist
  :diminish
  :init
  (setq history-length 25)
  (savehist-mode 1))

(use-package vertico
  :ensure t
  :after savehist
  :init
  (vertico-mode 1))

;; As suggested by: https://github.com/minad/vertico
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
		  (replace-regexp-in-string
		   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		   crm-separator)
		  (car args))
	  (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;;
  )

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  ;;
  )

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :ensure t
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode)

  :config
  (setq marginalia-field-width 80) ; 43 in Doom
  
  (add-hook 'icomplete-minibuffer-setup-hook
	    (lambda () (setq truncate-lines t)))
  
  (add-hook 'completion-list-mode-hook
	    (lambda () (setq truncate-lines t)))

  ;;; Disable Marginalia in *completions* buffer for non-one-column formats
  ;; -> doesn’t work
  ;; - https://github.com/minad/marginalia/issues/129
  ;; (defun disable-marginalia ()
  ;;   (when (and (equal t fido-mode)
  ;; 	       (not (eq completions-format 'one-column)))
  ;;     (setq-local marginalia-annotator-registry nil)))
  ;; (add-hook 'completion-list-mode-hook #'disable-marginalia)
  )

(use-package company
  :ensure t
  :defer t
  :diminish
  :init (add-hook 'after-init-hook 'global-company-mode))

;;; ? needed
;; (use-package company-box
;;   :after company
;;   :diminish
;;   :hook (company-mode . company-box-mode))

(use-package flycheck
  :ensure t
  :defer t
  :after eldoc
  :diminish
  :init (global-flycheck-mode)
  ;; :hook ((flycheck-mode . ph/flycheck-prefer-eldoc))
  :config
  ;;; emacs-lisp-checkdoc checker is annoying in emacs-lisp-mode,
  ;;; so I disable it there:
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (setq-local flycheck-disabled-checkers
			  '(emacs-lisp-checkdoc)))))

(use-package flycheck-color-mode-line
  :ensure t
  :diminish
  :after flycheck
  :hook flycheck-mode)

(use-package flycheck-clj-kondo
  :ensure t
  :after (flycheck clojure-mode)
  :diminish
  :config
  ;; ? how to disable elisp undefined warnings
  )

(use-package flycheck-janet
  :ensure t
  :after (flycheck janet-ts-mode)
  :diminish
  :vc (:fetcher github :repo sogaiu/flycheck-janet))

(use-package flycheck-rjan
  :ensure t
  :after (flycheck janet-ts-mode flycheck-janet)
  :diminish
  :vc (:fetcher github :repo sogaiu/flycheck-rjan)
  :config
  (flycheck-add-next-checker 'janet-rjan 'janet-janet))

(use-package smartparens
  :ensure t
  :after janet-ts-mode
  :init (require 'smartparens-config)
  :config
  (smartparens-global-mode t) ;; These options can be t or nil.
  (show-smartparens-global-mode t)
  (setq sp-show-pair-from-inside t)
  
  (sp-with-modes 'janet-ts-mode
    (sp-local-pair "'" nil :actions nil))

  (sp-with-modes 'org-mode
    (sp-local-pair "*" nil :actions nil)
    ;; (sp-local-pair "_" nil :actions nil)
    ;; (sp-local-pair "/" nil :actions nil)
    ;; (sp-local-pair "~" nil :actions nil)
    ;; (sp-local-pair "=" nil :actions nil)
    (sp-local-pair "'" nil :actions nil))

  ;; (sp-pair "`" "`")
  ;; (sp-pair "$" "$")
  )

(defun ph/sp-outermost ()
  "Moves outside to top-level sexp."
  (interactive)
  (let ((res (sp-up-sexp)))
    (while res
      (setq res (sp-up-sexp)))))

;; (defun ph/sp-outermost ()
;;   "Moves outside to top-level sexp."
;;   (interactive)
;;   (sp-up-sexp '(-4)))

(defun ph/sp-innermost ()
  "Moves to the leftmost-innermost sexp."
  (interactive)
  (sp-down-sexp '(4)))

(defun ph/sp-kill-sexp-or-region (beg end &optional arg dont-kill)
  "Selects the appropriate kill function from Smartparens by
chacking if a region is active or not."
  (interactive "rP")
  (if (region-active-p)
      (sp-kill-region beg end)
    (sp-kill-sexp arg dont-kill)))

(defun ph/sp-copy-sexp-or-region (beg end &optional arg)
  "Selects the appropriate kill/copy function from Smartparens by
chacking if a region is active or not."
  (interactive "rP")
  (if (region-active-p)
      (meow-save)
    (sp-kill-sexp arg t)))

(defun ph/symex-ts-insert-at-end ()
  "Insert at end of symex."
  (interactive)
  (when (symex-ts-get-current-node)
    (goto-char (tsc-node-end-position (symex-ts-get-current-node)))
    ;; (evil-insert-state)
    (meow-insert)))

(defun ph/symex-lisp--insert-at-end ()
  "Insert at end of symex."
  (interactive)
  (if (or (lispy-left-p)
          (symex-string-p))
      (progn (forward-sexp)
             (backward-char))
    (forward-sexp))
  ;; (symex-enter-lowest)
  (meow-insert))

(defun ph/symex-insert-at-end ()
  "Insert at end of symex."
  (interactive)
  (if (symex-tree-sitter-p)
      (ph/symex-ts-insert-at-end)
    (ph/symex-lisp--insert-at-end)))


(defun ph/symex-ts-insert-at-beginning ()
  "Insert at beginning of symex."
  (interactive)
  (when (symex-ts-get-current-node)
    (goto-char (tsc-node-start-position (symex-ts-get-current-node)))
    ;; (evil-insert-state)
    (meow-insert)))

(defun ph/symex-lisp--insert-at-beginning ()
  "Insert at beginning of symex."
  (interactive)
  (when (or (lispy-left-p)
            (symex-string-p))
    (forward-char))
  ;; (symex-enter-lowest)
  (meow-insert))

(defun ph/symex-insert-at-beginning ()
  "Insert at beginning of symex."
  (interactive)
  (if (symex-tree-sitter-p)
      (ph/symex-ts-insert-at-beginning)
    (ph/symex-lisp--insert-at-beginning)))


(defun ph/symex-ts-append-after ()
  "Append after symex (instead of vim's default of line)."
  (interactive)
  (when (symex-ts-get-current-node)
    (goto-char (tsc-node-end-position (symex-ts-get-current-node)))
    (insert " ")
    ;; (evil-insert-state)
    (meow-insert)))

(defun ph/symex-lisp--append-after ()
  "Append after symex (instead of vim's default of line)."
  (interactive)
  (forward-sexp)  ; selected symexes will have the cursor on the starting paren
  (insert " ")
  ;; (symex-enter-lowest)
  (meow-insert))

(defun ph/symex-append-after ()
  "Append after symex (instead of vim's default of line)."
  (interactive)
  (if (symex-tree-sitter-p)
      (ph/symex-ts-append-after)
    (ph/symex-lisp--append-after)))


(defun ph/symex-ts-insert-before ()
  "Insert before symex (instead of vim's default at the start of line)."
  (interactive)
  (when (symex-ts-get-current-node)
    (goto-char (tsc-node-start-position (symex-ts-get-current-node)))
    (insert " ")
    (backward-char)
    ;; (evil-insert-state)
    (meow-insert)))

(defun ph/symex-lisp--insert-before ()
  "Insert before symex (instead of vim's default at the start of line)."
  (interactive)
  (insert " ")
  (backward-char)
  ;; (symex-enter-lowest)
  (meow-insert))

(defun ph/symex-insert-before ()
  "Insert before symex (instead of vim's default at the start of line)."
  (interactive)
  (if (symex-tree-sitter-p)
      (ph/symex-ts-insert-before)
    (ph/symex-lisp--insert-before)))


(defun ph/symex-wrap ()
  "Wrap with containing symex."
  (interactive)
  (symex-wrap-round)
  (ph/symex-insert-at-beginning))


(defun ph/symex-wrap-and-append ()
  "Wrap with containing symex and append."
  (interactive)
  (symex-wrap-round)
  (ph/symex-insert-at-end))


(defun ph/symex-ts-change-node-forward (&optional count)
  "Delete COUNT nodes forward from the current node and enter Insert state."
  (interactive "p")
  (save-excursion (symex-ts-delete-node-forward count t))
  ;; (evil-insert-state 1)
  (meow-insert))

(defun ph/symex-lisp--change (count)
  "Change COUNT symexes."
  (interactive "p")
  (let ((start (point))
        (end (symex--get-end-point count)))
    (kill-region start end))
  ;; (symex-enter-lowest)
  (meow-insert))

(defun ph/symex-change (count)
  "Change COUNT symexes."
  (interactive "p")
  (if (symex-tree-sitter-p)
      (ph/symex-ts-change-node-forward count)
    (ph/symex-lisp--change count)))


(defun ph/symex-change-remaining ()
  "Change remaining symexes at this level."
  (interactive)
  (let ((count (symex--remaining-length)))
    (ph/symex-change count)))


(defun ph/symex-ts-replace ()
  "Replace contents of symex."
  (when symex-ts--current-node
    (let* ((child-count (tsc-count-named-children symex-ts--current-node))

           ;; Get new position for insertion: if the node has children
           ;; then the start of the first child node, otherwise the
           ;; current point.
           (new-pos (if (> child-count 0)
                        (tsc-node-start-position (tsc-get-nth-named-child symex-ts--current-node 0))
                      (point))))

      (symex-ts-clear)
      (goto-char new-pos)
      ;; (evil-insert-state 1)
      (meow-insert))))

(defun ph/symex-replace ()
  "Replace contents of symex."
  (interactive)
  (if (symex-tree-sitter-p)
      (ph/symex-ts-replace)
    (progn (symex--clear)
           (when (or (symex-form-p) (symex-string-p))
             (forward-char))
           ;; (symex-enter-lowest)
	   (meow-insert))))


(defun ph/symex-ts-open-line-after ()
  "Open new line after symex."
  (interactive)
  (when (symex-ts-get-current-node)
    (goto-char (tsc-node-end-position (symex-ts-get-current-node)))
    (newline-and-indent)
    ;; (evil-insert-state)
    (meow-insert)))

(defun ph/symex-lisp--open-line-after ()
  "Open new line after symex."
  (interactive)
  (forward-sexp)
  (newline-and-indent)
  ;; (symex-enter-lowest)
  (meow-insert))

(defun ph/symex-open-line-after ()
  "Open new line after symex."
  (interactive)
  (if (symex-tree-sitter-p)
      (ph/symex-ts-open-line-after)
    (ph/symex-lisp--open-line-after)))


(defun ph/symex-ts-open-line-before ()
  "Open new line before symex."
  (interactive)
  (when (symex-ts-get-current-node)
    (goto-char (tsc-node-start-position (symex-ts-get-current-node)))
    (newline-and-indent)
    (evil-previous-line)
    (indent-according-to-mode)
    ;; (evil-append-line 1)
    (meow-insert))) ;; ! incorrect command mapping from append-line

(defun ph/symex-lisp--open-line-before ()
  "Open new line before symex."
  (interactive)
  (newline-and-indent)
  (evil-previous-line)
  (indent-according-to-mode)
  (evil-move-end-of-line)
  (unless (or (symex--current-line-empty-p)
              (save-excursion (backward-char)
                              (lispy-left-p)))
    (insert " "))
  ;; (symex-enter-lowest)
  (meow-insert))

(defun ph/symex-open-line-before ()
  "Open new line before symex."
  (interactive)
  (if (symex-tree-sitter-p)
      (ph/symex-ts-open-line-before)
    (ph/symex-lisp--open-line-before)))

(defun ph/symex-eval-janet ()
  "Eval last sexp."
  (interactive)
  (ajrepl-send-expression-at-point))

;; changed significantly (trimmed down)
(defun ph/symex--evaluate ()
  "Evaluate symex."
  (save-excursion
    (forward-sexp)
    (cond ((equal major-mode 'janet-ts-mode)
	   (ph/symex-eval-janet))
	  ((member major-mode symex-racket-modes)
	   (symex-eval-racket))
	  ((member major-mode symex-elisp-modes)
	   (symex-eval-elisp))
	  ((equal major-mode 'scheme-mode)
	   (symex-eval-scheme))
	  ((member major-mode symex-clojure-modes)
	   (symex-eval-clojure))
	  ((member major-mode symex-common-lisp-modes)
	   (symex-eval-common-lisp))
	  ((equal major-mode 'arc-mode)
	   (symex-eval-arc))
	  (t (error "Symex mode: Lisp flavor not recognized!")))))

(defun ph/symex-evaluate (count)
  "Evaluate COUNT symexes."
  (interactive "p")
  (save-excursion
    (let ((i 0)
          (movedp t))
      (while (or (not movedp)
                 (< i count))
        (ph/symex--evaluate)
        (symex--go-forward)
        (setq i (1+ i))))))

;; ! may need adjustment
(defun ph/symex-ts--paste (count direction)
  "Paste before or after symex, COUNT times, according to DIRECTION.

DIRECTION should be either the symbol `before' or `after'."
  (interactive)
  (when (symex-ts-get-current-node)
    (symex-ts--handle-tree-modification
     (let* ((node (symex-ts-get-current-node))
	    (start (tsc-node-start-position node))
	    (end (tsc-node-end-position node))
	    (indent-start (save-excursion (back-to-indentation) (point)))
	    (block-node (or (not (= (line-number-at-pos start)
				    (line-number-at-pos end)))
			    (and (= start indent-start)
				 (= end (line-end-position))))))
       (goto-char (if (eq direction 'before) start end))
       (dotimes (_ count)
	 (when (eq direction 'after) (insert (if block-node "\n" " ")))
	 ;; (yank)
	 (meow-yank)
	 (when (eq direction 'before) (insert (if block-node "\n" " "))
	       (indent-according-to-mode)))))))


(defun ph/symex-ts-paste-before (count)
  "Paste before symex, COUNT times."
  (interactive)
  (ph/symex-ts--paste count 'before))

(defun ph/symex-lisp--paste-before ()
  "Paste before symex."
  (interactive)
  (let ((extra-to-append
         (cond ((or (and (symex--point-at-indentation-p)
                         (not (bolp)))
                    (save-excursion (forward-sexp)
                                    (eolp)))
                "\n")
               (t " "))))
    (save-excursion
      ;; (save-excursion
      ;;   (evil-paste-before nil nil)
      ;;   (when evil-move-cursor-back
      ;;     (forward-char))
      ;;   (insert extra-to-append))
      (meow-yank) ;; ++
      (insert extra-to-append) ;; ++
      (symex--go-forward)
      ;; (symex-tidy)
      )
    (symex-tidy)))

(defun ph/symex-paste-before (count)
  "Paste before symex, COUNT times."
  (interactive "p")
  (setq this-command 'meow-yank) ;; 'evil-paste-before
  (if (symex-tree-sitter-p)
      (ph/symex-ts-paste-before count)
    (symex--with-undo-collapse
      (dotimes (_ count)
        (ph/symex-lisp--paste-before)))))


(defun ph/symex-ts-paste-after (count)
  "Paste after symex, COUNT times."
  (interactive)
  (ph/symex-ts--paste count 'after))

(defun ph/symex-lisp--paste-after ()
  "Paste after symex."
  (interactive)
  (let ((extra-to-prepend
         (cond ((or (and (symex--point-at-indentation-p)
                         (not (bolp)))
                    (save-excursion (forward-sexp)
                                    (eolp)))
                "\n")
               (t " "))))
    (save-excursion
      (forward-sexp)
      (insert extra-to-prepend)
      ;; (evil-paste-before nil nil)
      (meow-yank))
    (symex--go-forward)
    (symex-tidy)))

(defun ph/symex-paste-after (count)
  "Paste after symex, COUNT times."
  (interactive "p")
  (setq this-command 'meow-yank) ;; 'evil-paste-after
  (if (symex-tree-sitter-p)
      (ph/symex-ts-paste-after count)
    (symex--with-undo-collapse
      (dotimes (_ count)
        (ph/symex-lisp--paste-after)))))

(use-package symex
  :ensure t
  :after janet-ts-mode
  :config
  (symex-initialize)
  ;; (global-set-key (kbd "s-;") 'symex-mode-interface)

  ;; (setq symex-highlight-p nil)

  ;;; symex-mode has functions that react on tree-sitter-mode with different
  ;;; implementations, possibly breaking my meow integration, so I overwrite
  ;;; the predicate to check for that to prevent activation.
  (defvar ph/symex-use-tree-sitter nil)
  ;;; !! overwrite
  (defun symex-tree-sitter-p ()
    "Whether to use the tree sitter primitives."
    (and ph/symex-use-tree-sitter
	 tree-sitter-mode
	 ;; We use the Lisp primitives for Clojure
	 ;; even though Emacs 29 provides tree-sitter APIs
	 ;; for it, since the Lisp primitives in Symex are
	 ;; more mature than the Tree Sitter ones at the
	 ;; present time.
	 (not (member major-mode symex-clojure-modes))))

  )

(use-package treesit-auto
  :ensure t
  :after tree-sitter
  :diminish
  :init
  ;; Needs to be set before config of the package:
  (setq treesit-language-source-alist
	'((janet-simple
	   . ("https://github.com/sogaiu/tree-sitter-janet-simple"))))
  :config
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

  (use-package eldoc
    :ensure t
    :pin elpa
    :config
    ;; ? enabled by default
    ;; (global-eldoc-mode 1)
    
    ;; ? does this work
    ;; Source: https://github.com/joaotavora/eglot/discussions/1328#discussioncomment-7787866
 ;;    (defun eldoc-fancy (arg)
 ;;      "`eldoc' but uses the echo area by default and a prefix
 ;; will swap to a buffer."
 ;;      (interactive "P")
 ;;      (let ((eldoc-display-functions
 ;;             (if arg '(eldoc-display-in-buffer) '(eldoc-display-in-echo-area))))
 ;; 	(eldoc t)))


    (add-to-list 'display-buffer-alist
		 '("^\\*eldoc" display-buffer-at-bottom
		   (window-height . 4)))


    ;;; use `eldoc-doc-buffer'
    ;; (setq eldoc-echo-area-prefer-doc-buffer t)
  
    ;;; see https://github.com/joaotavora/eglot/discussions/734
    ;; (setq eldoc-echo-area-use-multiline-p nil)
    )

(use-package eglot
  ;; :hook ((eglot-managed-mode . ph/eglot-eldoc)) 
  :config
  (add-to-list 'eglot-server-programs
               '(janet-ts-mode . ("janet-lsp")))
  
  ;; (add-hook 'janet-ts-mode-hook 'eglot-ensure)

  ;;; Disable documentation-on-hover, which is still accessible with `M-x
  ;;; eldoc':
  ;;; https://github.com/joaotavora/eglot/discussions/691#discussioncomment-719373
  (add-hook 'eglot-managed-mode-hook
	    (lambda () (eldoc-mode -1)))

  ;; (setq eglot-strict-mode nil)

  ;; (add-to-list 'eglot-ignored-server-capabilities :hoverProvider)

  ;; (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider)
  ;; (add-to-list 'eglot-stay-out-of 'flymake)
  )

(use-package haskell-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

  (use-package cider
    :ensure t
    :config
    (require 'flycheck-clj-kondo)

    ;;; For better editing in camelCase (Java names):
    (add-hook 'cider-repl-mode-hook #'subword-mode)

    ;;; C-[ gets stuck because of Ciders ESC-key prefix, so let’s disable it:
  
    ;;; (define-key cider-mode-map (kbd "ESC") nil)
    (setq cider-preferred-build-tool 'clojure-cli)

    ;;; Use enrich-classpath for better Java lib completions/docs
    (setq cider-enrich-classpath t)  
  
    (setq cider-eval-spinner-type 'moon)
  
    (setq cider-repl-history-size 2000)

    ;;; Don't show cider help text in repl after jack-in
    (setq cider-repl-display-help-banner nil)

    ;;; Show error as overlay instead of the buffer (buffer is generated anyway in
    ;;; case it's needed)
    (setq cider-show-error-buffer 'except-in-repl)
    ;;; If we set `cider-show-error-buffer' to non-nil, don't focus error buffer
    ;;; when error is thrown
    (setq cider-auto-select-error-buffer nil)

    ;; Don't pop to the REPL buffer on connect
    ;; Create and display the buffer, but don't focus it.
    (setq cider-repl-pop-to-buffer-on-connect 'display-only)

    ;;; skip host question on connect
    (defun cider--completing-read-host (hosts)
      '("localhost")))

  (use-package clj-refactor
    :ensure t
    :after cider
    :config
    ;;; Hook function from https://github.com/clojure-emacs/clj-refactor.el
    (add-hook 'clojure-mode-hook
	      (lambda ()
		(clj-refactor-mode 1)
		(yas-minor-mode 1) ; for adding require/use/import statements
		;; This choice of keybinding leaves cider-macroexpand-1 unbound
		(cljr-add-keybindings-with-prefix "C-c C-m")))

    (dolist (magic-require '(("clerk"    . "nextjournal.clerk")
			     ("csv"      . "clojure.data.csv")
			     ("edn"      . "clojure.edn")
			     ("pprint"   . "clojure.pprint")
			     ("reagent"  . "reagent.core")
			     ("re-frame" . "re-frame.core")))
      (add-to-list 'cljr-magic-require-namespaces magic-require)))

;; (defun clerk-show ()
;;   (interactive)
;;   (when-let
;;       ((filename
;;         (buffer-file-name)))
;;     (save-buffer)
;;     (cider-interactive-eval
;;      (concat "(nextjournal.clerk/show! \"" filename "\")"))))

;; (define-key clojure-mode-map (kbd "<M-return>") 'clerk-show)


;; Thanks to user 'dakra' for sharing this config:
;; - https://github.com/nextjournal/clerk/issues/170#issuecomment-1257013793

;; Shortcut for clerk/show
(defun clerk-serve ()
  "Serve clerk notebooks."
  (interactive)
  (let ((port "7777"))
    (cider-interactive-eval (concat
			     "(nextjournal.clerk/serve! {"
			     ":port " port " "
			     ":browse? false})"))
    (ph/browse-url-in-split-window (concat "http://localhost:" port))))

(defun clerk-build ()
  "Build static html for the current clerk notebook."
  (interactive)
  (message "Building static page")
  (when-let ((filename (buffer-file-name)))
    (let ((root (project-root (project-current t))))
      (cider-interactive-eval
       (concat "(nextjournal.clerk/build! {:paths [\""
               (file-relative-name filename root) "\"]})")))))

(defun clerk-show ()
  "Show buffer in clerk."
  (interactive)
  (message "Show buffer in clerk.")
  (when-let ((filename (buffer-file-name)))
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

(defun clerk-save-and-show ()
  "Save buffer and show in clerk."
  (interactive)
  (save-buffer)
  (clerk-show))

(define-minor-mode clerk-mode
  "A mode that just binds `<M-return>' to `clerk-show'."
  :lighter " clerk"
  :keymap `((,(kbd "<M-return>") . clerk-save-and-show))
  (if clerk-mode
      (add-hook 'after-save-hook #'clerk-show 100 t)
    (remove-hook 'after-save-hook #'clerk-show t)))


(defun ph/buffer-file-parent-dir-name ()
  (interactive)
  (file-name-nondirectory
   (directory-file-name (file-name-directory buffer-file-name))))

(defun activate-hook-for-dir (mode dir-name)
  "Activate `mode` if the directory in which the file resides has `dir-name`."
  (when (string-match-p dir-name (ph/buffer-file-parent-dir-name))
    (funcall mode)))

(add-hook 'find-file-hook
	  (lambda ()
	    (activate-hook-for-dir 'clerk-mode "notebooks")))

(use-package geiser-chicken
  :ensure t
  :config
  (setq geiser-default-implementation 'chicken))

(use-package racket-mode
  :ensure t
  :config
  (setq racket-program "/opt/homebrew/bin/racket")
  (add-to-list 'display-buffer-alist
	       '("\\*Racket REPL.+"
		 (display-buffer-in-side-window)
		 (side . right)
		 (window-width . 0.5))))

(use-package janet-ts-mode
  :vc (:fetcher github :repo sogaiu/janet-ts-mode)
  :ensure t)

(use-package ajrepl
  :ensure t
  :after janet-ts-mode
  :vc (:fetcher github :repo sogaiu/ajrepl)
  :config
  (add-hook 'janet-ts-mode-hook
            #'ajrepl-interaction-mode))

(use-package ajsc
  :ensure t
  :after (janet-ts-mode ajrepl)
  :vc (:fetcher github :repo sogaiu/a-janet-spork-client)
  :config
  (add-hook 'janet-ts-mode-hook
            #'ajsc-interaction-mode))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (dolist (x '(("clj" . clojure-mode)
               ("cljs" . clojure-mode)
               ("cljc" . clojure-mode)))
    (add-to-list 'markdown-code-lang-modes x)))

(use-package separedit
  :ensure t
  :after markdown-mode
  ;; :init
  :config
  ;; Default major-mode for edit buffer
  ;; can also be other mode e.g. ‘org-mode’.
  (setq separedit-default-mode 'markdown-mode)
  
  ;; Key binding for modes you want edit
  ;; or simply bind ‘global-map’ for all.
  (define-key prog-mode-map        (kbd "C-c '") #'separedit)
  (define-key minibuffer-local-map (kbd "C-c '") #'separedit)
  (define-key help-mode-map        (kbd "C-c '") #'separedit)
  (define-key helpful-mode-map     (kbd "C-c '") #'separedit)
  (define-key clojure-mode-map     (kbd "C-c '") #'separedit)

  ;; Feature options
  ;; (setq separedit-preserve-string-indentation t)
  ;; (setq separedit-continue-fill-column t)
  ;; (setq separedit-write-file-when-execute-save t)
  ;; (setq separedit-remove-trailing-spaces-in-comment t)
  )

(setq mac-command-modifier 'meta)          ;; left cmd = right cmd
(setq mac-right-command-modifier 'left)
(setq mac-option-modifier nil)             ;; keeps Umlauts, etc. accessible
(setq mac-right-option-modifier 'left)
(setq mac-control-modifier 'hyper)         ;; in case hyper is needed
(setq mac-right-control-modifier 'control) ;; also works for caps-lock as ctrl

(defun ph/newline-empty-below ()
  "Creates a newline below the point that is always empty."
  (interactive)
  (let ((beg (point)))
    (move-end-of-line nil)
    (open-line 1)
    (goto-char beg)))

(defun ph/newline-empty-above ()
  "Creates a newline above the point that is always empty."
  (interactive)
  (let ((beg (point)))
    (back-to-indentation)
    (open-line 1)
    (goto-char beg)))

(keymap-global-set "C-}" #'ph/newline-empty-below)
(keymap-global-set "C-{" #'ph/newline-empty-above)


(defun ph/join-with-next-line ()
  "Join the current line with the line after it."
  (interactive)
  (join-line -1))


(defun ph/kill-whole-line-move-prev ()
  "Deletes the current line and moves point back to end of previous line."
  (interactive)
  (kill-whole-line)
  (move-end-of-line 0))

;;; UPDATE: I don’t use the following bindings anymore, because of meow

;;; Feels more like Vims S-j to me and I use this very often:
;;; (note: C-j gets overwritten in Lisp Interactive mode)
;; (keymap-global-set "C-M-j" #'delete-indentation) ;; M-^ is weird to type
;; (keymap-global-set "C-S-j" #'ph/join-with-next-line) ; ? or C-c j

;;; I don’t use these often enough for their prominent keybindings:
;; (keymap-global-set "M-o" #'default-indent-new-line) ;; was C-M-j / M-j
;; (keymap-global-set "M-j" #'electric-newline-and-maybe-indent) ;; was C-j

(defun ph/wrap-with-char (start end)
  "Wraps a region with given input character."
  (interactive "r")
  (let ((char (string-to-char (read-string "Enter character: "))))
    (save-excursion
      (goto-char end)
      (insert-char char)
      (goto-char start)
      (insert-char char))))

(defun ph/change-wrapped-char (start end)
  "Changes wrapping characters in a region with given input character."
  (interactive "r")
  (let ((char (let ((input (read-string "Enter character: ")))
		(unless (string-blank-p input)
		  (string-to-char input)))))
    (save-excursion
      (goto-char end)
      (delete-char -1)
      (when char (insert-char char))
      (goto-char start)
      (delete-char 1)
      (when char (insert-char char)))))

(defun ph/remove-surrounding (start end)
  "Removes surrounding characters in a region."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (delete-char -1)
    (goto-char start)
    (delete-char 1)))

(defun ph/insert-date ()
  "Insert current date."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;;; TODO: bind to local key
(defun ph/comint-kill-output ()
  "In shell-mode, kills output instead of deleting, as in
comint-delete-output by default (C-c C-o)."
  (interactive)
  (comint-delete-output t))

(setq eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      ; eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      ; eshell-destroy-buffer-when-process-dies t  ;; WARNING: see variable info
      ; eshell-visual-commands'("bash" "htop" "ssh" "top" "zsh")
      )

(defun ph/find-nearest-color (color &optional use-hsv)
  "Finds the nearest color by RGB distance to COLOR.

If called with a universal argument (or if USE-HSV is set) use HSV instead of RGB.
Runs \\[list-colors-display] after setting `list-colors-sort'"
  (interactive "sColor: \nP")
  (let ((list-colors-sort `(,(if (or use-hsv current-prefix-arg)
                                 'hsv-dist
                               'rgb-dist) . ,color)))
    (if (color-defined-p color)
        (list-colors-display)
      (error "The color \"%s\" does not exist." color))))

(defun ph/find-nearest-color-at-point (pt)
  "Finds the nearest color at point PT.

If called interactively, PT is the value immediately under `point'."
  (interactive "d")
  (ph/find-nearest-color (with-syntax-table (copy-syntax-table (syntax-table))
                           ;; turn `#' into a word constituent to help
                           ;; `thing-at-point' find HTML color codes.
                           (modify-syntax-entry ?# "w")
                           (thing-at-point 'word))))

;; (defun window-full ()
;;   (interactive)
;;   (enlarge-window 1))

;;; inspired from https://www.masteringemacs.org/article/my-emacs-keybindings
(keymap-global-set "M-o" #'other-window)
;;; get rid of the annoying frame minimize command
(keymap-global-set "C-z" nil) 

;; (keymap-global-set "C-J" #'shrink-window-horizontally)
;; (keymap-global-set "C-L" #'enlarge-window-horizontally)
;; (keymap-global-set "C-I" #'enlarge-window)
;; (keymap-global-set "C-K" #'shrink-window)

;;; Source: https://gist.github.com/rmuslimov/72bf5a1561c7b60eb535
(setq
 ph/scripts-dired-reveal-in-finder
 "tell application \"Finder\"
  reveal POSIX file \"%s\"
  activate
end tell")

(defun ph/dired-reveal-in-finder ()
  "In Dired, reveal the location of the file/directory under cursor in Finder."
  (interactive)
  (let* ((filename (dired-get-file-for-visit))
         (cmd (format ph/scripts-dired-reveal-in-finder filename)))
    (do-applescript cmd)))

(keymap-set dired-mode-map "r" #'ph/dired-reveal-in-finder)

(defun ph/dired-open-in-finder ()
  "In Dired, open the file/directory under the cursor in Finder."
  (interactive)
  (let ((file-name (dired-get-file-for-visit)))
    (if (file-exists-p file-name)
        (shell-command (concat "open" " " file-name))
      (message "File does not exist!"))))

(keymap-set dired-mode-map "O" #'ph/dired-open-in-finder)

(defun ph/close-all-popups ()
  "Closes all open popup windows."
  (interactive)
  (dolist (window (window-list))
    (when (window-parameter window 'popup)
      (delete-window window))))

(defun ph/kill-all-help-buffers ()
  "Closes all open help buffers."
  (interactive)
  (let ((buffers (cl-remove-if-not
                  (lambda (b) (string-prefix-p "*Help" (buffer-name b) t))
                  (buffer-list))))
    (dolist (buf buffers)
      (when (buffer-live-p buf)
        (when (get-buffer-window buf)
          ;; Delete window if more than one window is open
          (when (> (length (window-list)) 1)
            (delete-window (get-buffer-window buf))))
        (kill-buffer buf)))))

;; (keymap-global-set "C-`" #'ph/kill-all-help-buffers)

(add-hook 'xwidget-webkit-mode-hook
	  (lambda ()
	    (display-line-numbers-mode 0)))

(defun ph/browse-url-in-split-window (url)
  (interactive "sEnter URL: ")
  (let ((buffer (generate-new-buffer "+xwidget-webkit*")))
    (set-frame-size nil 210 (frame-height))
    (split-window-right)
    (other-window 1)
    (let* ((pixel-size 980)
	   (desired-char-width (/ pixel-size (frame-char-width)))
	   (delta (- desired-char-width (window-total-width))))
      (when (> delta 0)
	(window-resize nil delta t))
      (switch-to-buffer buffer)
      ;; (setq window-size-fixed 'width)
      (xwidget-webkit-browse-url url))))

(defun ph/set-frame-size-xwidget-webkit-split ()
  (interactive)
  (set-frame-size nil 210 (frame-height)))

(defun ph/change-number-at-point (change increment)
  (let ((number (number-at-point))
        (point (point)))
    (when number
      (progn
        (forward-word)
        (search-backward (number-to-string number))
        (replace-match (number-to-string (funcall change number increment)))
        (goto-char point)))))

(defun ph/increment-number-at-point (&optional increment)
  "Increment number at point like vim's C-a"
  (interactive "p")
  (ph/change-number-at-point '+ (or increment 1)))

(defun ph/decrement-number-at-point (&optional increment)
  "Decrement number at point like vim's C-x"
  (interactive "p")
  (ph/change-number-at-point '- (or increment 1)))

(keymap-global-set "C-c +" #'ph/increment-number-at-point)
(keymap-global-set "C-c -" #'ph/decrement-number-at-point)

(defun ph/macos-open (path)
  "Open `path' using the built-in `open' command from MacOS."
  (shell-command (concat "open " path)))

(set-face-attribute 'default nil
                    :font "Berkeley Mono"
                    :height 130 ;; 12 pt
                    :weight 'regular)

(set-face-attribute 'variable-pitch nil
                    :font "Cambria"
                    :height 120
                    :weight 'regular)

(set-face-attribute 'fixed-pitch nil
                    :font "Berkeley Mono"
                    :height 130
                    :weight 'regular)

(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)

(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'normal)

(setq-default line-spacing 0.12)

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))
(load-theme 'pmacs t)

(set-face-attribute
 'org-transclusion-fringe nil
 :foreground "#83be9d"
 ;; :background "#494a63"
 )

(set-face-attribute
 'org-transclusion-source-fringe nil
 :foreground "#83be9d"
 ;; :background "#494a63"
 )

(set-face-attribute
 'org-transclusion nil
 ;; :foreground "#83be9d"
 :background "#2d3c33"
 )

(set-face-attribute
 'org-transclusion-source nil
 ;; :foreground "#83be9d"
 :background "#2d3c33"
 )
