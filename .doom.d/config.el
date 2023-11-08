;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Eager macro-expansion failure: (wrong-number-of-arguments (3 . 4) 2)
;; see https://github.com/hlissner/doom-emacs/issues/2213
;(load (expand-file-name "~/.roswell/helper.el"))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Peter Hofmann"
      user-mail-address "peter.hofmann@formsandlines.eu")

;; To test configuration:
;; (set-background-color "cornflower blue")


(desktop-save-mode)
;; doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(auto-fill-mode) ; automatically inserts line breaks at char limit
;; (global-display-fill-column-indicator-mode)

;; The 'Regular' cut is too thick and contrast to 'Bold' is insufficient
;; -> will have to wait for v2 release which might include a 'light' cut…
;; (setq doom-font
;;       (font-spec :family "Berkeley Mono"
;;                  :size 14
;;                  :weight 'Regular))
;; (setq doom-variable-pitch-font
;;       (font-spec :family "Berkeley Mono Variable"
;;                  :size 14))

(setq doom-font (font-spec :family "Iosevka Term" :size 14 :weight 'light))

(setq doom-unicode-font doom-font)

;; To resolve problems with modeline icons, see:
;; - https://github.com/doomemacs/doomemacs/issues/7368#issuecomment-1713109316

;(setq doom-font (font-spec :family "Iosevka" :size 12 :weight 'semi-light)
;      doom-variable-pitch-font (font-spec :family "Fira Sans") ; inherits `doom-font''s :size
;      doom-unicode-font (font-spec :family "Input Mono Narrow" :size 12)
;      doom-big-font (font-spec :family "Fira Mono" :size 19))
;(setq
; doom-font (font-spec :family "Iosevka Term SS04" :size 24 :weight 'light)
; doom-big-font (font-spec :family "Iosevka Term SS04" :size 36))
; doom-variable-pitch-font (font-spec :family "SF Pro Text"))

(setq line-spacing 0.2)

;; Note:
;; Seems like on Mac, Emacs ignores `--with-harfbuzz' and `-with-cairo' flags
;; in installation, but ligatures still work, even with `ligatures.el'
;; - although not all ligatures in fonts seem to be recognized(?)
;; - see https://github.com/mickeynp/ligature.el/issues/29
(use-package! ligature
  :config
  ;; Enable ligatures in every possible major mode
  (ligature-set-ligatures
   't
   '("www"))
  ;; Enable ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode
   '(
                                        ; Berkeley Mono sets
                                        ; Group A
     ".." ".=" "..." "..<" "::" ":::" ":=" "::=" ";;" ";;;" "??" "???"
     ".?" "?." ":?" "?:" "?=" "**" "***" "/*" "*/" "/**"
                                        ; Group B
     "<-" "->" "-<" ">-" "<--" "-->" "<<-" "->>" "-<<" ">>-" "<-<" ">->"
     "<-|" "|->" "-|" "|-" "||-" "<!--" "<#--" "<=" "=>" ">=" "<==" "==>"
     "<<=" "=>>" "=<<" ">>=" "<=<" ">=>" "<=|" "|=>" "<=>" "<==>" "||="
     "|=" "//=" "/="
                                        ; Group C
     "<<" ">>" "<<<" ">>>" "<>" "<$" "$>" "<$>" "<+" "+>" "<+>" "<:" ":<"
     "<:<" ">:" ":>" "<~" "~>" "<~>" "<<~" "<~~" "~~>" "~~" "<|" "|>"
     "<|>" "<||" "||>" "<|||" "|||>" "</" "/>" "</>" "<*" "*>" "<*>" ":?>"
                                        ; Group D
     "#(" "#{" "#[" "]#" "#!" "#?" "#=" "#_" "#_(" "##" "###" "####"
                                        ; Group E
     "[|" "|]" "[<" ">]" "{!!" "!!}" "{|" "|}" "{{" "}}" "{{--" "--}}"
     "{!--" "//" "///" "!!"
                                        ; Group F
     "www" "@_" "&&" "&&&" "&=" "~@" "++" "+++" "/\\" "\\/" "_|_" "||"
                                        ; Group G
     "=:" "=:=" "=!=" "==" "===" "=/=" "=~" "~-" "^=" "__" "!=" "!==" "-~"
     "--" "---"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-moonlight) ;; doom-dracula

;; (setq-default inhibit-startup-screen t)
;; (setq inhibit-splash-screen t)
;; (setq inhibit-startup-message t)
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(setq evil-escape-key-sequence "jk")

(setq doom-leader-key "SPC"
      doom-localleader-key ",")

;; (set-face-attribute 'highlight nil :background "red")
(custom-set-faces '(; cursor ((t (:background "yellow")))
                    highlight ((t (:background "#343e5f")))))

(require 'org-link-minor-mode)

(setq global-evil-mc-mode 1)
(evil-define-key 'visual evil-mc-key-map
  "A" #'evil-mc-make-cursor-in-visual-selection-end
  "I" #'evil-mc-make-cursor-in-visual-selection-beg)
(setq global-evil-surround-mode 1)
;; (map! :desc "Rebind {" :i "M-8" )

;; (defun my-kill-help-buffers ()
;;   "Select all help buffers and close them, if they are open."
;;   (interactive)
;;   (let ((buffers (cl-remove-if-not
;;                   (lambda (b) (string-prefix-p "*Help" (buffer-name b) t))
;;                   (buffer-list))))
;;     (dolist (buf buffers)
;;       (when (buffer-live-p buf)
;;         (kill-buffer buf)))))

;; to kill all help buffers:
;; (evil-define-key 'normal 'global
;;   (kbd "C-c q") #'my-kill-help-buffers)

;; because `q` doesn’t close help buffer in evil mode:
;; - ? obsolete -> use `C-~' to toggle visible popups in doom
;; (evil-define-key 'normal 'global
;;   (kbd "C-q") #'+popup/close-all)

; inserts new line (above/below) without entering insert mode
(evil-define-key 'normal 'global
  (kbd "SPC [") #'+evil/insert-newline-above
  (kbd "SPC ]") #'+evil/insert-newline-below
  (kbd "§") #'evil-ex-nohighlight)

;; My special keys:
;; left cmd -> M(was s), right cmd -> M
;; left/right alt -> nil
;; left/right ctrl -> C (also mapped to caps-lock in OSX)
;;
;; (setq mac-control-modifier 'meta)
;; (setq mac-control-modifier 'meta)
;; (setq mac-command-modifier 'super) ; default
(setq mac-command-modifier 'meta)
(setq mac-right-command-modifier 'meta)
(setq mac-right-option-modifier nil)
(setq mac-option-modifier nil)

;; System-like Copy, Paste & Cut (M-x needed, how to scope?)
(evil-define-key 'insert 'global
  (kbd "M-v") #'evil-paste-after)
(evil-define-key 'normal 'global
  (kbd "M-v") #'evil-paste-after
  (kbd "M-c") #'evil-yank)
  ;; (kbd "M-x") #'evil-org-delete)
;; (evil-define-key 'visual 'global
;;   (kbd "M-x") #'evil-org-delete)

;; (map! :desc "Ergonomic paragraph motion > backward" :m "M-8" #'evil-backward-paragraph)
;; (map! :desc "Ergonomic paragraph motion > forward" :m "M-9" #'evil-forward-paragraph)

;; (evil-define-key 'normal 'org-roam
;;   "?" magit-mode-map)


(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(global-set-key "\C-co" 'switch-to-minibuffer) ;; Bind to `C-c o'


(setq initial-major-mode 'org-mode)

(setq org-hide-emphasis-markers t)
(setq org-pretty-entities t)
;; (setq org-startup-with-latex-preview t)

(add-hook 'org-mode-hook 'org-appear-mode)
(setq org-appear-autoentities t)
(setq org-appear-autolinks t)
(setq org-appear-autosubmarkers t)



;; -Fix 'org-fill-paragraph' in 'org-indent-mode'-
;; === OBSOLETE - BUT SOMEHOW NOT IN DOOM??? ===

;;;; issue seems to be with 'org-adaptive-fill-function' in 'org.el', which is
;;;; very different between version 9.7pre (that Doom uses) and 9.6.6 (my bare
;;;; Emacs config, where it works)
;; --> THIS IS PROVED NOT TO BE THE ISSUE!

;; Credits to patrick: https://emacs.stackexchange.com/a/74973
;; (defun current-fill-column ()
;;       "Return the fill-column to use for this line.
;; Subtracts right margin and org indentation level from fill-column"
;;       (let ((indent-level (if (bound-and-true-p org-indent-mode)
;;                               (* org-indent-indentation-per-level
;;                                  (org-current-level))
;;                             0))
;;             (margin (or (get-text-property (point) 'right-margin) 0)))
;;         (- fill-column indent-level margin)))

;; (defun fill-paragraph-org-indent-mode-aware ()
;;   (interactive)
;;   (let ((prev-fill-column fill-column))
;;     (setq fill-column (current-fill-column))
;;     (fill-paragraph)
;;     (setq fill-column prev-fill-column)))

;; (global-set-key (kbd "M-q") 'fill-paragraph-org-indent-mode-aware)
;; (global-set-key (kbd "M-q") 'fill-paragraph)
;; =======================================

;; Using links outside Orgmode
;; see https://stackoverflow.com/a/7048954/1204047
;; and http://orgmode.org/manual/Using-links-outside-Org.html#Using-links-outside-Org
(evil-define-key 'insert 'global
  (kbd "C-c l") #'org-insert-link-global)
(evil-define-key 'normal 'global
  (kbd "M-o") #'org-open-at-point-global)


;; (persistent-scratch-setup-default)

;;(setq persistent-scratch--auto-restored t)
(persistent-scratch-autosave-mode)
(setq persistent-scratch--auto-restored t)
(setq persistent-scratch-backup-directory "~/.emacs.d/persistent-scratch-backups/")
(setq persistent-scratch-backup-filter (persistent-scratch-keep-n-newest-backups 100))
;; setq persistent-scratch-backup-filter (persistent-scratch-keep-backups-not-older-than ()))

(setq org-roam-directory
      (file-truename "~/Documents/emacs/org-roam"))

(org-roam-db-autosync-mode)

;; (evil-define-key 'normal 'org-roam
;;   "C-c n l" #'org-roam-buffer-toggle
;;   "C-c n f" #'org-roam-node-find
;;   "C-c n i" #'org-roam-node-insert)

(map! :leader
      :mode 'org-roam ; not really local -> ?
      (:prefix ("r" . "Org-roam")
       :desc "Toggle Org-roam buffer"
       "b" #'org-roam-buffer-toggle
       :desc "Find node"
       "f" #'org-roam-node-find
       :desc "Insert node"
       "i" #'org-roam-node-insert))

(map! :map org-mode-map
      :g "C-M-i" #'completion-at-point)

(setq org-roam-mode-section-functions
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            ;; #'org-roam-unlinked-references-section
            ))

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

(setq org-roam-completion-everywhere t)

(setq org-roam-capture-templates
 '(("d" "default" plain ; Default template for org-roam:
    "%?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
    :unnarrowed t)))

(setq org-roam-dailies-directory "daily/")

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))

;; (add-hook 'lisp-mode-hook #'evil-cleverparens-mode)
;; (add-hook 'scheme-mode-hook #'evil-cleverparens-mode)
;; (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
;; (add-hook 'racket-mode-hook #'evil-cleverparens-mode)
;; (add-hook 'cider-repl-mode-hook #'evil-cleverparens-mode)
;; (add-hook 'geiser-repl-mode-hook #'evil-cleverparens-mode)
;; (add-hook 'racket-repl-mode-hook #'evil-cleverparens-mode)

;; (add-hook 'org-mode-hook #'evil-cleverparens-mode)
;; (add-hook 'org-src-mode-hook #'evil-cleverparens-mode)

;; (add-hook 'org-src-mode-hook (lambda () (evil-cleverparens-mode 1)))


(map! :map evil-cleverparens-mode-map "M-n" #'evil-cp-beginning-of-defun)

(map! :localleader
      :mode 'cider-mode
      (:desc "Find variable"
       "." #'cider-find-var
       :desc "Pop back"
       "," #'cider-pop-back))

(map! :map cider-repl-mode-map "M-]" #'cider-find-var)
(map! :map cider-repl-mode-map "M-[" #'cider-pop-back)

(setq cider-show-error-buffer t)
;; (setq cider-auto-jump-to-error t)
;; (setq cider-auto-select-error-buffer t)

;; (setq geiser-racket-binary "/opt/homebrew/bin/racket")
(setq geiser-active-implementations '(racket chicken))
(setq geiser-default-implementation 'chicken)
(setq geiser-implementations-alist ;; does this even do something?
      '(((regexp "\\.scm$") chicken)
        ((regexp "\\.ss$") racket)
        ((regexp "\\.rkt$") racket)))

;; (add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

;; Geiser does not correctly recognize the Racket filetype to associate it with
;; the Racket implementation. How can this be fixed?
;; It seems like it uses a heuristic to automatically determine the implementation,
;; but ignores all other specifications in config once it could do that.
;; See https://www.nongnu.org/geiser/Between-the-parens.html

(defun my-geiser-racket-mode ()
  (when (string-equal (file-name-extension buffer-file-name) "rkt")
    ;; (and (string-equal (file-name-extension buffer-file-name) "rkt")
    ;;      (memq geiser-impl--implementation '(guile racket)))
    (geiser-mode 'racket)))

(add-hook 'scheme-mode-hook 'my-geiser-racket-mode)


(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file "..")))))


(use-package! symex
  :config
  (symex-initialize)
  (global-set-key (kbd "C-'") 'symex-mode-interface))


;; From insert mode to symex-mode:

;; (setq evil-symex-state-cursor 'box)
;; (setq symex-highlight-p nil)

(evil-define-key 'normal symex-mode-map
  (kbd "<escape>") 'symex-mode-interface)

;; (evil-define-key 'insert symex-mode-map
;;   (kbd "<escape>") 'symex-mode-interface)


(setq typescript-indent-level 2)

;; (add-hook 'typescript-mode-hook
;;           (setq))


;; Prevents Evil from overwriting Ciders keybindings e.g. in debugger
;; - does not work in symex mode
;; - https://github.com/emacs-evil/evil-collection/blob/master/modes/cider/evil-collection-cider.el#L92-L109
;; CRASHES IN ARM
; (evil-collection-cider-setup)

;; To enter debug mode from symex-mode also:
;; https://emacs.stackexchange.com/a/20818
(defun my-cider-debug-toggle-insert-state ()
  (if cider--debug-mode   ;; Checks if you're entering the debugger
      (evil-insert-state) ;; If so, turn on evil-insert-state
    (evil-normal-state))) ;; Otherwise, turn on normal-state

(add-hook 'cider--debug-mode-hook 'my-cider-debug-toggle-insert-state)


(use-package! clj2el)


(use-package! carp-mode)
(require 'carp-mode)
(require 'inf-carp-mode)
(require 'carp-flycheck)

;; Use carp-mode for .carp files
(add-to-list 'auto-mode-alist '("\\.carp\\'" . carp-mode))

(add-hook 'carp-mode-hook (lambda () (flycheck-mode 1)))


(use-package! janet-mode)
(require 'janet-mode)

;; logo-mode (experimental -> see packages.el)
;; (setq auto-mode-alist (append '(("\\.lgo?$" . logo-mode)) auto-mode-alist))
;; (setq logo-binary-name "/Applications/UCBLogo.app/Contents/MacOS/UCBLogo")
;; (setq logo-mode-root (string-replace "logo.el" "" (locate-library "logo")))
;; (setq logo-tutorial-path logo-mode-root)
;; (setq logo-help-path logo-mode-root)
;; (setq logo-info-file (string-join (list logo-mode-root "ucblogo.info") ""))

;; Custom modeline modal state indicator (doesn’t work yet):

;; (defsubst doom-modeline--symex ()
;;   "The current Symex state. Requires `symex-mode' to be enabled."
;;   (when (bound-and-true-p symex-mode)
;;     symmex--indicator))

;; (doom-modeline-def-segment modals
;;   "Displays modal editing states, including `evil', `overwrite', `god', `ryo'
;; and `xha-fly-kyes', etc."
;;   (let* ((evil (doom-modeline--evil))
;;          (ow (doom-modeline--overwrite))
;;          (god (doom-modeline--god))
;;          (ryo (doom-modeline--ryo))
;;          (xf (doom-modeline--xah-fly-keys))
;;          (boon (doom-modeline--boon))
;;          (vsep (doom-modeline-vspc))
;;          (meow (doom-modeline--meow))
;;          (symex (doom-modeline--symex))
;;          (sep (and (or evil ow god ryo xf boon) (doom-modeline-spc))))
;;     (concat sep
;;             (and evil (concat evil (and (or ow god ryo xf boon meow) vsep)))
;;             (and ow (concat ow (and (or god ryo xf boon meow) vsep)))
;;             (and god (concat god (and (or ryo xf boon meow) vsep)))
;;             (and ryo (concat ryo (and (or xf boon meow) vsep)))
;;             (and xf (concat xf (and (or boon meow) vsep)))
;;             (and boon (concat boon (and meow vsep)))
;;             meow
;;             sep)))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
