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

(setq doom-font (font-spec :family "Iosevka Term" :size 14 :weight 'light))
;(setq doom-font (font-spec :family "Iosevka" :size 12 :weight 'semi-light)
;      doom-variable-pitch-font (font-spec :family "Fira Sans") ; inherits `doom-font''s :size
;      doom-unicode-font (font-spec :family "Input Mono Narrow" :size 12)
;      doom-big-font (font-spec :family "Fira Mono" :size 19))
;(setq
; doom-font (font-spec :family "Iosevka Term SS04" :size 24 :weight 'light)
; doom-big-font (font-spec :family "Iosevka Term SS04" :size 36))
; doom-variable-pitch-font (font-spec :family "SF Pro Text"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

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


(setq global-evil-mc-mode 1)
(evil-define-key 'visual evil-mc-key-map
  "A" #'evil-mc-make-cursor-in-visual-selection-end
  "I" #'evil-mc-make-cursor-in-visual-selection-beg)
(setq global-evil-surround-mode 1)
;; (map! :desc "Rebind {" :i "M-8" )

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



(setq initial-major-mode 'org-mode)

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

(add-hook 'lisp-mode-hook #'evil-cleverparens-mode)
(add-hook 'scheme-mode-hook #'evil-cleverparens-mode)
(add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
(add-hook 'racket-mode-hook #'evil-cleverparens-mode)
(add-hook 'cider-repl-mode-hook #'evil-cleverparens-mode)
(add-hook 'geiser-repl-mode-hook #'evil-cleverparens-mode)
(add-hook 'racket-repl-mode-hook #'evil-cleverparens-mode)
;; (add-hook 'org-mode-hook #'evil-cleverparens-mode)
(add-hook 'org-src-mode-hook #'evil-cleverparens-mode)
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

(setq geiser-default-implementation 'mit)


(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file "..")))))








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
