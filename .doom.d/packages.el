;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)


;; There is a warning that “Package cl is deprecated” in the minibuffer on
;; startup that cannot be resolved, since cl is required by the
;; ‘persistent-soft’ package that appears to be loaded by Doom Emacs from
;; core/ackages.el -> see https://github.com/doomemacs/doomemacs/pull/2462
;; Apparently, the package author wanted to fix this, but maybe got stuck:
;; https://github.com/rolandwalker/persistent-soft/issues/5

;; (package! linum-relative)

;; Fix OrgMode to 9.6.6 (default in Emacs)
;; -> could be upgraded again since issue was not resolved by downgrade
(package! org :pin "1760867")

(package! ligature)

(package! persistent-scratch)
(package! org-roam)
;; (package! evil-cleverparens)
(package! evil-cleverparens :disable t)
(package! symex)

;; https://github.com/awth13/org-appear
(package! org-appear)

;; Major mode for editing typescript
(package! typescript-mode)

;; Clojure to Emacs Lisp transpilation
(package! clj2el :recipe (:host github :repo "borkdude/clj2el" :files ("*.el")))
(package! carp-mode :recipe (:host github :repo "carp-lang/carp-emacs"
                             :files ("*.el")))
(package! janet-mode :recipe (:host github :repo "ALSchwalm/janet-mode"
                              :files ("*.el")))

;; Logo Mode (works kind-of, but with multiple issues)
;; - see: https://www.emacswiki.org/emacs/LogoMode
;; - overrides Emacs font face/color configs
;; (package! logo-mode
;;   :recipe (:host github :repo "brianharvey/UCBLogo"
;;            :files ("source/emacs/*" "source/helpfiles/*" "source/docs/*")))

(package! org-link-minor-mode
  :recipe (:host github :repo "seanohalpin/org-link-minor-mode"))

;; Workaround for https://github.com/hlissner/doom-emacs/issues/5668
;; See https://github.com/hlissner/doom-emacs/issues/5667#issuecomment-948229579
;; Should no longer be necessary (delete lines below?)
; (package! gitconfig-mode
; 	  :recipe (:host github :repo "magit/git-modes"
; 			 :files ("gitconfig-mode.el")))
; (package! gitignore-mode
; 	  :recipe (:host github :repo "magit/git-modes"
; 			 :files ("gitignore-mode.el")))


;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
