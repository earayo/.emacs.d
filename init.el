;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)


;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
;; (package-initialize) It should be used first time
;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ("melpa" . "http://melpa-stable.milkbox.net/packages/")))

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creates helper functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst +emacs-dir+ "~/.emacs.d")
(defconst +emacs-lib-dir+ (concat +emacs-dir+ "/libs"))

(defun add-load-path (p)
  (add-to-list 'load-path (concat +emacs-dir+ "/" p)))

(defun add-lib-path (p)
  (add-to-list 'load-path (concat +emacs-lib-dir+ "/" p)))



;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ;; ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters
    rainbow-mode

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit

    ;; Better code completion
    company

    ;; Fuzzy matching
    company-flx

    ;; refactorings
    clj-refactor

    ;; smart expand regions
    ;; https://github.com/emacsmirror/expand-region
    expand-region

    ;; https://github.com/jaypei/emacs-neotree
    neotree

    ;; Cool mode suggested by the cider guy
    which-key

    helm
    helm-projectile
    helm-themes
    
    ;; Refactor namespace declarations. Doesn't work. See:
    ;;https://github.com/clojure-emacs/cider/blob/master/CHANGELOG.md
    ;; Problem is due to nrepl-send-string -> nrepl-request:eval
    ;; slamhound

    find-file-in-project

    ;; Javascript
    eslintd-fix
    makey
    tern

    ;; Custom themes
    color-theme-sanityinc-solarized))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

;; Load all the packages!
(when (= 0 (seq-count (lambda (package) (package-installed-p package)) my-packages))
  (package-refresh-contents))

(mapc (lambda (package)
        (unless (package-installed-p package)
          (package-install package)))
      my-packages)

;; find in project
(setq ffip-match-path-instead-of-filename t)
(global-set-key [(meta p)] 'find-file-in-project-by-selected)

;; (dolist (p my-packages)
;;   (when (not (package-installed-p p))
;;     (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")

;; ;;;;;;;;;;;;;;;
;; Load libs
;; ;;;;;;;;;;;;;;;
(add-load-path "lib")

;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
;; (add-to-list 'load-path "~/.emacs.d/customizations")
(add-load-path "customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-golang.el")
(load "setup-javascript.el")

(global-set-key (kbd "RET") 'newline-and-indent)

(defun highlight-dump ()
  "Dump the current buffer to a highlighted clipboard"
  (interactive)
  (shell-command (concat
                   "highlight -O rtf "
                   buffer-file-name
                   " --syntax clojure --style zellner --line-numbers | pbcopy")))

(global-set-key (kbd "C-c f") 'highlight-dump)

(if (and (eq system-type 'windows-nt)
  (require 'cygwin-mount nil t))
(progn
  (setenv "PATH" (concat "c:/cygwin/bin;" (getenv "PATH")))
  (setq exec-path (cons "c:/cygwin/bin/" exec-path))
  (require 'setup-cygwin)
  (setq find-program "c:/cygwin/bin/find.exe")
  (setq grep-program "c:/cygwin/bin/grep.exe")))

;; (setq find-program "c:/cygwin/bin/find.exe")
;; (setq grep-program "c:/cygwin/bin/grep.exe")

;;(setq grep-find-template " c:/cygwin/bin/find . <X> -type f <F> -exec /usr/bin/grep <C> --regexp=<R> --with-filename --line-number --color=always {} +")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" default)))
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-missing-semi-warning nil)
 '(package-selected-packages
   (quote
    (color-theme-sanityinc-solarized ac-cider flycheck-clj-kondo flycheck-golangci-lint ## go-gopath go-guru rainbow-mode which-key tagedit smex rainbow-delimiters neotree magit helm-themes helm-projectile find-file-in-project expand-region company-flx clojure-mode-extra-font-locking clj-refactor))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
