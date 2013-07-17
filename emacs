;; -*- mode:emacs-lisp -*-
;; vi: set filetype=lisp:

;;;;;;;;;;;;;;;;
;;; Packages

(defun is-emacs-24-or-more ()
  (string-match "Emacs \\([0-9]+\\)" (version))
  (let ((v (string-to-int (match-string 1 (version)))))
    (>= v 24)))

(unless (is-emacs-24-or-more)
  (when (not (file-exists-p "~/.emacs.d/elpa/package.el"))
    (make-directory "~/.emacs.d/elpa" t)
    (url-copy-file "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el" "~/.emacs.d/elpa/package.el"))
  (load "~/.emacs.d/elpa/package.el"))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar my-packages '(clojure-mode clojure-test-mode dired+ evil evil-leader evil-numbers helm
  helm-projectile powershell-mode projectile undo-tree
    zenburn-theme)
  "List of my sine qua non packages")

(unless (is-emacs-24-or-more)
  (add-to-list 'my-packages 'color-theme t))

(require 'cl)
(let ((missing-packages (remove-if 'package-installed-p my-packages)))
  (when missing-packages
    (package-refresh-contents)
    (dolist (p missing-packages)
      (package-install p))))


;;;;;;;;;;;;;;;;;
;; Encoding prefs

(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")


;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

(setq custom-file "~/dotfiles/custom.el")
(load-file custom-file)

;;;;;;;;;;;
;; My tools

(defvar *emacs-plugin-dirs*
    '("~/developer/smart-tab" "~/developer/emacs"))
(dolist (dir *emacs-plugin-dirs*)
    (add-to-list 'load-path dir t))

(require 'tools)
(require 'smart-tab)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil

(require 'evil)
(evil-mode 1)

(require 'evil-leader)
(global-evil-leader-mode t)

(evil-ex-define-cmd "p[rojectile]" 'helm-projectile)
(evil-ex-define-cmd "a[nything]" 'save-buffer-then-helm)
(evil-leader/set-key "t" 'save-buffer-then-helm)
(defun save-buffer-then-helm ()
  (interactive)
  (when (buffer-file-name)
    (save-buffer))
  (cond ((fboundp 'helm-for-files)
         (helm-for-files))
        ((fboundp 'helm-mini)
         (helm-mini))
        (t (ibuffer))))

(load-theme 'zenburn)

;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tab

(defun hippie-unexpand ()
  (interactive)
  (hippie-expand 0))

(global-set-key [(shift tab)] 'hippie-unexpand)
(global-set-key [(backtab)] 'hippie-unexpand)

(global-smart-tab-mode 1)

(define-key read-expression-map [(tab)] 'hippie-expand)
(define-key read-expression-map [(shift tab)] 'hippie-unexpand)

