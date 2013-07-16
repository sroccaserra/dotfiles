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

;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file "~/dotfiles/custom.el")
(load-file custom-file)

(require 'evil)
(evil-mode 1)

(require 'evil-leader)
(global-evil-leader-mode t)

(cond ((is-emacs-24-or-more)
       (load-theme 'zenburn t))
      (t
       (require 'color-theme)
       (color-theme-initialize)
       (load-theme 'zenburn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil

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

