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

(defvar my-packages '(clojure-mode clojure-test-mode dired+ evil
  evil-leader evil-numbers git-gutter helm helm-projectile
  maxframe pager powershell-mode projectile undo-tree
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

(setq frame-title-format
      (concat "%b %+ (%f) - " invocation-name))

;;;;;;;;;;;
;; My tools

(defvar *emacs-plugin-dirs*
    '("~/developer/smart-tab" "~/developer/emacs"))
(dolist (dir *emacs-plugin-dirs*)
    (add-to-list 'load-path dir t))

(require 'smart-tab)
(require 'tools)


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

;;;;;;;;;;;;
;; Libraries

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(load-theme 'zenburn)

(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

(whitespace-mode)

;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

(define-key global-map (kbd "RET") 'newline-and-indent)

(global-set-key [(control c)(control m)] 'execute-extended-command)
(global-set-key [(control c)(m)] 'execute-extended-command)

(define-key minibuffer-local-map [(control n)] 'next-complete-history-element)
(define-key minibuffer-local-map [(control p)] 'previous-complete-history-element)

(global-set-key [(control shift d)]   'backward-delete-char-untabify)

(setq skeleton-pair t)
(global-set-key (kbd "[")  'skeleton-pair-insert-maybe)
(global-set-key (kbd "(")  'skeleton-pair-insert-maybe)
(global-set-key (kbd "{")  'skeleton-pair-insert-maybe)
(global-set-key (kbd "<")  'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "'") 'skeleton-pair-insert-maybe)

(define-key lisp-mode-shared-map "'" 'self-insert-command)
(define-key text-mode-map "'" 'self-insert-command)
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map "'" 'self-insert-command)))

(require 'pager)
(global-set-key [(control v)] 'pager-page-down)
(global-set-key [next]        'pager-page-down)
(global-set-key [(meta v)]    'pager-page-up)
(global-set-key [prior]       'pager-page-up)


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

;;;;;;;;
;; Tests

(define-key emacs-lisp-mode-map [(f9)] 'save-and-eval-buffer-then-ert-run-tests)
(defun save-and-eval-buffer-then-ert-run-tests ()
  (interactive)
  (when (buffer-file-name)
    (save-buffer))
  (when (fboundp 'ert-delete-all-tests)
    (ert-delete-all-tests))
  (eval-buffer)
  (when (fboundp 'ert-run-tests-interactively)
    (ert-run-tests-interactively t)))
