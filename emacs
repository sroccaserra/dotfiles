;; -*- mode:emacs-lisp -*-OB

;; vi: set filetype=lisp:

;;;;;;;;;;;;;;;;
;;; Packages

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(defvar my-packages '(ace-jump-mode clojure-mode
  dash default-text-scale editorconfig evil evil-leader evil-numbers
  helm helm-projectile maxframe pager
  projectile racket-mode rainbow-delimiters s undo-tree zenburn-theme)
  "List of my sine qua non packages")

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

(add-to-list 'exec-path "~/bin")
(add-to-list 'exec-path "/usr/local/bin/")


;;;;;;;
;; Evil

(require 'evil)
(evil-mode 1)

(require 'evil-leader)
(global-evil-leader-mode t)

(evil-ex-define-cmd "p[rojectile]" 'helm-projectile)
(evil-ex-define-cmd "a[nything]" 'save-buffer-then-helm)
(evil-leader/set-key "p" 'helm-projectile)
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

(require 'ace-jump-mode)
(setq ace-jump-word-mode-use-query-char nil)

(require 'dash)
(require 's)
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (make-local-variable 'electric-pair-pairs)
                                  (delete '(?\' . ?\') electric-pair-pairs)))

(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))

(whitespace-mode)
(default-text-scale-mode)

(load-theme 'zenburn)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;;;;;;;;;;
;; My tools

(--each '("~/Developer/smart-tab" "~/Developer/emacs")
  (add-to-list 'load-path it t))

(require 'smart-tab)


;;;;;;;;;;;;;;;;;;
;; My key bindings

(define-key global-map (kbd "RET") 'newline-and-indent)

(global-set-key [(control c)(control m)] 'execute-extended-command)
(global-set-key [(control c)(m)] 'execute-extended-command)
(global-set-key [(super left)] 'move-beginning-of-line)
(global-set-key [(super right)] 'move-end-of-line)

(define-key minibuffer-local-map [(control n)] 'next-complete-history-element)
(define-key minibuffer-local-map [(control p)] 'previous-complete-history-element)

(global-set-key [(control shift d)]   'backward-delete-char-untabify)

;; Note: skeleton-pair is now replaced by electric-pair
;;
;; (setq skeleton-pair t)
;; (global-set-key (kbd "[")  'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "(")  'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "{")  'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "<")  'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "'") 'skeleton-pair-insert-maybe)
;;
;; (define-key lisp-mode-shared-map "'" 'self-insert-command)
;; (define-key text-mode-map "'" 'self-insert-command)
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (define-key org-mode-map "'" 'self-insert-command)))


(require 'pager)
(global-set-key [(control v)] 'pager-page-down)
(global-set-key [next]        'pager-page-down)
(global-set-key [(meta v)]    'pager-page-up)
(global-set-key [prior]       'pager-page-up)

(evil-leader/set-key ",w" 'ace-jump-word-mode)
(evil-leader/set-key ",f" 'ace-jump-char-mode)
(evil-leader/set-key ",j" 'ace-jump-line-mode)

(when (and (tty-type)
           (s-contains? "xterm" (tty-type) t))
  (define-key input-decode-map "\e[4~" [end]))

;; Note: customized mac-right-option-modifier to nil instead
;;
;; (if (string-match "Aquamacs\\|NS apple-appkit\\|NS appkit\\|darwin"
;;                   (emacs-version))
;;  (progn
;;    (global-set-key [(meta n)] (lambda () (interactive) (insert-string "~")))
;;    (global-set-key (kbd "M-(") (lambda () (interactive) (insert-string "{")))
;;    (global-set-key (kbd "M-)") (lambda () (interactive) (insert-string "}")))
;;    (global-set-key (kbd "M-5") (lambda () (interactive) (insert-string "[")))
;;    (global-set-key (kbd "M-°") (lambda () (interactive) (insert-string "]")))
;;    (global-set-key [(meta l)] (lambda () (interactive) (insert-string "|")))
;;    (global-set-key [(meta /)] (lambda () (interactive) (insert-string "\\")))
;;    (global-set-key [(meta $)] (lambda () (interactive) (insert-string "€")))
;;  )
;; )


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
