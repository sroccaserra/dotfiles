;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful key bindings

(defun my-kbd (str)
  "Private kbd, the original one breaks for M-체."
  (read-kbd-macro (encode-coding-string str locale-coding-system)))

(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-cm" 'execute-extended-command)

(global-set-key [(control +)] 'text-scale-increase)

;;;;;;;;;;;;;;;
;; Minibuffer

(define-key minibuffer-local-map [(control n)] 'next-complete-history-element)
(define-key minibuffer-local-map [(control p)] 'previous-complete-history-element)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text deletion customization

(global-set-key [(control shift d)]   'backward-delete-char-untabify)
;; (global-set-key [(control backspace)] 'kill-syntax-backward)
;; (global-set-key [(meta shift d)]      'kill-syntax-backward)
;; (global-set-key [(meta d)]            'kill-syntax-forward)

;; (defun kill-syntax-forward ()
;;   "Kill characters with syntax at point."
;;   (interactive)
;;   (kill-region (point)
;;                (progn
;;                  (skip-syntax-forward (string (char-syntax (char-after))))
;;                  (point))))

;; (defun kill-syntax-backward ()
;;   "Kill characters with syntax at point."
;;   (interactive)
;;   (kill-region (point)
;;                (progn
;;                  (skip-syntax-backward (string (char-syntax (char-before))))
;;                  (point))))

;;;
;; Page up & down fix
(require 'pager)
(global-set-key [(control v)] 'pager-page-down)
(global-set-key [next]        'pager-page-down)
(global-set-key [(meta v)]    'pager-page-up)
(global-set-key [prior]       'pager-page-up)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ISearch mods

;;;
;; Always use regexps
(global-set-key [(control s)]               'isearch-forward-regexp)
(global-set-key [(control r)]               'isearch-backward-regexp)
(global-set-key [(meta %)]                  'query-replace-regexp)
(define-key isearch-mode-map (my-kbd "M-첫") 'isearch-query-replace-regexp)

;;;
;; Always end searches at the beginning of the matching expression.

;; (add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)

;; (defun custom-goto-match-beginning ()
;;   "Use with isearch hook to end search at first char of match."
;;   (setq viper-s-string isearch-string)
;;   (->> (goto-char isearch-other-end)
;;        (unless (null isearch-other-end))
;;        (when isearch-forward)))

;;;
;; Move to beginning of word before yanking word in isearch-mode.
;; Make C-s C-w and C-r C-w act like Vim's g* and g#, keeping Emacs'
;; C-s C-w [C-w] [C-w]... behaviour.

;; (require 'thingatpt)
;; (defun my-isearch-yank-word-or-char-from-beginning ()
;;   "Move to beginning of word before yanking word in isearch-mode."
;;   (interactive)
;;   (if (= 0 (length isearch-string))
;;       (beginning-of-thing 'word))
;;   (isearch-yank-word-or-char)
;;   ;; Revert to 'isearch-yank-word-or-char for subsequent calls
;;   (substitute-key-definition 'my-isearch-yank-word-or-char-from-beginning
;;                              'isearch-yank-word-or-char
;;                              isearch-mode-map))
;;
;; (add-hook 'isearch-mode-hook
;;           (lambda ()
;;             "Activate my customized Isearch word yank command."
;;             (substitute-key-definition 'isearch-yank-word-or-char
;;                                        'my-isearch-yank-word-or-char-from-beginning
;;                                        isearch-mode-map)))

;;;
;; Zap up to char
;; (require 'misc)

;; (global-set-key "\M-z"      'zap-up-to-char)
;; (global-set-key (kbd "M-Z") 'my-zap-back-to-char)

;; (defun my-zap-back-to-char (arg char)
;;   "No need to enter C-- to zap back"
;;   (interactive "p\ncZap back to char: ")
;;   (zap-to-char (- arg) char))

;;;
;; Vim's * and #
;; (require 'etags)

;; (defun isearch-yank-regexp (regexp)
;;   "Pull REGEXP into search regexp."
;;   (let ((isearch-regexp nil)) ;; Dynamic binding of global.
;;     (isearch-yank-string regexp))
;;   (if (not isearch-regexp)
;;       (isearch-toggle-regexp))
;;   (isearch-search-and-update))

;; (defun isearch-yank-symbol ()
;;   "Put symbol at current point into search string."
;;   (interactive)
;;   (let ((sym (find-tag-default)))
;;     (if (null sym)
;;         (message "No symbol at point")
;;       (isearch-yank-regexp
;;        (concat "\\_<" (regexp-quote sym) "\\_>")))))

;; (define-key isearch-mode-map "\C-\M-w" 'isearch-yank-symbol)

;; ;;;
;; ;; Vim's Ctrl-r /
;; (global-set-key (kbd "C-S-s") (lambda ()
;;                                  "Inserts last searched string."
;;                                  (interactive)
;;                                  (insert isearch-string)))

;; (global-set-key (kbd "C-S-k") 'fc-kill-to-beginning-of-line)
;; (defun fc-kill-to-beginning-of-line ()
;;   "Kill from the beginning of the line to point."
;;   (interactive)
;;   (kill-region (point-at-bol)
;;                (point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto parenthesis & the like

(define-key lisp-mode-shared-map "'" 'self-insert-command)
(define-key text-mode-map "'" 'self-insert-command)
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map "'" 'self-insert-command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moving lines up & down with <M-up> & <M-down>

(global-set-key [(meta up)]   'move-line-up)
(global-set-key [(meta down)] 'move-line-down)

(defun move-line (&optional n)
  "Move current line N (1) lines up/down leaving point in place."
  (interactive "p")
  (when (null n)
    (setq n 1))
  (let ((col (current-column)))
    (beginning-of-line)
    (forward-line)
    (transpose-lines n)
    (forward-line -1)
    (forward-char col))
  (indent-according-to-mode))

(defun move-line-up (n)
  "Moves current line N (1) lines up leaving point in place."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Moves current line N (1) lines down leaving point in place."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun duplicate-start-of-line-or-region ()
  (interactive)
  (if mark-active
      (duplicate-region)
    (duplicate-start-of-line)))

(defun duplicate-start-of-line ()
  (let ((text (buffer-substring (point)
                                (beginning-of-thing 'line))))
    (forward-line)
    (push-mark)
    (insert text)
    (open-line 1)))

(defun duplicate-region ()
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning)
                                 end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))

(global-set-key [(control x)(control e)] 'pp-eval-last-sexp)
(defun eval-last-sexp-or-region ()
  (interactive)
  (if mark-active
      (eval-region (region-beginning) (region-end) t)
    (pp-eval-last-sexp nil)))

;;;
;; Arrows

(defun my-restore-arrows ()
  (interactive)
  "Restore arrows for other people."
  (global-set-key (kbd "<up>") 'previous-line)
  (global-set-key (kbd "<down>") 'next-line)
  (global-set-key (kbd "<left>") 'backward-char)
  (global-set-key (kbd "<right>") 'forward-char))

(defun my-hide-arrows ()
  (global-set-key (kbd "<up>") 'windmove-up)
  (global-set-key (kbd "S-<up>") 'previous-line)
  (global-set-key (kbd "<down>") 'windmove-down)
  (global-set-key (kbd "S-<down>") 'next-line)
  (global-set-key (kbd "<left>") 'windmove-left)
  (global-set-key (kbd "S-<left>") 'backward-char)
  (global-set-key (kbd "<right>") 'windmove-right)
  (global-set-key (kbd "S-<right>") 'forward-char))

;;;
;; Anchored transpose
(global-set-key [?\C-x ?t] 'anchored-transpose)
(autoload 'anchored-transpose "anchored-transpose" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers

(global-set-key (kbd "C-x !") 'my-transpose-buffers) ;French Mac
(global-set-key (kbd "C-x _") 'my-transpose-buffers) ;French PC
(defun my-transpose-buffers ()
  "Transpose this buffer and the buffer in other window"
  (interactive)
  (let ((current-window-buffer (current-buffer)))
    (other-window 1)
    (let ((other-window-buffer (current-buffer)))
      (switch-to-buffer current-window-buffer)
      (other-window -1)
      (switch-to-buffer other-window-buffer)))
  (other-window 1))

(define-key *my-map* "k" 'server-done-or-kill-buffer)
(defun server-done-or-kill-buffer ()
  (interactive)
  (let ((server-done-result (and (fboundp 'server-done)
                                 (server-done))))
    (if server-done-result
        (server-switch-buffer server-done-result)
      (kill-buffer (current-buffer)))))

(global-set-key [mouse-2] 'describe-function-at-point)
(defun describe-function-at-point (&optional event)
  (interactive "e")
  (mouse-set-point event)
  (when (function-called-at-point)
    (describe-function (function-called-at-point))))

(define-key *my-map* "K" 'my-kill-buffer-other-window)
(global-set-key (kbd "C-x 4 k")   'my-kill-buffer-other-window)
(defun my-kill-buffer-other-window ()
  (interactive)
  (other-window 1)
  (kill-buffer (current-buffer))
  (other-window -1))

;; buffer list
;; (require 'bs)
;; (global-set-key [(control x) (control b)] 'bs-show-sorted)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-file-dir-completion-map (kbd "<up>")
              'ido-prev-work-directory)
            (define-key ido-file-dir-completion-map (kbd "<down>")
              'ido-next-work-directory)))

(define-key *my-map* "d" 'my-ediff-with-other-window)
(defun my-ediff-with-other-window ()
  "Ediff curent buffer & other window buffer."
  (interactive)
  (let ((current-window-buffer (current-buffer)))
    (other-window 1)
    (ediff-buffers
     current-window-buffer
     (current-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Norton Commander - like shortcuts

;; (global-set-key [f5] (lambda ()
;;                        "Copy buffer to other window."
;;                        (interactive)
;;                        (let ((current-window-buffer (current-buffer)))
;;                          (other-window 1)
;;                          (switch-to-buffer current-window-buffer))
;;                        (other-window -1)))

(global-set-key [f6] (lambda ()
                       "Move buffer to other window."
                       (interactive)
                       (let ((current-window-buffer (current-buffer)))
                         (bury-buffer)
                         (other-window 1)
                         (switch-to-buffer current-window-buffer))))

(global-set-key [f8] (lambda ()
                       "Delete (burry) buffer."
                       (interactive)
                       (bury-buffer)))
(global-set-key [S-f8] (lambda ()
                         "Delete (burry) other window buffer."
                         (interactive)
                         (let ((current-window-buffer (current-buffer)))
                           (other-window 1)
                           (bury-buffer))
                         (other-window -1)))

;; Flymake
;; (global-set-key [f4] (lambda ()
;;                         (interactive)
;;                         (flymake-goto-prev-error)
;;                         (flymake-display-err-menu-for-current-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clumsy French keyboard keys relocation & usefull function bindings

(global-set-key (kbd "짼")          'mark-word)
(global-set-key (my-kbd "M-짼")     'mark-word)
(global-set-key (my-kbd "M-횪")     'mark-word)
(global-set-key (my-kbd "M-(")     'mark-sexp)
(global-set-key (my-kbd "M-첫")     'query-replace-regexp)
(global-set-key (kbd "C-x 챕")      'split-window-vertically)   ;French
(global-set-key (kbd "C-x 횪")      'delete-window)             ;French
(global-set-key [(control meta _)] 'indent-region) ;French PC
(global-set-key [(control meta :)] 'indent-region) ;French Mac
(global-set-key [(meta _)]         'align-regexp)
(global-set-key [(meta n)]         'forward-paragraph)
(global-set-key [(meta p)]         'backward-paragraph)
(global-set-key [(control x)(control x)] 'exchange-point-and-mark-nomark)
(define-key *my-map* (kbd "SPC")     'delete-trailing-whitespace)
(define-key *my-map* (kbd "o")       'occur)
(define-key *my-map* (kbd "l")       'longlines-mode)

;; For clumsy co-workers
(global-set-key [(control z)]       'undo)
(global-set-key [(control shift z)] 'redo)
(global-set-key [(control shift f)] 'rgrep)

(setq woman-use-own-frame nil)     ; don't create new frame for manpages
(setq woman-use-topic-at-point t)  ; don't prompt upon K key (manpage display)


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pp-eval

(substitute-key-definition 'eval-last-sexp
                           'pp-eval-last-sexp global-map)
(substitute-key-definition 'eval-expression
                           'pp-eval-expression global-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tab

(defun hippie-unexpand ()
  (interactive)
  (hippie-expand 0))

(global-set-key [(shift tab)] 'hippie-unexpand)

(require 'smart-tab)
(global-smart-tab-mode 1)

(define-key read-expression-map [(tab)] 'hippie-expand)
(define-key read-expression-map [(shift tab)] 'hippie-unexpand)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation

(autoload 'smart-compile "smart-compile" nil t)
(global-set-key [(f9)] 'smart-compile)

(autoload 'compile-goto-error-and-close-compilation-window "smart-compile" nil t)
(eval-after-load "compile"
  '(define-key
     compilation-mode-map
     [remap compile-goto-error]
     'compile-goto-error-and-close-compilation-window))
