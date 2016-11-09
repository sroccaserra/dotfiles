(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-mode t t)
 '(calc-undo-length 1000)
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(cursor-in-non-selected-windows nil)
 '(custom-safe-themes
   (quote
    ("4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "f3d6a49e3f4491373028eda655231ec371d79d6d2a628f08d5aa38739340540b" "3dafeadb813a33031848dfebfa0928e37e7a3c18efefa10f3e9f48d1993598d3" "1f4e6cf4cb3bdba8afdb9244e037698238080eeecb209084602f7d717225f102" default)))
 '(debug-on-error t)
 '(delete-old-versions t)
 '(electric-indent-mode t)
 '(electric-layout-mode nil)
 '(electric-pair-mode t)
 '(electric-pair-pairs
   (quote
    ((34 . 34)
     (40 . 41)
     (123 . 125)
     (91 . 93)
     (39 . 39))))
 '(evil-default-cursor (quote (t)))
 '(evil-leader/leader ",")
 '(git-gutter:modified-sign "~")
 '(global-auto-revert-mode t)
 '(global-git-gutter-mode t)
 '(global-hl-line-mode t)
 '(global-whitespace-mode t)
 '(helm-boring-file-regexp-list
   (quote
    ("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "~$" "\\.pyc$" "\\.class$")))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (zenburn-theme s pager maxframe markdown-mode+ helm-projectile git-gutter evil-numbers evil-leader dired+ clojure-mode ace-jump-mode)))
 '(pop-up-windows nil)
 '(recentf-max-saved-items 1024)
 '(recentf-mode t)
 '(require-final-newline t)
 '(ruby-indent-level 4)
 '(save-place t nil (saveplace))
 '(savehist-mode t nil (savehist))
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-paren-style (quote parenthesis))
 '(show-trailing-whitespace t)
 '(smart-tab-completion-functions-alist
   (quote
    ((emacs-lisp-mode . lisp-complete-symbol)
     (text-mode . dabbrev-completion)
     (lisp-interaction-mode . lisp-complete-symbol))))
 '(smart-tab-disabled-major-modes (quote (org-mode term-mode eshell-mode Custom-mode)))
 '(smart-tab-using-hippie-expand t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(undo-limit 800000)
 '(undo-strong-limit 1200000)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-follow-symlinks t)
 '(view-exits-all-viewing-windows t)
 '(visible-bell t)
 '(whitespace-style (quote (face tabs trailing lines space-before-tab empty))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-ff-directory ((t (:foreground "#DC8CC3"))) t))
