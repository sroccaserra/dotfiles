;;;;;;;;;;;;;;;;
;;; Packages

(load
  (expand-file-name "~/.emacs.d/elpa/package.el"))

(require 'package)
(add-to-list 'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar my-packages '(clojure-mode clojure-test-mode dired+ evil evil-leader evil-numbers helm
  helm-projectile powershell-mode projectile undo-tree
    zenburn-theme)
  "List of my sine qua non packages")

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

