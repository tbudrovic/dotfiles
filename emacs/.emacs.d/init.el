;;; init.el --- starting point of Emacs configuration
;;; Commentary:
;;; this is here to silence flymake

;;; Code:

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(require 'package)

(setq-default use-package-always-ensure t)
(setq package-archives nil)

(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; load packages info if there is no cache
(let 'init-dir (file-name-directory (or load-file-name (buffer-file-name)))
     (unless (and (file-exists-p (concat init-dir "elpa/archives/gnu"))
		  (file-exists-p (concat init-dir "elpa/archives/melpa")))
       (package-refresh-contents)))

(defun packages-install (&rest packages)
  "Install any missing packages.
PACKAGES list of packages to install"
  (message "running packages-install")
  (mapc (lambda (package)
	  (let ((name (car package))
		(repo (cdr package)))
	    (when (not (package-installed-p name))
	      (let ((package-archives (list repo)))
		(package-initialize)
		(package-install name)))))
	packages)
  (package-initialize)
  (delete-other-windows))

(defun init--install-packages ()
  "Install extensions if they are missing."
  (message "Lets install some packages")
  (packages-install
   ;; Since use-package this is the only entry here
   ;; ALWAYS try to use use-package!
   (cons 'use-package '("melpa" . "https://melpa.org/packages/"))
   ))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq
 inhibit-splash-screen t
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 initial-scratch-message "")

(set-frame-font "Knack Nerd Font 10")

;; location of backup and temporary files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(show-paren-mode t)
(electric-pair-mode t)
(setq show-trailing-whitespace t)
(setq fill-column 80)
(global-subword-mode t)
(setq-default indent-tabs-mode nil)

(setq cursor-type 'bar)
(setq cursor-in-non-selected-windows 'outline)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;;; keep quiet
(defun my-bell-function () "Do nothing.")
(setq ring-bell-function 'my-bell-function)
(setq visible-bell nil)


;;; Packages configuration
;;; init: before package loading
;;; config: after package loading

(require 'ido)
(use-package ido
  :config
  (ido-mode)
  (setq ido-enable-flex-matching t)
  (setq ido-create-new-buffer 'always)
  (ido-everywhere))

(use-package which-key
  :config
  (which-key-mode))

(use-package flycheck-pos-tip
  :init
  (global-flycheck-mode)
  :config
  (flycheck-pos-tip-mode))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode t))))

(use-package web-mode
  :mode "\\.erb\\'"
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing nil))

(use-package ruby-mode
  :mode ("Capfile" "Gemfile" "Rakefile" "\\.rake\\'" "\\.rb\\'")
  :init
  (setq ruby-deep-arglist t)
  (setq ruby-deep-indent-paren nil))

(use-package evil
  :config
  (evil-mode))

(use-package linum-relative
  :config
  (setq linum-relative-current-symbol "")
  (global-linum-mode)
  (linum-relative-mode))

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(use-package rust-mode)

(use-package frames-only-mode
  :config
  (frames-only-mode))

(use-package solarized-theme
  :config
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-emphasize-indicators nil)
  (setq solarized-scale-org-headlines nil)
  (load-theme 'solarized-dark 'no-confirm))


(provide 'init)

;;; init.el ends here
