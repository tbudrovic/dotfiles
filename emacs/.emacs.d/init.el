;;; init.el --- starting point of Emacs configuration
;;; Commentary:
;;; this is here to silence flymake

;;; Code:

(setq-default tls-checktrust t)
(let '(trustfile "/etc/ssl/certs/ca-certificates.crt")
  (setq-default tls-program
		(list
		 (format "gnutls-cli --x509-cafile %s -p %%p %%h"
			 trustfile)))
  (setq-default gnutls-verify-error t)
  (setq-default gnutls-trustfiles (list trustfile)))

(setq package-archives nil)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(setq-default use-package-always-ensure t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq
 inhibit-splash-screen t
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 initial-scratch-message "")

(set-frame-font "Knack Nerd Font 10")

;; backup and temp files settings
(setq backup-by-copying t
      version-control t
      kept-old-versions 2
      kept-new-versions 6
      delete-old-versions t)
(setq backup-directory-alist
      `(("." . ,temporary-file-directory)))
(setq-default tramp-backup-directory-alist backup-directory-alist)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(setq-default
 indent-tabs-mode nil
 bidi-display-reordering nil
 show-trailing-whitespace t
 fill-column 75
 require-final-newline t
 select-enable-primary t
 select-enable-clipboard t
 cursor-type 'bar
 cursor-in-non-selected-windows 'outline)

(global-subword-mode t)
(global-hl-line-mode t)
(column-number-mode t)
(show-paren-mode t)
(electric-pair-mode t)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;; keep quiet
(defun my-bell-function () "Do nothing.")
(setq ring-bell-function 'my-bell-function)
(setq visible-bell nil)

;; enable commands disabled by default
(put 'narrow-to-region 'disabled nil)          ; C-x n n
(put 'narrow-to-page 'disabled nil)            ; C-x n p
(put 'scroll-left 'disabled nil)               ; C-x > or <
(put 'downcase-region 'disabled nil)           ; C-x C-l
(put 'upcase-region 'disabled nil)             ; C-x C-u
(put 'set-goal-column 'disabled nil)           ; C-x C-n ==> disable with C-u
(put 'dired-find-alternate-file 'disabled nil) ; a in dired


;; Packages configuration
; init: before package loading
; config: after package loading

(use-package helm
  :config
  (helm-mode 1)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)))

(use-package which-key
  :config
  (which-key-mode))

(use-package flycheck-pos-tip
  :init
  (global-flycheck-mode)
  :config
  (eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

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

(use-package frames-only-mode
  :config
  (frames-only-mode))

(use-package geiser)

(use-package company
  :config
  (add-to-list 'company-backends 'company-irony)
  (global-company-mode)
  (setq company-minimum-prefix-length 4)
  (global-unset-key (kbd "M-/"))
  (define-key company-mode-map (kbd "C-;") 'helm-company)
  (define-key company-mode-map (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "C-;") 'helm-company)
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort))

(use-package irony
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))

  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package irony-eldoc
  :config
  (eval-after-load 'irony-mode
    (add-hook 'irony-mode-hook 'irony-eldoc)))

(use-package flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package rtags
  :config
  (setq rtags-display-result-backend 'helm))

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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/gtd.org")))
 '(package-selected-packages
   (quote
    (company-rtags flycheck-rtags helm-rtags rtags flycheck-ledger ledger-mode helm-company company-irony company-irony-c-headers company async flycheck-irony irony-eldoc irony irony-mode auto-complete geiser solarized-theme which-key use-package frames-only-mode flycheck-ocaml web-mode spaceline sass-mode ruby-compilation rhtml-mode popup org-bullets log4e linum-relative jump js2-mode ht gntp flycheck-pos-tip evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
