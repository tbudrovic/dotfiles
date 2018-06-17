;;; init.el --- starting point of Emacs configuration
;;; Commentary:
;;; this is here to silence flycheck

;;; Code:

(setq-default tls-checktrust t)
(let '(trustfile "/etc/ssl/certs/ca-certificates.crt")
  (setq-default tls-program
                '(format "gnutls-cli --x509-cafile %s -p %%p %%h"
                         trustfile))
  (setq-default gnutls-verify-error t)
  (setq-default gnutls-trustfiles (list trustfile)))

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))

(require 'use-package)
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

;;(set-frame-font "-unknown-Hack Nerd Font Mono-normal-normal-normal-*-14-*-*-*-*-0-iso10646-1" t)

;; backup and temp files settings
(setq
 backup-by-copying t
 version-control t
 kept-old-versions 2
 kept-new-versions 6
 delete-old-versions t
 backup-directory-alist
 `(("." . ,temporary-file-directory)))
(setq-default tramp-backup-directory-alist backup-directory-alist)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; misc settings
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

;; built-in modes settings
(global-subword-mode t)
(global-hl-line-mode t)
(global-eldoc-mode t)
(column-number-mode t)
(show-paren-mode t)
(electric-pair-mode t)
(setq vc-handled-backends nil)

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

(use-package ag)

(use-package ivy
  :diminish (ivy-mode)
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t
        ivy-height 20
        ivy-count-format "%d/%d "
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

(use-package swiper
  :bind
  ("C-s" . swiper))

(use-package counsel
  :demand t
  :diminish (counsel-mode)
  :config
  (counsel-mode))
  ;; :bind
  ;; ("C-; g f" . counsel-git)
  ;; ("C-; a" . counsel-ag))

(use-package which-key
  :config
  (which-key-mode))

(use-package evil
  :config
  (evil-mode))

(use-package linum-relative
  :config
  (setq linum-relative-current-symbol "")
  (global-linum-mode)
  (linum-relative-mode))

(use-package geiser)

(use-package company
  :demand t
  :diminish (company-mode)
  :config
  (setq company-minimum-prefix-length 4)
  (global-company-mode)
  :bind (("M-/" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next-or-abort)
         ("C-p" . company-select-previous-or-abort)))

(use-package flycheck
  :diminish (flycheck-mode)
  :config (global-flycheck-mode))

(use-package flycheck-pos-tip
  :after (flycheck)
  :config (flycheck-pos-tip-mode))

(use-package nord-theme
  :config (load-theme 'nord 'no-confirm))

(use-package python
  :commands (python-mode)
  :mode ("\\.py\\'" . python-mode)
  :config (setq python-shell-interpreter "python3"))

(use-package cmake-mode
  :defer t)

(use-package projectile
  :bind-keymap
  ("C-; p" . projectile-command-map))

(use-package magit
  :bind
  ("C-; g s" . magit-status))

(use-package org-plus-contrib
  :commands (org-mode org-agenda)
  :bind ("C-; o a" . org-agenda)
  :defines (org-agenda-files)
  :init (setq org-agenda-files '("~/win/plans/")))

(use-package org-bullets
  :after (org-mode)
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode t))))

(use-package ledger-mode
  :defer t)

(use-package flycheck-ledger
  :after (ledger-mode))

(use-package lsp-mode
  :defer t)

(use-package lsp-ui
  :after (lsp-mode)
  :config (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company-lsp
  :after (lsp-mode)
  :config (push 'company-lsp company-backends))

(use-package lsp-python
  :after (python)
  :config (add-hook 'python-mode-hook 'lsp-python-enable))

(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :defines
  (mu4e-compose-format-flowed
   mu4e-compose-in-new-frame
   mu4e-maildir
   mu4e-headers-date-format
   mu4e-view-show-addresses
   mu4e-headers-include-related
   mu4e-sent-messages-behavior
   mu4e-html2text-command
   mu4e-sent-folder
   mu4e-drafts-folder
   mu4e-trash-folder
   mu4e-refile-folder
   mu4e-user-email-address-list
   mu4e-get-mail-command)
  :config
  (setq
   mu4e-compose-format-flowed t
   mu4e-compose-in-new-frame t
   mu4e-maildir "~/Mail"
   mu4e-headers-date-format "%Y-%m-%d %H:%M"
   mu4e-view-show-addresses t
   message-kill-buffer-on-exit t
   mu4e-headers-include-related t
   mu4e-sent-messages-behavior 'delete
   mu4e-html2text-command "html2text"
   mu4e-sent-folder   "/Sent"
   mu4e-drafts-folder "/Drafts"
   mu4e-trash-folder  "/Trash"
   mu4e-refile-folder "/Archive"
   mu4e-user-email-address-list '("")
   mu4e-get-mail-command "offlineimap -o"
   message-send-mail-function 'message-send-mail-with-sendmail
   user-mail-address ""
   user-full-name ""))


(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (mu4e ag apel w3m which-key use-package projectile org-plus-contrib org-bullets nord-theme magit lsp-ui lsp-python linum-relative ledger-mode geiser frames-only-mode flycheck-pos-tip flycheck-ledger evil diminish counsel company-lsp cmake-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
