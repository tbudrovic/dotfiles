;; package managers

;;; code
(require 'package)

(setq package-archives nil)

(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; load packages info if there is no cache
(setq init-dir (file-name-directory (or load-file-name (buffer-file-name))))
(unless (and (file-exists-p (concat init-dir "elpa/archives/gnu"))
             (file-exists-p (concat init-dir "elpa/archives/melpa")))
  (package-refresh-contents))

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

(set-frame-font "Hack 10")

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
(setq-default show-trailing-whitespace t)
(setq-default fill-column 80)
(setq-default c-subword-mode t)
(global-subword-mode)

(setq-default cursor-type 'box)
(setq-default cursor-in-non-selected-windows 'bar)

(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;;; keep quiet
(defun my-bell-function ())

(setq ring-bell-function 'my-bell-function)
(setq visible-bell nil)

(setq-default ido-enable-flex-matching t)
(setq-default ido-everywhere t)
(setq-default ido-create-new-buffer 'always)
(ido-mode 1)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package flycheck-pos-tip
  :ensure t
  :config
  (global-flycheck-mode)
  (flycheck-pos-tip-mode))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package web-mode
  :ensure t
  :mode "\\.erb\\'"
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing nil))

(use-package ruby-mode
  :ensure t
  :mode ("Capfile" "Gemfile" "Rakefile" "\\.rake\\'" "\\.rb\\'")
  :config
  (setq ruby-deep-arglist t)
  (setq ruby-deep-indent-paren nil)
  (setq-default c-tab-always-indent nil))


(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package linum-relative
  :ensure t
  :config
  (setq linum-relative-current-symbol "")
  (global-linum-mode t)
  (linum-relative-mode))

(use-package ace-jump-mode
  :ensure t
  :config
  (define-key global-map (kbd "C-c SPC") 'ace-jump-mode))

(use-package rust-mode
  :ensure t)

(use-package frames-only-mode
  :ensure t
  :config
  (frames-only-mode))

;; (use-package sr-speedbar
;   :ensure t
;;   :init
;;   (setq speedbar-use-images nil)
;;   (setq sr-speedbar-right-side nil)
;;   :config
;;   (sr-speedbar-open))

(use-package solarized
  :disabled t
  :if (window-system)
  :defer t
  :init (load-theme 'solarized-dark 'no-confirm)
  :config
  (progn
    ;; As of 20140313: avoid underlining the modeline
    (set-face-attribute 'mode-line nil :underline nil)
    (set-face-attribute 'mode-line-inactive nil :underline nil)

    ;; Nicer trailing whitespace indication
    (set-face-attribute 'trailing-whitespace nil
			:background
			(face-attribute 'warning :foreground)))

  ;; Fix solarized linum background
  (add-hook 'linum-before-numbering-hook
	    (lambda ()
	      (set-face-attribute
	       'linum nil :background
	       (face-attribute 'header-line  :background)))))

(provide 'init)

;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(package-selected-packages
   (quote
    (ace-jump-mode which-key use-package frames-only-mode rust-mode flycheck-ocaml web-mode spaceline sass-mode ruby-compilation rhtml-mode popup org-bullets log4e linum-relative jump js2-mode ht gntp flymake-jslint flymake-jshint flycheck-pos-tip evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
