;;; -*- lexical-binding: t -*-

;;; ui
(add-to-list 'default-frame-alist '(fullscreen . fullboth))
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function #'ignore)

(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      scroll-conservatively 101
      scroll-margin 3)

(when (member "Iosevka" (font-family-list))
  (set-face-attribute 'default nil :font "Iosevka" :height 210))

(setq-default cursor-type 'box)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(dolist (hook '(term-mode-hook vterm-mode-hook shell-mode-hook
			       eshell-mode-hook compilation-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode 0))))

;;; packages
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(when (< emacs-major-version 27) (package-initialize))
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file))

;;; mac modifier
(when (eq system-type 'darwin)
  (setq ns-option-modifier 'none
        ns-command-modifier 'meta))

;;; persistence
(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)

;;; evil
(use-package evil
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq evil-normal-state-cursor 'box
        evil-insert-state-cursor 'box))

(use-package evil-collection :after evil :config (evil-collection-init))
(use-package evil-commentary :after evil :config (evil-commentary-mode))

(defun save-and-kill-this-buffer ()
  (interactive)
  (save-buffer)
  (kill-current-buffer))

(evil-ex-define-cmd "wq" #'save-and-kill-this-buffer)

;;; theme
(use-package gruber-darker-theme
  :config (load-theme 'gruber-darker t))

;;; keys
(global-set-key (kbd "M-=") #'text-scale-increase)
(global-set-key (kbd "M--") #'text-scale-decrease)
(global-set-key (kbd "M-0") (lambda () (interactive) (text-scale-set 0)))

(global-set-key (kbd "M-d") #'dired-jump)
(global-set-key (kbd "M-'") #'vterm)
(global-set-key (kbd "M-e") #'find-file)

;;; requested shortcuts
(global-set-key (kbd "M-u") #'undo)              ;; undo
(global-set-key (kbd "M-b") #'consult-buffer)    ;; buffers
(global-set-key (kbd "M-k") #'compile)           ;; compile

;;; compile
(setq compile-command "")  ;; prevent default "make -k"

;;; esc to quit ui but NOT break evil insert
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "<escape>") 'keyboard-escape-quit)
  (define-key evil-visual-state-map (kbd "<escape>") 'keyboard-escape-quit))

;;; vterm, magit, dired
(use-package vterm :commands vterm)
(use-package magit :commands magit-status)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :config
  (require 'dired-x)
  (setq dired-omit-files "^\\(?:\\.?#\\|.*~$\\)")
  (add-hook 'dired-mode-hook #'dired-omit-mode))

;;; org
(use-package org
  :ensure nil
  :config
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-hide-leading-stars t
        org-ellipsis " ⤵"
        org-src-window-setup 'current-window)
  (add-hook 'org-mode-hook #'org-indent-mode))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("●" "◉" "○" "◆" "◇" "▣" "□")
        org-modern-hide-stars nil))

;;; formatting
(defun my/format-buffer ()
  (cond
   ;; elisp indentation
   ((memq major-mode '(emacs-lisp-mode lisp-mode))
    (indent-region (point-min) (point-max)))
   ;; go formatting
   ((eq major-mode 'go-mode)
    (gofmt-before-save))))

(defun my/enable-format-on-save ()
  (add-hook 'before-save-hook #'my/format-buffer nil t))

(add-hook 'prog-mode-hook #'my/enable-format-on-save)

;;; completion
(setq tab-always-indent 'complete
      completion-cycle-threshold 3
      completion-ignore-case t)

(use-package corfu
  :init (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0)
  (corfu-preselect-first t)
  :config
  (corfu-popupinfo-mode 1))

(use-package cape)

;;; minibuffer
(use-package vertico :init (vertico-mode))
(use-package orderless
  :custom
  (completion-styles '(orderless basic)))
(use-package marginalia :init (marginalia-mode))
(use-package consult)
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)))
(use-package embark-consult :after (embark consult))

;;; go support
(use-package go-mode
  :mode "\\.go\\'"
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'gofmt-before-save nil t))))
