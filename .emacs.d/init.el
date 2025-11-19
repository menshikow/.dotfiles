;;; -*- lexical-binding: t; -*-

;;; ------------------------------------------------------------
;;; ui
;;; ------------------------------------------------------------

(add-to-list 'default-frame-alist '(fullscreen . fullboth))
;; (setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      scroll-conservatively 101
      scroll-margin 3)

;; font
(when (member "Iosevka" (font-family-list))
  (set-face-attribute 'default nil :font "Iosevka" :height 220))

(setq-default cursor-type 'box)

;; line numbers in code, but not in terminals and compilation
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

(dolist (hook '(term-mode-hook vterm-mode-hook shell-mode-hook
			       eshell-mode-hook compilation-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode 0))))

(fset 'yes-or-no-p 'y-or-n-p)

;;; ------------------------------------------------------------
;;; package system + use-package
;;; ------------------------------------------------------------

(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(when (< emacs-major-version 27)
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Keep custom.el separate
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;; ------------------------------------------------------------
;;; macOS modifier keys
;;; ------------------------------------------------------------

(when (eq system-type 'darwin)
  (setq ns-option-modifier 'none
        ns-command-modifier 'meta))

;;; ------------------------------------------------------------
;;; persistence
;;; ------------------------------------------------------------

(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)

;;; ------------------------------------------------------------
;;; evil mode 
;;; ------------------------------------------------------------

(use-package evil
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq evil-normal-state-cursor 'box
        evil-insert-state-cursor 'box))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(defun my/save-and-kill-this-buffer ()
  (interactive)
  (save-buffer)
  (kill-current-buffer))

(evil-ex-define-cmd "wq" #'my/save-and-kill-this-buffer)

;; escape quits prompts works not like shit  but doesn't mess with evil states
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "<escape>") #'keyboard-escape-quit)
  (define-key evil-visual-state-map (kbd "<escape>") #'keyboard-escape-quit))

;;; ------------------------------------------------------------
;;; theme
;;; ------------------------------------------------------------

(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker t))

;;; ------------------------------------------------------------
;;; keybindings
;;; ------------------------------------------------------------

(global-set-key (kbd "M-=") #'text-scale-increase)
(global-set-key (kbd "M--") #'text-scale-decrease)
(global-set-key (kbd "M-0")
                (lambda () (interactive) (text-scale-set 0)))

(global-set-key (kbd "M-d") #'dired-jump)
(global-set-key (kbd "M-'") #'vterm)
(global-set-key (kbd "M-e") #'find-file)

;; shortcuts
(global-set-key (kbd "M-u") #'undo)              ;; undo
(global-set-key (kbd "M-b") #'consult-buffer)    ;; buffers
(global-set-key (kbd "M-k") #'compile)           ;; compile

(setq compile-command "") ;; no default shit "make -k"

;;; ------------------------------------------------------------
;;; tools: vterm, magit, dired
;;; ------------------------------------------------------------

(use-package vterm
  :commands vterm)

(use-package magit
  :commands magit-status)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :config
  (require 'dired-x)
  (setq dired-omit-files "^\\(?:\\.?#\\|.*~$\\)")
  (add-hook 'dired-mode-hook #'dired-omit-mode))

;;; ------------------------------------------------------------
;;; org mode
;;; ------------------------------------------------------------

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

;;; ------------------------------------------------------------
;;; corfu
;;; ------------------------------------------------------------

(setq tab-always-indent 'complete
      completion-cycle-threshold 3
      completion-ignore-case t)

(use-package corfu
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0)
  (corfu-preselect-first t)
  :config
  (corfu-popupinfo-mode 1))

(use-package cape)

;;; ------------------------------------------------------------
;;; minibuffer completion: Vertico, Orderless, Marginalia, Consult, Embark
;;; ------------------------------------------------------------

(use-package vertico
  :init
  (vertico-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package consult)

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)))

(use-package embark-consult
  :after (embark consult))

;;; ------------------------------------------------------------
;;; eglot + format-on-save
;;; ------------------------------------------------------------

(use-package eglot
  :hook ((python-mode
          c-mode
          c++-mode
          go-mode
          rust-mode) . eglot-ensure)
  :config
  ;; clangd config
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode)
                 . ("clangd" "--header-insertion=never"))))

(defun my/format-buffer ()
  "Format buffer using Eglot if available; fall back where needed."
  (interactive)
  (cond
   ;; if this buffer is managed by Eglot, use LSP formatting
   ((and (bound-and-true-p eglot-managed-p)
         (eglot-managed-p))
    (eglot-format))
   ;; language-specific fallbacks
   ((eq major-mode 'go-mode)
    (gofmt))
   ((memq major-mode '(emacs-lisp-mode lisp-mode))
    (indent-region (point-min) (point-max)))))

(defun my/enable-format-on-save ()
  (add-hook 'before-save-hook #'my/format-buffer nil t))

(add-hook 'prog-mode-hook #'my/enable-format-on-save)

;;; ------------------------------------------------------------
;;; language modes
;;; ------------------------------------------------------------

;; Python
(add-hook 'python-mode-hook #'eglot-ensure)

;; C / C++
(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)

;; Go
(use-package go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . eglot-ensure)))

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . eglot-ensure))



(provide 'init)

