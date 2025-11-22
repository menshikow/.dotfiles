;;; -*- lexical-binding: t; -*-

;;; ------------------------------------------------------------
;;; platform detection
;;; ------------------------------------------------------------

(defconst my/is-mac (eq system-type 'darwin))
(defconst my/is-linux (eq system-type 'gnu/linux))

;;; ------------------------------------------------------------
;;; ui
;;; ------------------------------------------------------------

(add-to-list 'default-frame-alist '(fullscreen . fullboth))
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      scroll-conservatively 101
      scroll-margin 3)

;; font - fallback if iosevka not available
(when (member "Iosevka" (font-family-list))
  (set-face-attribute 'default nil :font "Iosevka" :height 220))

(setq-default cursor-type 'box)

;; line numbers in code, but not in terminals and compilation
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(dolist (hook '(term-mode-hook vterm-mode-hook shell-mode-hook
                eshell-mode-hook compilation-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode 0))))

(fset 'yes-or-no-p 'y-or-n-p)

;; disable all bells
(setq visible-bell nil
      ring-bell-function 'ignore)

;;; ------------------------------------------------------------
;;; package system + use-package
;;; ------------------------------------------------------------

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(when (< emacs-major-version 27)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; keep custom.el separate
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;; ------------------------------------------------------------
;;; macos settings
;;; ------------------------------------------------------------

(when my/is-mac
  ;; modifier keys
  (setq ns-option-modifier 'none
        ns-command-modifier 'meta)
  
  ;; fix PATH from shell
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

;;; ------------------------------------------------------------
;;; nixos settings
;;; ------------------------------------------------------------

(when my/is-linux
  ;; use super key as meta
  (setq x-super-keysym 'meta)
  
  ;; better font rendering on some Linux systems
  (setq-default line-spacing 0.1)

  ;; FIX for NixOS: Inherit PATH from the shell.
  ;; Essential for finding executables (like language servers) 
  ;; when using a declarative package manager like Nix.
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

;;; ------------------------------------------------------------
;;; persistence
;;; ------------------------------------------------------------

(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)

;; disable backup files
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

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
  (setq evil-collection-mode-list
        '(magit dired vterm compile))
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

;; escape quits prompts
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "<escape>") #'keyboard-escape-quit)
  (define-key evil-visual-state-map (kbd "<escape>") #'keyboard-escape-quit))

;;; ------------------------------------------------------------
;;; undo-tree
;;; ------------------------------------------------------------

(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
  :config
  (setq undo-tree-auto-save-history nil
        undo-tree-history-directory-alist `(("." . ,(locate-user-emacs-file "undo-tree-history"))))
  (evil-set-undo-system 'undo-tree))

;;; ------------------------------------------------------------
;;; theme
;;; ------------------------------------------------------------

(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker t))

;;; ------------------------------------------------------------
;;; projectile
;;; ------------------------------------------------------------

(use-package projectile
  :init
  (projectile-mode 1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-project-search-path '("~/projects/" "~/work/")
        projectile-completion-system 'default))

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
(global-set-key (kbd "M-u") #'undo-tree-undo)   ;; undo with undo-tree
(global-set-key (kbd "M-U") #'undo-tree-redo)   ;; redo
(global-set-key (kbd "M-b") #'consult-buffer)   ;; buffers
(global-set-key (kbd "M-c") #'compile)         ;; compile
(global-set-key (kbd "C-c s") #'shell-command) ;; shell command

;; buffer management
(global-set-key (kbd "M-w") #'kill-current-buffer)     ;; quick kill buffer
(global-set-key (kbd "M-W") #'kill-buffer-and-window)  ;; kill buffer and window

(setq compile-command "") ;; no default "make -k"

;;; ------------------------------------------------------------
;;; tools: vterm, magit, dired
;;; ------------------------------------------------------------

(use-package vterm
  :if (or my/is-mac my/is-linux)
  :defer t
  :commands vterm
  :config
  ;; better scrollback
  (setq vterm-max-scrollback 10000))

(use-package magit
  :defer t
  :commands magit-status)

(use-package dired
  :ensure nil
  :defer t
  :commands (dired dired-jump)
  :config
  (require 'dired-x)
  (setq dired-omit-files "^\\(?:\\.?#\\|.*~$\\)")
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  
  ;; use gnu ls on mac if available for better sorting
  (when my/is-mac
    (let ((gls (executable-find "gls")))
      (when gls
        (setq insert-directory-program gls)))))

;;; ------------------------------------------------------------
;;; org mode
;;; ------------------------------------------------------------

(use-package org
  :ensure nil
  :defer t
  :config
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-hide-leading-stars t
        org-ellipsis " ⤵"
        org-src-window-setup 'current-window)
  (add-hook 'org-mode-hook #'org-indent-mode))

(use-package org-modern
  :defer t
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
  :defer 0.1
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-preselect-first t)
  :config
  (corfu-popupinfo-mode 1))

(use-package cape
  :defer t
  :config
  ;; add useful completion sources
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;;; ------------------------------------------------------------
;;; minibuffer completion: vertico, orderless, marginalia, consult, embark
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

(use-package consult
  :defer t
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("M-g g" . consult-goto-line)
         ("M-s l" . consult-line)
         ("M-s g" . consult-ripgrep)  ;; project-wide search (requires ripgrep)
         ("M-s f" . consult-find)))   ;; find files

(use-package embark
  :defer t
  :bind (("C-." . embark-act)
         ("C-," . embark-dwim))) 

(use-package embark-consult
  :defer t
  :after (embark consult))

;;; ------------------------------------------------------------
;;; evil-multiedit
;;; ------------------------------------------------------------

(use-package iedit
  :defer t)

(use-package evil-multiedit
  :after (evil iedit)
  :config
  (evil-multiedit-default-keybinds))

;;; ------------------------------------------------------------
;;; eglot + format-on-save
;;; ------------------------------------------------------------

(use-package eglot
  :defer t
  :hook ((python-mode c-mode c++-mode go-mode rust-mode haskell-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  
  ;; platform-specific server paths if needed
  ;; Note: The exec-path-from-shell fix above often makes manual paths unnecessary on NixOS.
  (when my/is-linux
    ;; example: specific paths for Linux
    ;; (add-to-list 'eglot-server-programs
    ;;      '(python-mode . ("pylsp")))
    ))

;;; formatting
(defun my/format-buffer ()
  (if (eglot-managed-p)
      (ignore-errors (eglot-format))
    (cond
      ((eq major-mode 'go-mode) (gofmt))
      ((memq major-mode '(emacs-lisp-mode lisp-mode))
       (indent-region (point-min) (point-max))))))

(defun my/enable-format-on-save ()
  (add-hook 'before-save-hook #'my/format-buffer nil t))

(add-hook 'prog-mode-hook #'my/enable-format-on-save)

;;; ------------------------------------------------------------
;;; language modes
;;; ------------------------------------------------------------

;; go
(use-package go-mode
  :defer t
  :mode "\\.go\\'"
  :config
  ;; set GOPATH if needed (may be unnecessary with exec-path-from-shell)
  (when my/is-linux
    (setenv "GOPATH" (expand-file-name "~/go"))))

;; rust
(use-package rust-mode
  :defer t
  :mode "\\.rs\\'")

;; haskell
(use-package haskell-mode
  :defer t
  :mode "\\.hs\\'"
  :hook (haskell-mode . interactive-haskell-mode)
  :config
  (setq haskell-indentation-layout-offset 4
    haskell-indentation-left-offset 4
    haskell-indentation-starter-offset 4))

;;; ------------------------------------------------------------
;;; performance tweaks
;;; ------------------------------------------------------------

;; increase gc threshold for better performance
(setq gc-cons-threshold (* 50 1000 1000))

;; reduce startup time
(setq package-enable-at-startup nil)

(provide 'init)
