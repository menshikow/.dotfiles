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

;; font - iosevka
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
;;; ubuntu settings
;;; ------------------------------------------------------------

(when my/is-linux
  ;; use alt as meta
  (setq x-alt-keysym 'meta
        x-super-keysym 'super)
  
  ;; better font rendering on Ubuntu
  (setq-default line-spacing 0.1)

  ;; inherit PATH from shell for GUI Emacs
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
        evil-want-keybinding nil
        evil-undo-system 'undo-redo)
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
;;; undo system
;;; ------------------------------------------------------------

;; built-in undo-redo for Emacs 28+
(when (>= emacs-major-version 28)
  (global-set-key (kbd "C-?") #'undo-redo)
  (with-eval-after-load 'evil
    (evil-set-undo-system 'undo-redo)))

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
(global-set-key (kbd "M-u") #'undo)
(global-set-key (kbd "M-U") #'undo-redo)
(global-set-key (kbd "M-b") #'consult-buffer)
(global-set-key (kbd "M-c") #'compile)
(global-set-key (kbd "C-c s") #'shell-command)

;; buffer management
(global-set-key (kbd "M-w") #'kill-current-buffer)
(global-set-key (kbd "M-W") #'kill-buffer-and-window)

(setq compile-command "")

;;; ------------------------------------------------------------
;;; tools: vterm, magit, dired
;;; ------------------------------------------------------------

(use-package vterm
  :if (or my/is-mac my/is-linux)
  :defer t
  :commands vterm
  :config
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
  
  ;; use gnu ls on mac if available
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
         ("M-s g" . consult-ripgrep)
         ("M-s f" . consult-find)))

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
;;; eglot + formatters
;;; ------------------------------------------------------------

(use-package eglot
  :defer t
  :hook ((python-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (elixir-mode . eglot-ensure)
         (zig-mode . eglot-ensure))
  :config
  ;; language server configurations
  (setq-default eglot-workspace-configuration
                '(:pylsp (:plugins (:pycodestyle (:enabled :json-false)
                                    :mccabe (:enabled :json-false)
                                    :pyflakes (:enabled :json-false)
                                    :autopep8 (:enabled :json-false)
                                    :yapf (:enabled :json-false)))))
  
  ;; server programs
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pylsp" "-v")))
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs
               '(go-mode . ("gopls")))
  (add-to-list 'eglot-server-programs
               '(rust-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs
               '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  (add-to-list 'eglot-server-programs
               '((js-mode typescript-mode) . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(elixir-mode . ("elixir-ls")))
  (add-to-list 'eglot-server-programs
               '(zig-mode . ("zls")))
  
  (setq eglot-connect-timeout 30
        eglot-events-buffer-size 0)) ;; disable events buffer for performance

;; format on save
(defun my/format-buffer ()
  "Format buffer using eglot if available."
  (when (and (eglot-managed-p)
             (eglot--server-capable :documentFormattingProvider))
    (eglot-format-buffer)))

(add-hook 'before-save-hook #'my/format-buffer)

;;; ------------------------------------------------------------
;;; systems programming: c, c++, assembly, zig, cmake, make
;;; ------------------------------------------------------------

;; c/c++ settings
(setq c-default-style "linux"
      c-basic-offset 4)

;; cmake
(use-package cmake-mode
  :defer t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; modern cmake font-lock
(use-package cmake-font-lock
  :defer t
  :after cmake-mode
  :hook (cmake-mode . cmake-font-lock-activate))

;; makefile mode (built-in)
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq-local tab-width 8)
            (setq-local indent-tabs-mode t)))

;; nasm assembly
(use-package nasm-mode
  :defer t
  :mode ("\\.asm\\'" "\\.s\\'" "\\.nasm\\'"))

;; zig - modern systems language
(use-package zig-mode
  :defer t
  :mode "\\.zig\\'")

;; compilation buffer improvements
(use-package ansi-color
  :ensure nil
  :config
  (defun my/colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook #'my/colorize-compilation-buffer))

;; gdb multi-window layout
(setq gdb-many-windows t
      gdb-show-main t)

;;; ------------------------------------------------------------
;;; frontend: javascript, typescript, jsx, tsx, json, css, html
;;; ------------------------------------------------------------

;; javascript
(use-package js2-mode
  :defer t
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2
        js-indent-level 2))

;; typescript
(use-package typescript-mode
  :defer t
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

;; tsx support with tree-sitter (Emacs 29+)
(when (>= emacs-major-version 29)
  (use-package tsx-ts-mode
    :ensure nil
    :mode "\\.tsx\\'"
    :hook (tsx-ts-mode . eglot-ensure)))

;; web-mode for jsx, html, templates
(use-package web-mode
  :defer t
  :mode ("\\.jsx\\'"
         "\\.html\\'"
         "\\.php\\'"
         "\\.vue\\'"
         "\\.svelte\\'")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t))

;; json
(use-package json-mode
  :defer t
  :mode "\\.json\\'")

;; css/scss
(use-package css-mode
  :ensure nil
  :defer t
  :mode ("\\.css\\'" "\\.scss\\'")
  :config
  (setq css-indent-offset 2))

;; emmet for html/css expansion
(use-package emmet-mode
  :defer t
  :hook ((web-mode . emmet-mode)
         (html-mode . emmet-mode)
         (css-mode . emmet-mode)))

;; prettier integration via apheleia (better than prettier-js)
(use-package apheleia
  :defer t
  :hook ((js2-mode . apheleia-mode)
         (typescript-mode . apheleia-mode)
         (tsx-ts-mode . apheleia-mode)
         (web-mode . apheleia-mode)
         (json-mode . apheleia-mode)
         (css-mode . apheleia-mode))
  :config
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath)))

;;; ------------------------------------------------------------
;;; devops: yaml, toml, dockerfile, terraform, ansible
;;; ------------------------------------------------------------

;; yaml
(use-package yaml-mode
  :defer t
  :mode "\\.ya?ml\\'")

;; toml
(use-package toml-mode
  :defer t
  :mode "\\.toml\\'")

;; dockerfile
(use-package dockerfile-mode
  :defer t
  :mode "Dockerfile\\'")

;; terraform
(use-package terraform-mode
  :defer t
  :mode "\\.tf\\'")

;; markdown
(use-package markdown-mode
  :defer t
  :mode ("\\.md\\'" "\\.markdown\\'")
  :config
  (setq markdown-command "multimarkdown"))

;; ansible (yaml with specific patterns)
(use-package ansible
  :defer t
  :hook ((yaml-mode . (lambda ()
                        (when (and (buffer-file-name)
                                   (string-match-p "ansible" (buffer-file-name)))
                          (ansible-mode))))))

;; docker-compose
(use-package docker-compose-mode
  :defer t
  :mode "docker-compose.*\\.ya?ml\\'")

;; systemd unit files
(use-package systemd
  :defer t
  :mode ("\\.service\\'" "\\.timer\\'" "\\.target\\'" 
         "\\.mount\\'" "\\.socket\\'" "\\.path\\'"))

;;; ------------------------------------------------------------
;;; backend: elixir, python, go, rust, haskell
;;; ------------------------------------------------------------

;; python
(use-package python
  :ensure nil
  :defer t
  :config
  (setq python-indent-guess-indent-offset nil
        python-indent-offset 4))

;; elixir
(use-package elixir-mode
  :defer t
  :mode "\\.exs?\\'")

;; go
(use-package go-mode
  :defer t
  :mode "\\.go\\'"
  :config
  (setq gofmt-command "goimports")
  (when my/is-linux
    (setenv "GOPATH" (expand-file-name "~/go"))))

;; rust
(use-package rust-mode
  :defer t
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save nil)) ;; let eglot handle it

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
;;; additional utilities
;;; ------------------------------------------------------------

;; flycheck for additional linting (works alongside eglot)
(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-indication-mode 'left-fringe))

;;; ------------------------------------------------------------
;;; performance tweaks
;;; ------------------------------------------------------------

;; increase gc threshold for better performance
(setq gc-cons-threshold (* 50 1000 1000))

;; reduce startup time
(setq package-enable-at-startup nil)

;; increase the amount of data emacs reads from processes
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; use a larger compilation buffer
(setq compilation-scroll-output t)

(provide 'init)
