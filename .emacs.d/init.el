;;; init.el --- Final Config -*- lexical-binding: t -*-

;; -----------------------------------------------------------------------------
;; 1. CORE & PERFORMANCE
;; -----------------------------------------------------------------------------

;; Make startup faster by reducing garbage collection frequency
(setq gc-cons-threshold (* 50 1000 1000))

;; Keep custom settings in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; -----------------------------------------------------------------------------
;; 2. PACKAGE MANAGEMENT
;; -----------------------------------------------------------------------------

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Automatically refresh package list if it's empty (fixes first-run errors)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Fix PATH on macOS (so Emacs sees things like node, go, cargo)
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; -----------------------------------------------------------------------------
;; 3. UI & VISUALS
;; -----------------------------------------------------------------------------

;; Clean UI (no bars)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t
      visible-bell nil
      ring-bell-function 'ignore)

;; Font & Line Numbers
(set-face-attribute 'default nil :font "Geist Mono" :height 160)
(column-number-mode)
(global-display-line-numbers-mode t)

;; Cursor: Force it to be a BOX in all cases
(setq-default cursor-type 'box)

;; Scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      scroll-conservatively 101
      scroll-margin 3)

;; MacOS Keys: Command = Meta, Option = Normal
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; Start Maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Theme
(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; -----------------------------------------------------------------------------
;; 4. EVIL MODE (VIM)
;; -----------------------------------------------------------------------------

(use-package undo-fu)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  ;; Force Box Cursor in ALL states (Insert, Normal, Visual)
  (setq evil-insert-state-cursor 'box)
  (setq evil-normal-state-cursor 'box)
  (setq evil-visual-state-cursor 'box))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Keybindings (Leader = SPACE)
(use-package general
  :config
  (general-create-definer my-leader-def
    :keymaps '(normal visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (my-leader-def
    "f"  '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find file")
    "fs" '(save-buffer :which-key "save file")

    "b"  '(:ignore t :which-key "buffers")
    "bb" '(consult-buffer :which-key "switch buffer")
    "bk" '(kill-current-buffer :which-key "kill buffer")

    "w"  '(:ignore t :which-key "window")
    "wl" '(evil-window-right :which-key "right")
    "wh" '(evil-window-left :which-key "left")
    "wk" '(evil-window-up :which-key "up")
    "wj" '(evil-window-down :which-key "down")
    "w/" '(split-window-right :which-key "v-split")
    "w-" '(split-window-below :which-key "h-split")
    "wd" '(delete-window :which-key "delete")

    "g"  '(:ignore t :which-key "git")
    "gs" '(magit-status :which-key "status")

    "e"  '(:ignore t :which-key "errors")
    "el" '(flymake-show-buffer-diagnostics :which-key "list")
    "en" '(flymake-goto-next-error :which-key "next")
    "ep" '(flymake-goto-prev-error :which-key "prev")))

;; Global Keys
(global-set-key (kbd "M-=") #'text-scale-increase)
(global-set-key (kbd "M--") #'text-scale-decrease)
(global-set-key (kbd "M-0") (lambda () (interactive) (text-scale-set 0)))
(global-set-key (kbd "M-d") #'dired-jump)
(global-set-key (kbd "M-e") #'find-file)
(global-set-key (kbd "C-c s") #'shell-command)

;; -----------------------------------------------------------------------------
;; 5. NAVIGATION & SEARCH
;; -----------------------------------------------------------------------------

(use-package vertico
  :init (vertico-mode))

(use-package savehist
  :init (savehist-mode))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)))

(use-package marginalia
  :init (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; -----------------------------------------------------------------------------
;; 6. DEVELOPMENT
;; -----------------------------------------------------------------------------

(use-package magit)

(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-quit-no-match 'separator))

(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (web-mode . eglot-ensure))
  :config
  (setq eglot-events-buffer-size 0))

;; Format on Save
(defun my/eglot-format-on-save ()
  (when (eglot-managed-p)
    (eglot-format-buffer)))

(add-hook 'before-save-hook #'my/eglot-format-on-save)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; -----------------------------------------------------------------------------
;; 7. LANGUAGES
;; -----------------------------------------------------------------------------

;; Web (JS/TS/HTML/CSS)
(use-package web-mode
  :mode ("\\.html\\'" "\\.css\\'" "\\.jsx\\'" "\\.tsx\\'" "\\.ts\\'")
  :config
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(use-package typescript-mode)

;; Rust
(use-package rust-mode
  :config (setq rust-format-on-save t))

;; Go
(use-package go-mode
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

;; C / C++
(setq-default c-basic-offset 4)

;; Assembly
(add-to-list 'auto-mode-alist '("\\.fasm\\'" . asm-mode))

;; Config Files
(use-package yaml-mode :mode "\\.ya?ml\\'")
(use-package toml-mode :mode "\\.toml\\'")
(use-package dockerfile-mode :mode "Dockerfile\\'")
(use-package terraform-mode :mode "\\.tf\\'")
(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :config (setq markdown-command "multimarkdown"))

(use-package docker-compose-mode
  :mode "docker-compose.*\\.ya?ml\\'")

(use-package cmake-font-lock
  :after cmake-mode
  :hook (cmake-mode . cmake-font-lock-activate))

;; -----------------------------------------------------------------------------
;; 8. MATH & SCIENCE
;; -----------------------------------------------------------------------------

;; LaTeX
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-PDF-mode t)
  (setq TeX-view-program-selection '((output-pdf "Open")))
  (setq TeX-view-program-list '(("Open" "open %o")))
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode))

;; Rocq / Coq
(use-package proof-general
  :init
  (setq proof-splash-enable nil))

(use-package company-coq
  :hook (coq-mode . company-coq-mode))

;; -----------------------------------------------------------------------------
;; 9. MISC
;; -----------------------------------------------------------------------------

(show-paren-mode 1)
(electric-pair-mode 1)
(setq compile-command "")

(provide 'init)
