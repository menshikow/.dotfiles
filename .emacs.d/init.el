;;; init.el --- final config -*- lexical-binding: t -*-

;; -----------------------------------------------------------------------------
;; core & performance
;; -----------------------------------------------------------------------------

;; make startup faster by reducing garbage collection frequency
(setq gc-cons-threshold (* 50 1000 1000))

;; keep custom settings in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; disable backup files (file~) and auto-save files (#file#)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; -----------------------------------------------------------------------------
;; package management
;; -----------------------------------------------------------------------------

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; -----------------------------------------------------------------------------
;; status bar
;; -----------------------------------------------------------------------------

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 30)
  (setq doom-modeline-bar-width 4)

  ;; kill all icons
  (setq doom-modeline-icon nil)

  ;; hide clutter
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-total-line-number nil))

;; -----------------------------------------------------------------------------
;; ui & visuals
;; -----------------------------------------------------------------------------

;; clean ui
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

;; silence bell
(setq visible-bell nil
      ring-bell-function 'ignore)

;; font & relative line numbers
(set-face-attribute 'default nil :font "Source Code Pro" :height 160)
(column-number-mode)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; scrolling (modern & smooth)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; scroll one line at a time
      mouse-wheel-progressive-speed nil            ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't                  ;; scroll window under mouse
      scroll-step 1
      scroll-conservatively 101                    ;; value > 100 prevents recentering
      scroll-preserve-screen-position t
      scroll-margin 0)                             ;; margin > 0 causes jumps

;; pixel precision scrolling (Emacs 29+)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; macos keys
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; type y/n instead of yes/no
(setq use-short-answers t)

;; -----------------------------------------------------------------------------
;; themes (auto switching)
;; -----------------------------------------------------------------------------

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil)
  ;; ensure org headers look good
  (doom-themes-org-config))

(use-package theme-changer
  :ensure t
  :config

  (setq calendar-latitude 48.14)
  (setq calendar-longitude 11.58)

  ;; disable old themes before switching to avoid clashes
  (mapc #'disable-theme custom-enabled-themes)

  ;; (change-theme 'day_theme 'night_theme)
  (change-theme 'doom-homage-white 'doom-homage-black))

;; -----------------------------------------------------------------------------
;; evil mode
;; -----------------------------------------------------------------------------

(use-package undo-fu)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  (setq evil-normal-state-cursor 'box)
  (setq evil-visual-state-cursor 'box)
  (setq evil-insert-state-cursor 'box))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; multicursors
(use-package evil-multiedit
  :config
  (evil-multiedit-default-keybinds))

;; keybindings (leader = space)
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

    "c"  '(compile :which-key "compile")        ;; direct compile
    "r"  '(recompile :which-key "re-compile")   ;; quick re-run

    "d"  '(dired-jump :which-key "dired")

    "b"  '(:ignore t :which-key "buffers")
    "bb" '(consult-buffer :which-key "switch buffer")
    "bk" '(kill-current-buffer :which-key "kill buffer")
    "bm" '(view-echo-area-messages :which-key "log messages")

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
    "el" '(flymake-show-buffer-diagnostics :which-key "list all")
    "en" '(flymake-goto-next-error :which-key "next")
    "ep" '(flymake-goto-prev-error :which-key "prev")
    "ed" '(flymake-show-diagnostic :which-key "show detail")))

;; global keys
(global-set-key (kbd "M-=") #'text-scale-increase)
(global-set-key (kbd "M--") #'text-scale-decrease)
(global-set-key (kbd "M-0") (lambda () (interactive) (text-scale-set 0)))
(global-set-key (kbd "M-j") #'dired-jump)
(global-set-key (kbd "M-e") #'find-file)
(global-set-key (kbd "C-c s") #'shell-command)

;; -----------------------------------------------------------------------------
;; navigation & search
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
;; development
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

;; silence errors if formatting fails on save
(defun my/eglot-format-on-save ()
  (when (eglot-managed-p)
    (ignore-errors (eglot-format-buffer))))

(add-hook 'before-save-hook #'my/eglot-format-on-save)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; -----------------------------------------------------------------------------
;; languages
;; -----------------------------------------------------------------------------

;; --- Elisp Formatting ---
(defun my/indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun my/elisp-mode-hook ()
  ;; Auto-indent elisp files on save
  (add-hook 'before-save-hook #'my/indent-buffer nil t))

(add-hook 'emacs-lisp-mode-hook #'my/elisp-mode-hook)

;; --- Web ---
(use-package web-mode
  :mode ("\\.html\\'" "\\.css\\'" "\\.jsx\\'" "\\.tsx\\'" "\\.ts\\'")
  :config
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(use-package typescript-mode)

(use-package rust-mode
  :config (setq rust-format-on-save t))

(use-package go-mode
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(setq-default c-basic-offset 4)

(add-to-list 'auto-mode-alist '("\\.fasm\\'" . asm-mode))

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
;; math & science
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; math
;; -----------------------------------------------------------------------------

(use-package tex
  :ensure auctex
  :config

  ;; -- compiler & viewer --
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-PDF-mode t)

  ;; SyncTeX: C-c C-v jumps to the PDF.
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex)

  ;; Use "open" on Mac
  (setq TeX-view-program-selection '((output-pdf "Open")))
  (setq TeX-view-program-list '(("Open" "open %o")))

  ;; hooks
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)  ;; Word wrap
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)   ;; Math keys
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)    ;; Enable RefTeX
  (add-hook 'LaTeX-mode-hook 'flyspell-mode))    ;; Spell check

;; RefTeX
(use-package reftex
  :ensure t
  :config
  (setq reftex-plug-into-AUCTeX t))

;; -----------------------------------------------------------------------------
;; misc
;; -----------------------------------------------------------------------------

(electric-pair-mode -1)
(show-paren-mode 1)
(setq compile-command "")
