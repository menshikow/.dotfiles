
;;; init.el --- emacs config *- lexical-binding: t -*-

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

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

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
(set-face-attribute 'default nil :font "Iosevka Nerd Font Mono" :height 170)
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

;; pixel precision scrolling (emacs 29+)
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
;; file tree
;; -----------------------------------------------------------------------------

(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-width 30
        ;; disable icons: force text-based rendering
        treemacs-no-png-images t)

  ;; follow the cursor in the file tree
  (treemacs-follow-mode t)
  ;; update the tree when files change on disk
  (treemacs-filewatch-mode t)
  ;; show git status colors in the tree
  (treemacs-git-mode 'deferred)

  ;; bind ret to visit-and-close
  (with-eval-after-load 'treemacs
    (define-key treemacs-mode-map (kbd "RET") #'treemacs-visit-node-close-treemacs)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; -----------------------------------------------------------------------------
;; theme
;; -----------------------------------------------------------------------------

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil)

  (load-theme 'doom-opera t)

  ;; corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

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
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (my-leader-def
    "f"  '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find file")
    "fs" '(save-buffer :which-key "save file")
    "ft" '(treemacs :which-key "toggle tree")

    "c"  '(compile :which-key "compile")
    "r"  '(recompile :which-key "re-compile")

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
    "ed" '(flymake-show-diagnostic :which-key "show detail")

    "a" '(org-agenda :which-key "agenda")))

;; global keys
(global-set-key (kbd "M-=") #'text-scale-adjust)
(global-set-key (kbd "M-+") #'text-scale-increase)
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
;; org mode (modernized)
;; -----------------------------------------------------------------------------

(use-package org
  :hook ((org-mode . visual-line-mode)  ;; wrap lines at screen edge
         (org-mode . org-indent-mode))  ;; indent text according to outline
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t))   ;; hide *bold* markers

;; modern look for org (bullets, tables, etc.)
(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-mode . org-modern-agenda))
  :config
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka Nerd Font Mono"))

;; -----------------------------------------------------------------------------
;; org agenda
;; -----------------------------------------------------------------------------

;; 1. set the location of your notes
;;    (make sure you create this folder: mkdir ~/org)
(setq org-directory "~/org/")

;; 2. tell agenda to look at every .org file in that directory
(setq org-agenda-files (list "~/org/"))

;; 3. better defaults
(setq org-agenda-start-on-weekday 1)    ;; start agenda on monday
(setq org-agenda-span 10)               ;; show next 10 days
(setq org-agenda-start-day "-0d")       ;; start from today

;; -----------------------------------------------------------------------------
;; development (git & lsp)
;; -----------------------------------------------------------------------------

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; makes git diffs in magit look like github/gitlab with syntax highlighting
(use-package magit-delta
  :after magit
  :hook (magit-mode . magit-delta-mode))

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

;; configure flymake ui (force bottom window for errors)
(use-package flymake
  :ensure nil
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Flymake diagnostics.*"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 0)
                 (window-height . 0.2))))

;; silence errors if formatting fails on save
(defun my/eglot-format-on-save ()
  (when (eglot-managed-p)
    (ignore-errors (eglot-format-buffer))))

(add-hook 'before-save-hook #'my/eglot-format-on-save)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; -----------------------------------------------------------------------------
;; languages
;; -----------------------------------------------------------------------------

;; --- elisp formatting ---
(defun my/indent-buffer ()
  "indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun my/elisp-mode-hook ()
  ;; auto-indent elisp files on save
  (add-hook 'before-save-hook #'my/indent-buffer nil t))

(add-hook 'emacs-lisp-mode-hook #'my/elisp-mode-hook)

;; --- web ---
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
;; math & pdfs
;; -----------------------------------------------------------------------------

;; best pdf viewer for emacs (requires running m-x pdf-tools-install once)
(use-package pdf-tools
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-loader-install)
  (setq-default pdf-view-display-size 'fit-page)

  ;; 1. disable line numbers in pdf mode to fix the warning
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))

  ;; 2. enable midnight mode (dark mode for pdfs) automatically
  (add-hook 'pdf-view-mode-hook (lambda () (pdf-view-midnight-minor-mode))))

(use-package tex
  :ensure auctex
  :defer t
  :hook ((LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . LaTeX-math-mode)) ;; fixed: was 'math-minor-mode'
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-PDF-mode t)
  (setq TeX-master nil)

  ;; use pdf-tools to open compiled pdfs
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t))

;; -----------------------------------------------------------------------------
;; misc
;; -----------------------------------------------------------------------------

(electric-pair-mode -1)
(show-paren-mode 1)
(setq compile-command "")

;; hide the title bar and borders completely
(add-to-list 'default-frame-alist '(undecorated . t))

;; wq like in vim
(defun evil-wq-to-dired ()
  (interactive)
  (save-buffer)
  (kill-this-buffer)
  (dired default-directory))

(evil-ex-define-cmd "wq" #'evil-wq-to-dired)
