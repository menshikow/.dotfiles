;;; init.el --- config *- lexical-binding: t -*-

;;; core
(setq gc-cons-threshold (* 50 1000 1000))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

(setq make-backup-files nil
      auto-save-default nil
      inhibit-startup-message t
      use-short-answers t
      visible-bell nil
      ring-bell-function 'ignore)

;;; packages
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;;; mac-settings

;; modifier keys
(setq ns-option-modifier 'none
      ns-command-modifier 'meta)

;; fix PATH from shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;;; ui
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode)

(set-face-attribute 'default nil
                    :font "Iosevka Nerd Font"
                    :height 170
                    :weight 'light) ;; or 'extra-light or 'thin

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      scroll-step 1
      scroll-conservatively 101
      scroll-preserve-screen-position t
      scroll-margin 0)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(undecorated . t))

;;; modeline
(setq-default mode-line-format
              '("%e" mode-line-front-space mode-line-mule-info mode-line-client
                mode-line-modified mode-line-remote mode-line-frame-identification
                mode-line-buffer-identification "    " mode-line-position
                (vc-mode vc-mode) "  " mode-line-modes mode-line-misc-info
                mode-line-end-spaces))

(use-package diminish
  :ensure t
  :after (eglot flymake)
  :config
  (diminish 'eglot-mode)
  (diminish 'flymake-mode))

;;; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'less t)

;;; evil
(use-package undo-fu)

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  (setq evil-normal-state-cursor 'box
        evil-visual-state-cursor 'box
        evil-insert-state-cursor 'box))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-multiedit
  :config (evil-multiedit-default-keybinds))

;;; keys
(use-package general
  :config
  (general-create-definer my-leader-def
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (my-leader-def
    "f"  '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find")
    "fs" '(save-buffer :which-key "save")
    "ft" '(treemacs :which-key "tree")
    "c"  '(compile :which-key "compile")
    "r"  '(recompile :which-key "recompile")
    "d"  '(dired-jump :which-key "dired")
    "b"  '(:ignore t :which-key "buffers")
    "bb" '(consult-buffer :which-key "switch")
    "bk" '(kill-current-buffer :which-key "kill")
    "bm" '(view-echo-area-messages :which-key "log")
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
    "ep" '(flymake-goto-prev-error :which-key "prev")
    "ed" '(flymake-show-diagnostic :which-key "detail")
    "a"  '(org-agenda :which-key "agenda")))

(global-set-key (kbd "M-=") #'text-scale-adjust)
(global-set-key (kbd "M-+") #'text-scale-increase)
(global-set-key (kbd "M--") #'text-scale-decrease)
(global-set-key (kbd "M-0") (lambda () (interactive) (text-scale-set 0)))
(global-set-key (kbd "M-j") #'dired-jump)
(global-set-key (kbd "M-e") #'find-file)
(global-set-key (kbd "C-c s") #'shell-command)

;;; completion
(use-package vertico :init (vertico-mode))
(use-package savehist :init (savehist-mode))
(use-package marginalia :init (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))
                                   (eglot (styles orderless)))))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)))

(use-package corfu
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 1)
  (corfu-quit-no-match 'separator)
  :config
  (corfu-popupinfo-mode 1)
  (setq corfu-popupinfo-delay 0.3)
  (defun my/disable-corfu-in-minibuffer ()
    (setq-local corfu-auto nil)
    (corfu-mode -1))
  (add-hook 'minibuffer-setup-hook #'my/disable-corfu-in-minibuffer))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package tempel
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand completion-at-point-functions)))
  :hook ((prog-mode . tempel-setup-capf) (text-mode . tempel-setup-capf)))

;;; treemacs
(use-package treemacs
  :defer t
  :config
  (setq treemacs-width 30
        treemacs-no-png-images t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred)
  (with-eval-after-load 'treemacs
    (define-key treemacs-mode-map (kbd "RET") #'treemacs-visit-node-close-treemacs)))

(use-package treemacs-projectile :after (treemacs projectile))
(use-package treemacs-evil :after (treemacs evil))
(use-package treemacs-magit :after (treemacs magit))

;;; windows
(setq display-buffer-alist
      '(("\\*compilation\\*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom) (slot . 0) (window-height . 0.35))
        ("\\*Flymake.*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom) (slot . 0) (window-height . 0.25))
        ("\\*.*\\*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom) (slot . 0) (window-height . 0.35))))

;;; git
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-delta
  :after magit
  :hook (magit-mode . magit-delta-mode))

;;; dev
(use-package eglot
  :hook ((python-mode rust-mode c-mode c++-mode go-mode js-mode typescript-mode web-mode) . eglot-ensure)
  :config
  (setq eglot-events-buffer-size 0)
  (fset 'jsonrpc-log-event #'ignore)
  (setq eglot-extend-to-xref t))

(use-package flymake
  :ensure nil
  :config
  (set-face-attribute 'flymake-error nil :underline nil :background nil)
  (set-face-attribute 'flymake-warning nil :underline nil :background nil)
  (set-face-attribute 'flymake-note nil :underline nil :background nil)
  (setq flymake-no-changes-timeout nil
        flymake-start-on-flymake-mode nil))

(defun my/eglot-format-on-save ()
  (when (eglot-managed-p) (ignore-errors (eglot-format-buffer))))

(add-hook 'before-save-hook #'my/eglot-format-on-save)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; org
(use-package org
  :hook ((org-mode . visual-line-mode) (org-mode . org-indent-mode))
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-directory "~/org/"
        org-agenda-files '("~/org/")
        org-agenda-start-on-weekday 1
        org-agenda-span 10
        org-agenda-start-day "-0d"))

(use-package org-modern
  :hook ((org-mode . org-modern-mode) (org-agenda-mode . org-modern-agenda))
  :config
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka Nerd Font Mono"))

;;; languages
(defun my/indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(add-hook 'emacs-lisp-mode-hook (lambda () (add-hook 'before-save-hook #'my/indent-buffer nil t)))

(use-package web-mode
  :mode ("\\.html\\'" "\\.css\\'" "\\.jsx\\'" "\\.tsx\\'" "\\.ts\\'")
  :config
  (setq web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2))

(use-package typescript-mode)
(use-package rust-mode :config (setq rust-format-on-save t))
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
(use-package docker-compose-mode :mode "docker-compose.*\\.ya?ml\\'")
(use-package cmake-font-lock :after cmake-mode :hook (cmake-mode . cmake-font-lock-activate))

;;; pdf & math
(use-package pdf-tools
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-loader-install)
  (setq-default pdf-view-display-size 'fit-page)
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'pdf-view-mode-hook (lambda () (pdf-view-midnight-minor-mode))))

(use-package tex
  :ensure auctex
  :defer t
  :hook ((LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . LaTeX-math-mode))
  :config
  (setq TeX-auto-save t TeX-parse-self t TeX-PDF-mode t TeX-master nil
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t))

;;; misc
(electric-pair-mode -1)
(show-paren-mode 1)
(setq compile-command "")

(defun evil-wq-to-dired ()
  (interactive)
  (save-buffer)
  (kill-this-buffer)
  (dired default-directory))

(evil-ex-define-cmd "wq" #'evil-wq-to-dired)
