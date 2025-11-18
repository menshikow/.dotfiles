;;; -*- lexical-binding: t -*-

;;; ui + startup
(add-to-list 'default-frame-alist '(fullscreen . fullboth))
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t
      visible-bell nil)

(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function #'ignore)

(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      scroll-conservatively 101
      scroll-margin 3
      scroll-step 1)

(when (member "Iosevka" (font-family-list))
  (set-face-attribute 'default nil :font "Iosevka" :height 210))
(setq-default cursor-type 'box)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(dolist (hook '(term-mode-hook vterm-mode-hook shell-mode-hook eshell-mode-hook compilation-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode 0))))

;;; package system
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(when (< emacs-major-version 27) (package-initialize))
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file))

;;; macos modifiers
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
        evil-insert-state-cursor 'box
        evil-visual-state-cursor 'box
        evil-replace-state-cursor 'box
        evil-emacs-state-cursor 'box))

(use-package evil-collection :after evil :config (evil-collection-init))
(use-package evil-commentary :after evil :config (evil-commentary-mode))

(defun save-and-kill-this-buffer () (interactive) (save-buffer) (kill-current-buffer))
(evil-ex-define-cmd "wq" #'save-and-kill-this-buffer)

;;; theme
(use-package gruber-darker-theme :config (load-theme 'gruber-darker t))

;;; keybindings
(global-set-key (kbd "M-=") #'text-scale-increase)
(global-set-key (kbd "M--") #'text-scale-decrease)
(global-set-key (kbd "M-0") (lambda () (interactive) (text-scale-set 0)))
(global-set-key (kbd "M-d") #'dired-jump)
(global-set-key (kbd "M-'") #'vterm)
(global-set-key (kbd "M-e") #'find-file)

;;; compile default
(setq compile-command "")

;;; vundo
(use-package vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (global-set-key (kbd "M-u") #'vundo))

;;; ibuffer
(global-set-key (kbd "M-b") #'ibuffer)

;;; compile shortcut
(defun my/open-compile () (interactive) (call-interactively #'compile))
(global-set-key (kbd "M-k") #'my/open-compile)

;;; FUCK THE SHIT JUST GIVE ME THE FUCKING WORKING ESC JUST CLOSE THE FUCKING SHIT WHAT IS YOUR PROBLEM
(defun my/escape-everything ()
  (interactive)
  ;; close lsp doc
  (when (fboundp 'lsp-ui-doc-hide) (lsp-ui-doc-hide))
  ;; close help
  (when-let ((win (get-buffer-window "*Help*" t))) (quit-window t win))
  ;; close compilation
  (when-let ((win (get-buffer-window "*compilation*" t))) (quit-window t win))
  ;; kill embark previews
  (when (and (fboundp 'embark-quit) embark-last-prefix-map)
    (embark-quit))
  ;; minibuffer
  (when (minibufferp) (abort-recursive-edit))
  ;; fallback
  (keyboard-escape-quit))

(global-set-key (kbd "<escape>") #'my/escape-everything)

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "<escape>") #'my/escape-everything)
  (define-key evil-visual-state-map (kbd "<escape>") #'my/escape-everything)
  ;; let evil handle insert ESC
  ;; but also quit popups immediately *after* returning to normal mode
  (define-key evil-insert-state-map (kbd "<escape>")
    (lambda () (interactive)
      (evil-normal-state)
      (my/escape-everything))))

;;; vterm magit dired
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
        org-edit-src-content-indentation 0
        org-hide-leading-stars t
        org-ellipsis " ⤵"
        org-src-window-setup 'current-window)
  (add-hook 'org-mode-hook #'org-indent-mode))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("●" "◉" "○" "◆" "◇" "▣" "□")
        org-modern-hide-stars nil))

;;; lsp
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet t
        lsp-headerline-breadcrumb-enable nil
        lsp-prefer-flymake nil
        lsp-completion-provider :none)
  :hook ((c-mode c++-mode python-mode js-mode rust-mode haskell-mode typescript-mode go-ts-mode) . lsp-deferred)
  :commands lsp lsp-deferred)

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-delay 0)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-max-width 80)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-peek-enable t)
  :config
  (with-eval-after-load 'evil
    (defun my/lsp-K-or-man ()
      (interactive)
      (if (and (bound-and-true-p lsp-mode) (fboundp 'lsp-ui-doc-glance))
          (lsp-ui-doc-glance)
        (evil-lookup)))
    (define-key evil-normal-state-map (kbd "K")  #'my/lsp-K-or-man)
    (define-key evil-normal-state-map (kbd "gK") #'lsp-describe-thing-at-point)))

;;; treesit go mode
(setq major-mode-remap-alist '((go-mode . go-ts-mode)))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-hook 'go-ts-mode-hook #'lsp-deferred)

;;; formatting
(defun my/lsp-format-if-supported ()
  (when (and (bound-and-true-p lsp-mode)
             (lsp-feature? "textDocument/formatting"))
    (lsp-format-buffer)
    (when (fboundp 'lsp-organize-imports)
      (ignore-errors (lsp-organize-imports)))))

(defun my/format-buffer ()
  (cond
   ((and (bound-and-true-p lsp-mode)
         (lsp-feature? "textDocument/formatting"))
    (my/lsp-format-if-supported))
   ((memq major-mode '(emacs-lisp-mode lisp-mode))
    (indent-region (point-min) (point-max)))))

(defun my/enable-format-on-save ()
  (add-hook 'before-save-hook #'my/format-buffer nil t))

(add-hook 'prog-mode-hook #'my/enable-format-on-save)

;;; completion
(setq tab-always-indent 'complete
      completion-cycle-threshold 3
      completion-ignore-case t
      read-file-name-completion-ignore-case t)

(use-package corfu
  :init (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0)
  (corfu-preselect-first t)
  (corfu-quit-no-match 'separator)
  :config (corfu-popupinfo-mode 1))

(use-package cape)
(add-hook 'lsp-completion-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list (cape-super-capf
                               #'lsp-completion-at-point
                               #'cape-dabbrev
                               #'cape-file)))))

;;; minibuffer
(use-package vertico :init (vertico-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles . (orderless partial-completion))))))

(use-package marginalia :init (marginalia-mode 1))

(use-package consult
  :bind (("C-s"     . consult-line)
         ("C-x b"   . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("M-y"     . consult-yank-pop)
         ("M-g g"   . consult-goto-line)
         ("M-g i"   . consult-imenu)))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult :after (embark consult))

(use-package affe
  :after consult
  :config (global-set-key (kbd "C-c s") #'affe-grep))
