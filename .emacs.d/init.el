;;; -*- lexical-binding: t -*-

;;; fullscreen on startup
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

;;; package setup
(require 'package)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; emacs 27+ calls package-initialize automatically
(when (< emacs-major-version 27)
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;; keep custom junk outside of init.el
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;; macOS keyboard
(when (eq system-type 'darwin)
  (setq ns-option-modifier 'none
        ns-command-modifier 'meta))

;;; smooth scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      scroll-conservatively 101
      scroll-margin 3
      scroll-step 1)

;;; y & no prompt
(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function #'ignore)

;;; ui sanity
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t
      visible-bell nil)

;;; font + cursor
(when (member "Iosevka" (font-family-list))
  (set-face-attribute 'default nil :font "Iosevka" :height 210))
(setq-default cursor-type 'box)

;;; line number
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

(dolist (hook '(term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook
                compilation-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode 0))))

;;; persistence
(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)

;;; evil mode
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
        evil-emacs-state-cursor  'box))

(setq evil-collection-mode-list '(calendar dired magit org))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(defun save-and-kill-this-buffer ()
  (interactive)
  (save-buffer)
  (kill-current-buffer))

(evil-ex-define-cmd "wq" #'save-and-kill-this-buffer)

;;; theme
(use-package gruber-darker-theme
  :config (load-theme 'gruber-darker t))

;;; zoom in out
(global-set-key (kbd "M-=") #'text-scale-increase)
(global-set-key (kbd "M--") #'text-scale-decrease)
(global-set-key (kbd "M-0")
                (lambda ()
                  (interactive)
                  (text-scale-set 0)))

;;; vterm + magit
(use-package vterm :commands vterm)
(use-package magit :commands magit-status)

;;; dired + hide backup files
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :config
  (require 'dired-x)
  (setq dired-omit-files "^\\(?:\\.?#\\|.*~$\\)")
  (add-hook 'dired-mode-hook #'dired-omit-mode))

;;; personal keys
(global-set-key (kbd "M-d") #'dired-jump)
(global-set-key (kbd "M-'") #'vterm)
dotfiles/.emacs.d/
;; like :e in vim
(defun my/consult-find-project ()
  (interactive)
  (consult-find (project-root (project-current))))

(global-set-key (kbd "M-e") #'find-file)

;;; org mode
(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-hide-leading-stars t
        org-ellipsis " ⤵"
        org-src-window-setup 'current-window)
  (add-hook 'org-mode-hook #'org-indent-mode))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("●" "◉" "○" "◆" "◇" "▣" "□")
        org-modern-hide-stars nil))

;;; LSP
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet t
        lsp-prefer-flymake nil
        ;; disable top breadcrumb bar
        lsp-headerline-breadcrumb-enable nil
        ;; let Emacs/corfu drive completion UI
        lsp-completion-provider :none)
  :hook ((c-mode c++-mode python-mode js-mode go-mode rust-mode haskell-mode
                 typescript-mode) . lsp-deferred)
  :commands (lsp lsp-deferred))

;;; lsp-ui: hover docs + "K" and "gK"
(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor nil)   ;; no auto-popup
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-delay 0)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-max-width 80)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-always-show t)
  :config
  ;; K = LSP hover if available, else old evil-lookup (man/info)
  (with-eval-after-load 'evil
    (defun my/lsp-K-or-man ()
      "Use LSP hover if available, otherwise fall back to `evil-lookup`."
      (interactive)
      (if (and (bound-and-true-p lsp-mode)
               (fboundp 'lsp-ui-doc-glance))
          (lsp-ui-doc-glance)
        (evil-lookup)))

    (define-key evil-normal-state-map (kbd "K")  #'my/lsp-K-or-man)
    (define-key evil-normal-state-map (kbd "gK") #'lsp-describe-thing-at-point)))

(use-package lsp-haskell :after lsp-mode)

;;; TypeScript major mode
(use-package typescript-mode
  :mode ("\\.ts\\'" . typescript-mode)
  :hook (typescript-mode . (lambda () (setq typescript-indent-level 2))))

;;; unified format on save
(defun my/format-buffer ()
  "Smart, language-aware formatting."
  (cond
   ;; LSP-backed formatting + imports
   ((and (bound-and-true-p lsp-mode)
         (lsp-feature? "textDocument/formatting"))
    (lsp-format-buffer)
    (when (fboundp 'lsp-organize-imports)
      (ignore-errors (lsp-organize-imports))))
   ;; Fallback for Lisp-ish buffers
   ((memq major-mode '(emacs-lisp-mode lisp-mode))
    (indent-region (point-min) (point-max)))))

(defun my/enable-format-on-save ()
  "Enable `my/format-buffer` for this buffer."
  (add-hook 'before-save-hook #'my/format-buffer nil t))

(add-hook 'prog-mode-hook #'my/enable-format-on-save)

;;; minibuffer completion stack: vertico + orderless + marginalia + consult
(use-package vertico
  :init
  (vertico-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles . (orderless partial-completion))))))

(use-package marginalia
  :init
  (marginalia-mode 1))

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
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package affe
  :after consult
  :config
  (global-set-key (kbd "C-c s") #'affe-grep))

;;; complition ui: corfu + popupinfo
(setq tab-always-indent 'complete
      completion-cycle-threshold 3
      completion-ignore-case t
      read-file-name-completion-ignore-case t)

(use-package corfu
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-preselect-first t)
  (corfu-quit-no-match 'separator)
  :config
  ;; popupinfo lives inside corfu, no separate package
  (corfu-popupinfo-mode 1))
