;; -*- lexical-binding: t; -*-

;; ==============================================================================
;; performance
;; ==============================================================================
(defvar native-comp-async-report-warnings-errors)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      native-comp-async-report-warnings-errors 'silent)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 100000000
                  gc-cons-percentage 0.1)))

;; ==============================================================================
;; custom file
;; ==============================================================================
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; ==============================================================================
;; packages
;; ==============================================================================
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-always-ensure t)

;; ==============================================================================
;; macos and german keyboard
;; ==============================================================================
(when (eq system-type 'darwin)
  (setq ns-command-modifier 'meta
        ns-option-modifier 'none
        ns-right-alternate-modifier 'none)

  (add-to-list 'exec-path "/opt/homebrew/bin")
  (add-to-list 'exec-path (expand-file-name "~/.pyenv/shims"))
  (setenv "PATH" (concat "/opt/homebrew/bin:"
                         (expand-file-name "~/.pyenv/shims") ":"
                         (getenv "PATH"))))

;; toolchain bins shared by both machines (same install location on mac + linux)
(dolist (dir (list (expand-file-name "~/.local/bin")
                   (expand-file-name "~/.cargo/bin")
                   (expand-file-name "~/go/bin")
                   (expand-file-name "~/.local/go/bin")
                   (expand-file-name "~/.ghcup/bin")
                   (expand-file-name "~/.opam/default/bin")))
  (add-to-list 'exec-path dir))

;; ==============================================================================
;; ui and defaults
;; ==============================================================================
(setq-default cursor-type 'box)
(setq inhibit-startup-message 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq-default truncate-lines t
              indicate-truncated-lines nil)
(electric-pair-mode 1)
(setq backward-delete-char-untabify-method 'hungry)
(setq initial-buffer-choice nil)
(add-hook 'after-init-hook #'dired-jump)
(save-place-mode 1)
(repeat-mode 1)
(global-auto-revert-mode 1)
(winner-mode 1)

;; line numbers
(setq display-line-numbers-type 'visual
      display-line-numbers-width 5
      display-line-numbers-grow-only nil)
(global-display-line-numbers-mode -1)

;; font 
(cond
 ((eq system-type 'darwin)
  (set-face-attribute 'default nil
                      :font "DejaVu Sans Mono"
                      ;; :font "Iosevka Fixed"
                      :height 130))


 ((eq system-type 'gnu/linux)
                      :font "Iosevka Fixed"
                      :height 140))

;; colorscheme
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
;; (load-theme 'dark-void t)
;; (load-theme 'light-void t)

;; correct indentation
(defun my/smart-return ()
  (interactive)
  (let* ((pairs '((?\{ . ?\}) (?\( . ?\)) (?\[ . ?\])))
         (match (assq (char-before) pairs)))
    (if (and match (eq (char-after) (cdr match)))
        (progn
          (newline)
          (save-excursion
            (newline)
            (indent-according-to-mode))
          (indent-according-to-mode))
      (newline-and-indent))))

(defun my/toggle-maximize-window ()
  (interactive)
  (if (= (count-windows) 1)
      (winner-undo)
    (delete-other-windows)))

;; packages
(use-package avy
  :bind ("C--" . avy-goto-char-timer))

(use-package ace-link
  :after (org info)
  :config
  (ace-link-setup-default)
  :bind
  (:map org-mode-map ("C-c o" . ace-link-org)))

(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(undecorated . t))

(setq visible-bell t
      ring-bell-function 'ignore
      warning-minimum-level :emergency)

(setq vc-handled-backends '(git))

(defvar my/file-name-handler-alist-backup file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my/file-name-handler-alist-backup)))

(setq backup-directory-alist `(("." . "~/.config/emacs/saves/")))

(setq compile-command "")

(setq scroll-conservatively 101
      scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)

;; better scrolling + turn off the mac version warnings
(pixel-scroll-precision-mode 1)
(add-to-list 'display-buffer-alist '("\\*warnings\\*" (display-buffer-no-window)))

;; =============================================================================
;; keybindings
;; ==============================================================================

;; global keybindings
(global-set-key (kbd "C-c w") #'toggle-truncate-lines)
(define-key prog-mode-map (kbd "RET") #'my/smart-return)
(global-set-key (kbd "C-c z") #'delete-other-windows)
(global-set-key (kbd "C-c u") #'winner-undo)
(global-set-key (kbd "C-x 9") #'my/toggle-maximize-window)
(global-set-key [escape] 'keyboard-escape-quit)

;; minibuffer pasting with C-z
(dolist (map (list minibuffer-local-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-filename-completion-map
                   minibuffer-local-isearch-map))
  (define-key map (kbd "C-z") #'yank))

(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "M-0") (lambda () (interactive) (text-scale-set 0)))
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "M-n") 'flymake-goto-next-error)
(global-set-key (kbd "M-p") 'flymake-goto-prev-error)
(global-set-key (kbd "C-c r") #'recentf-open-files)
(global-set-key (kbd "C-c c") #'compile)

(use-package evil
  :init
  (setq evil-want-integration t evil-want-keybinding nil
        evil-want-C-u-scroll t)
  :config
  (setq evil-insert-state-cursor '(box) 
        evil-normal-state-cursor '(box) 
        evil-visual-state-cursor '(box) 
        evil-replace-state-cursor '(box))
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

(use-package evil-mc
  :after evil
  :config
  (global-evil-mc-mode 1)
  (define-key evil-normal-state-map (kbd "C-n") 'evil-mc-make-and-goto-next-match)
  (define-key evil-visual-state-map (kbd "C-n") 'evil-mc-make-and-goto-next-match)
  (define-key evil-normal-state-map (kbd "<escape>") 'evil-mc-undo-all-cursors))

(with-eval-after-load 'evil
  ;; ── wrapper commands for jump-to-char ──
  (defun my/evil-find-char-forward-curly (count)
    (interactive "p") (evil-find-char count t ?\{))
  (defun my/evil-find-char-backward-curly (count)
    (interactive "p") (evil-find-char count nil ?\{))
  (defun my/evil-find-char-forward-bracket (count)
    (interactive "p") (evil-find-char count t ?\[))
  (defun my/evil-find-char-backward-bracket (count)
    (interactive "p") (evil-find-char count nil ?\[))

  ;; ── / = evil search, C-s = consult-line ──
  (define-key evil-normal-state-map (kbd "C-s") 'consult-line)
  (define-key evil-motion-state-map (kbd "C-s") 'consult-line)

  ;; ── text objects (inner/around {} and []) ──
  (define-key evil-outer-text-objects-map (kbd "C-8") 'evil-a-curly)
  (define-key evil-inner-text-objects-map (kbd "C-8") 'evil-inner-curly)
  (define-key evil-outer-text-objects-map (kbd "C-5") 'evil-a-bracket)
  (define-key evil-inner-text-objects-map (kbd "C-5") 'evil-inner-bracket)

  ;; ── jump-to-char motions (f{ / F{ / f[ / F[) ──
  (define-key evil-motion-state-map (kbd "C-8") 'my/evil-find-char-forward-curly)
  (define-key evil-motion-state-map (kbd "M-8") 'my/evil-find-char-backward-curly)
  (define-key evil-motion-state-map (kbd "C-c C-5") 'my/evil-find-char-forward-bracket)
  (define-key evil-motion-state-map (kbd "C-c M-5") 'my/evil-find-char-backward-bracket)

  ;; ── block nav (like [b ]b [B ]B) ──
  (define-key evil-normal-state-map (kbd "C-5 b") 'beginning-of-defun)
  (define-key evil-normal-state-map (kbd "C-6 b") 'end-of-defun)
  (define-key evil-normal-state-map (kbd "C-5 B") 'backward-paragraph)
  (define-key evil-normal-state-map (kbd "C-6 B") 'forward-paragraph))

(recentf-mode 1)

;; ==============================================================================
;; Lsp, formatting & linting
;; ==============================================================================
(use-package eglot
  :custom
  (eglot-sync-connect nil)
  (eglot-ignored-server-capabilities '(:codeActionProvider :codeActionResolve))
  :hook
  ((python-mode-hook
    python-ts-mode-hook
    java-mode-hook
    java-ts-mode-hook
    c-mode-hook
    c-ts-mode-hook
    c++-mode-hook
    c++-ts-mode-hook) . eglot-ensure)
  :config
  (fset #'jsonrpc--log-event #'ignore)
   (add-to-list 'eglot-server-programs
                '((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((java-mode java-ts-mode) . ("jdtls")))
  ;; OS-conditional: macOS gets clangd, Linux gets clangd with gcc query-driver flags
  (add-to-list 'eglot-server-programs
               `((c-mode c-ts-mode c++-mode c++-ts-mode)
                 . ,(if (eq system-type 'darwin)
                        '("clangd")
                      '("clangd" "--query-driver=/usr/bin/g++,/usr/bin/gcc")))))
 

(add-hook 'java-ts-mode-hook (lambda () (setq java-ts-mode-indent-offset 4)))
(add-hook 'java-mode-hook (lambda () (setq c-basic-offset 4)))

(defun my/lisp-indent-settings ()
  (setq-local indent-tabs-mode nil
              tab-width 2
              electric-indent-inhibit t)
  (electric-indent-local-mode -1))

(add-hook 'emacs-lisp-mode-hook #'my/lisp-indent-settings)
(add-hook 'lisp-mode-hook       #'my/lisp-indent-settings)

(setq read-process-output-max (* 1024 1024))
(setq eldoc-idle-delay 0.2)


;; ==============================================================================
;; Completion and tools
;; ==============================================================================
(use-package vertico
  :config
  (vertico-mode))
(use-package savehist :init (savehist-mode))
(use-package marginalia
  :init (marginalia-mode))
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-s" . consult-ripgrep)
         ("M-y" . consult-yank-pop)
         ("M-g" . consult-goto-line))
  :custom
  (consult-project-root-function #'project-root))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :config
  (setq embark-help-key "?"))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package magit
  :bind ("C-x g" . magit-status))

(add-hook 'c-mode-common-hook
  (lambda ()
    (setq-local eglot-workspace-configuration
                '(:clangd (:fallbackStyle "{IndentWidth: 2, ColumnLimit: 100}")))
    (add-hook 'before-save-hook #'eglot-format nil t)))

;; completion
(setq tab-always-indent 'complete)

(use-package corfu
  :demand t
  :custom
  (corfu-count 8)
  (corfu-min-width 30)
  (corfu-max-width 100)
  (corfu-on-exact-match nil)
  
  (corfu-scroll-margin 4)
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  :config
  (define-key corfu-map (kbd "M-RET") #'corfu-insert)
  (setq corfu-auto nil)
  (set-face-attribute 'corfu-default nil
              :background "#1e1e1e"
              :foreground "#d4d4d4")
  (set-face-attribute 'corfu-current nil
              :background "#3a3a3a"
              :foreground "#ffffff")
  (set-face-attribute 'corfu-border nil
              :background "#333333"
              :foreground "#333333"))

(use-package project
  :ensure nil
  :custom (project-list-file "~/.config/emacs/projects"))

(use-package dired
  :ensure nil
  :bind ("M-d" . dired-jump)
  :custom (dired-listing-switches "-algh")
  :config
  (dolist (candidate '("/opt/homebrew/bin/gls" "/usr/local/bin/gls"))
    (when (file-executable-p candidate)
      (setq insert-directory-program candidate)))
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "RET") 'dired-find-file)
    (define-key dired-mode-map (kbd "-") 'dired-up-directory)
    (define-key dired-mode-map (kbd "o") 'dired-find-file-other-window)
    (define-key dired-mode-map (kbd "q") 'quit-window)))

;; language specific (lsp and shit)

;; Go
(use-package go-ts-mode
  :mode ("\\.go\\'" . go-ts-mode)
  :hook (go-ts-mode . eglot-ensure)
  :config
  (add-hook 'go-ts-mode-hook (lambda () (setq go-ts-mode-indent-offset 4))))

;; Rust
(use-package rust-ts-mode
  :mode ("\\.rs\\'" . rust-ts-mode)
  :hook (rust-ts-mode . eglot-ensure)
  :config
  ;; Use clippy instead of cargo check
  (add-hook 'rust-ts-mode-hook
        (lambda ()
          (setq-local eglot-workspace-configuration
              '(:rust-analyzer
                (:checkOnSave (:command "clippy")
                      :rustfmt (:extraArgs ["--edition" "2021"])))))))

(use-package cargo
  :hook (rust-ts-mode . cargo-minor-mode))

;; OCaml
(use-package tuareg
  :mode ("\\.ml[ip]?\\'" . tuareg-mode)
  :hook (tuareg-mode . eglot-ensure))

(use-package ocaml-eglot
  :after (tuareg eglot)
  :hook (tuareg-mode . ocaml-eglot))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(tuareg-mode . ("ocamllsp"))))

;; Haskell
(use-package haskell-mode
  :hook (haskell-mode . eglot-ensure))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(haskell-mode . ("haskell-language-server-wrapper" "--lsp"))))

;; Common Lisp
(use-package sly
  :config
  (setq inferior-lisp-program "sbcl")
  (setq sly-auto-start 'always)
  :hook ((lisp-mode . sly-editing-mode)))

;; Mark
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

(defun my/fix-nil-faces ()
  (dolist (face '(error trailing-whitespace highlight region))
    (when (and (facep face) (not (face-attribute face :foreground nil t)))
      (set-face-attribute face nil :foreground 'unspecified))))
(add-hook 'after-init-hook #'my/fix-nil-faces)

(setq custom-enabled-themes nil)
(provide 'init)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
