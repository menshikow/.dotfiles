;; -*- lexical-binding: t; -*-

;; ==============================================================================
;; performance
;; ==============================================================================
(defvar native-comp-async-report-warnings-errors)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      native-comp-async-report-warnings-errors 'silent)

(add-hook' emacs-startup-hook
	   (lambda ()
	     (setq gc-cons-threshold 100000000
		   gc-cons-percentage 0.1)))

;; ==============================================================================
;; packages
;; ==============================================================================
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-always-ensure nil)

;; ==============================================================================
;; macos and german keyboard
;; ==============================================================================
(setq ns-command-modifier 'meta)
(setq ns-option-modifier 'none)
(setq ns-right-alternate-modifier 'none)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Locally installed tools (~/.local/bin, cargo, go, etc.)
(dolist (dir (list (expand-file-name "~/.local/bin")
                   (expand-file-name "~/.cargo/bin")
                   (expand-file-name "~/go/bin")
                   (expand-file-name "~/.local/go/bin")))
  (add-to-list 'exec-path dir))

(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (add-to-list 'exec-path (expand-file-name "~/.pyenv/shims"))
  (add-to-list 'exec-path (expand-file-name "~/.ghcup/bin"))
  (add-to-list 'exec-path (expand-file-name "~/go/bin"))
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
  (setenv "PATH" (concat (expand-file-name "~/.pyenv/shims") ":"
                         (expand-file-name "~/.ghcup/bin") ":"
                         (expand-file-name "~/go/bin") ":"
                         (expand-file-name "~/.cargo/bin") ":"
                         "/opt/homebrew/bin:"
                         (getenv "PATH"))))

;; ==============================================================================
;; Ui and defaults
;; ==============================================================================
(setq-default cursor-type 'box)
(setq inhibit-startup-message 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq-default truncate-lines t)
(electric-pair-mode 1)
(setq backward-delete-char-untabify-method 'hungry)
(setq initial-buffer-choice nil)
(add-hook 'after-init-hook #'dired-jump)
(save-place-mode 1)
(repeat-mode 1)
(global-auto-revert-mode 1)
(winner-mode 1)
(setq display-line-numbers-type 'visual
      display-line-numbers-width 4)
(global-display-line-numbers-mode)

(global-hl-line-mode -1)

(set-face-attribute 'hl-line nil :background "#e0e0e0")

;; colorscheme
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

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

(define-key prog-mode-map (kbd "RET") #'my/smart-return)

;; splits-settings 
(global-set-key (kbd "C-c z") #'delete-other-windows)
(global-set-key (kbd "C-c u") #'winner-undo)

(defvar my/window-toggle nil)
(defun my/toggle-maximize-window ()
  (interactive)
  (if (= (count-windows) 1)
      (winner-undo)
    (progn
      (setq my/window-toggle t)
      (delete-other-windows))))


(global-set-key (kbd "C-x 9") #'my/toggle-maximize-window)
(global-set-key (kbd "C-M-i") #'completion-at-point)

;; packages
(use-package avy
  :ensure t
  :bind ("C--" . avy-goto-char-timer))

(use-package ace-link
  :ensure t
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

;; (set-face-attribute 'default nil :font (font-spec :family "Terminus (TTF)" :size 16.0) :weight 'normal) 
(set-face-attribute 'default nil :font (font-spec :family "Iosevka Extended" :size 15.0) :weight 'normal)
;; (set-face-attribute 'default nil :font (font-spec :family "DejaVu Sans Mono" :size 15.0) :weight 'normal)

(setq compile-command "")
(global-set-key [escape] 'keyboard-escape-quit)

(setq scroll-conservatively 101
      scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)

;; better scrolling + turn off the mac version warnings
(pixel-scroll-precision-mode 1)
(add-to-list 'display-buffer-alist '("\\*warnings\\*" (display-buffer-no-window)))

;; pasting in the minibuffer with C-z (fucking german keyboard)
(global-set-key (kbd "C-z") #'Evil)
(dolist (map (list minibuffer-local-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-filename-completion-map
                   minibuffer-local-isearch-map))
  (define-key map (kbd "C-z") #'yank))

;; ==============================================================================
;; Evil-mode
;; ==============================================================================
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t evil-want-keybinding nil
        evil-want-C-u-scroll t)
  :config
  (setq evil-insert-state-cursor '(box) 
        evil-normal-state-cursor '(box) 
        evil-visual-state-cursor '(box) 
        evil-replace-state-cursor'(box))
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode 1))

(use-package evil-commentary
  :ensure t
  :after evil
  :config (evil-commentary-mode))

(use-package evil-mc
  :ensure t
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
    (interactive "p") (evil-find-char cunt nil ?\{))
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

(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "M-0") (lambda () (interactive) (text-scale-set 0)))
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "M-n") 'flycheck-next-error)
(global-set-key (kbd "M-p") 'flycheck-previous-error)
(recentf-mode 1)
(global-set-key (kbd "C-c r") #'recentf-open-files)

;; ==============================================================================
;; Lsp, formatting & linting
;; ==============================================================================
(use-package eglot
  :custom
  (eglot-sync-connect nil)
  (eglot-ignored-server-capabilities '(:codeActionProvider :codeActionResolve))
  :hook ((python-ts-mode python-mode) . eglot-ensure)
  ((java-mode java-ts-mode) . eglot-ensure)
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("ruff" "server")))
  (add-to-list 'eglot-server-programs
               '((java-mode java-ts-mode) . ("jdtls"))))

(add-hook 'java-ts-mode-hook (lambda () (setq java-ts-mode-indent-offset 4)))
(add-hook 'java-mode-hook (lambda () (setq c-basic-offset 4)))

(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

(setq read-process-output-max (* 1024 1024))
(setq eldoc-idle-delay 0.2)

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1)
  ;; Python via ruff
  (setf (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "--stdin-filename" file "-"))
  (add-to-list 'apheleia-mode-alist '(python-mode . ruff))
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff))
  ;; Go via gofmt
  (setf (alist-get 'gofmt apheleia-formatters)
        '("gofmt"))
  (add-to-list 'apheleia-mode-alist '(go-mode . gofmt))
  (add-to-list 'apheleia-mode-alist '(go-ts-mode . gofmt))
  ;; Rust via rustfmt
  (setf (alist-get 'rustfmt apheleia-formatters)
        '("rustfmt" "--edition" "2021"))
  (add-to-list 'apheleia-mode-alist '(rust-mode . rustfmt))
  (add-to-list 'apheleia-mode-alist '(rust-ts-mode . rustfmt))
  ;; Java via clang-format (4 spaces)
  (setf (alist-get 'clang-format apheleia-formatters)
        '("clang-format" "-assume-filename" file "--style={IndentWidth: 4, ColumnLimit: 100}"))
  (add-to-list 'apheleia-mode-alist '(java-mode . clang-format))
  (add-to-list 'apheleia-mode-alist '(java-ts-mode . clang-format)))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :custom
  (flycheck-indication-mode nil)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc emacs-lisp-package-lint org-lint python-mypy)))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :custom (flycheck-eglot-exclusive t)
  :config (global-flycheck-eglot-mode 1))

;; ==============================================================================
;; Completion and tools
;; ==============================================================================
(use-package vertico
  :ensure t
  :config
  (vertico-mode))
(use-package savehist :init (savehist-mode))
(use-package marginalia
  :ensure t
  :init (marginalia-mode))
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-s" . consult-ripgrep)
         ("M-y" . consult-yank-pop)
         ("M-g" . consult-goto-line))
  :custom
  (consult-project-root-function #'project-root))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :config
  (setq embark-help-key "?"))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; autocompletion
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 2)
  (corfu-count 8)
  (corfu-min-width 30)
  (corfu-max-width 100)
  (corfu-on-exact-match nil)
  (corfu-scroll-margin 4)
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  :config
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
  :custom (project-list-file "~/.config/emacs/projects"))

(use-package dired
  :bind ("M-d" . dired-jump)
  :custom (dired-listing-switches "-algh")
  :config
  (when (executable-find "gls")
    (setq insert-directory-program "gls"))
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "RET") 'dired-find-file)
    (define-key dired-mode-map (kbd "-") 'dired-up-directory)
    (define-key dired-mode-map (kbd "o") 'dired-find-file-other-window)
    (define-key dired-mode-map (kbd "q") 'quit-window)))

;; Go
(use-package go-ts-mode
  :ensure t
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
  :ensure t
  :hook (rust-ts-mode . cargo-minor-mode))

;; Common Lisp
(use-package sly
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl")
  (setq sly-auto-start 'always)
  :hook ((lisp-mode . sly-editing-mode)))

;; Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Mark
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

(defun my/fix-nil-faces ()
  (dolist (face '(error trailing-whitespace highlight region))
    (when (and (facep face) (not (face-attribute face :foreground nil t)))
      (set-face-attribute face nil :foreground 'unspecified))))
(add-hook 'after-init-hook #'my/fix-nil-faces)

(setq custom-enabled-themes nil) ;; we manage themes manually in init.el
(provide 'init)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(void-gruber))
 '(custom-safe-themes
   '("ef61d651d3d4b8c1ce24c10e8ce99de5eb8554161c7e7d923d18152f7528b7f2"
     "d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c"
     "3d39093437469a0ae165c1813d454351b16e4534473f62bc6e3df41bb00ae558"
     "7833b86eaa71d72cddfd2ef1fb296f3d42e9e5e15d9fa26ab9a527b0d37ecdb0"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
