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

(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (add-to-list 'exec-path (expand-file-name "~/.pyenv/shims"))
  (add-to-list 'exec-path (expand-file-name "~/.ghcup/bin"))
  (setenv "PATH" (concat (expand-file-name "~/.pyenv/shims") ":"
                         (expand-file-name "~/.ghcup/bin") ":"
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
(global-visual-line-mode 1)
(electric-pair-mode 1)
(setq backward-delete-char-untabify-method 'hungry)
(setq initial-buffer-choice nil)
(add-hook 'after-init-hook #'dired-jump)
(save-place-mode 1)
(repeat-mode 1)
(global-auto-revert-mode 1)

;; theme
;; (add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
;; (load-theme 'naysayer t)

(defun my/smart-return ()
  "Press RET between an empty pair like `{|}`, `(|)`, or `[|]` and
get the pair split onto two lines with the cursor properly indented
on the blank line, and the closing delimiter on its own indented
line below. Otherwise behaves like a normal `newline`."
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

(set-face-attribute 'default nil :font "Liberation Mono" :height 160 :weight 'regular)

(setq compile-command "")
(global-set-key [escape] 'keyboard-escape-quit)

(setq scroll-conservatively 101
      scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)

(pixel-scroll-precision-mode 1)
(add-to-list 'display-buffer-alist '("\\*warnings\\*" (display-buffer-no-window)))

;; ==============================================================================
;; Evil
;; ==============================================================================
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t evil-want-keybinding nil)
  :config
  (setq evil-insert-state-cursor '(bar)
        evil-normal-state-cursor '(box)
        evil-visual-state-cursor '(box)
        evil-replace-state-cursor '(box))
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

(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "M-0") (lambda () (interactive) (text-scale-set 0)))
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "M-n") 'flycheck-next-error)
(global-set-key (kbd "M-p") 'flycheck-previous-error)
(recentf-mode 1)
(global-set-key (kbd "C-c r") #'recentf-open-files)

;; ==============================================================================
;; Lsp
;; ==============================================================================
(use-package eglot
  :custom
  (eglot-sync-connect nil)
  :config
  (fset #'jsonrpc--log-event #'ignore)
  ;; Prefer pyright for Python over pylsp
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))))

(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

(setq read-process-output-max (* 1024 1024))
(setq eldoc-idle-delay 0.2)

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
  :config (vertico-mode))
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
         ("M-g g" . consult-goto-line))
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

(use-package corfu
  :ensure t
  :custom
  (corfu-auto nil)
  (corfu-quit-no-match t)
  :init
  (global-corfu-mode))

(global-set-key (kbd "C-<tab>") #'completion-at-point)

;; ==============================================================================
;; Magit
;; ==============================================================================
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package apheleia
  :ensure t
  :config 
  (apheleia-global-mode +1)
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff))
  (setf (alist-get 'c-mode apheleia-mode-alist) '(clang-format))
  (setf (alist-get 'c++-mode apheleia-mode-alist) '(clang-format))
  (setf (alist-get 'c-ts-mode apheleia-mode-alist) '(clang-format))
  (setf (alist-get 'c++-ts-mode apheleia-mode-alist) '(clang-format)))

;; hl-todo
(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1)
  (keymap-set hl-todo-mode-map "C-c p" #'hl-todo-previous)
  (keymap-set hl-todo-mode-map "C-c n" #'hl-todo-next)
  (keymap-set hl-todo-mode-map "C-c o" #'hl-todo-occur)
  (keymap-set hl-todo-mode-map "C-c i" #'hl-todo-insert))

;; ==============================================================================
;; org-mode and latex
;; ==============================================================================
(use-package org
  :custom
  (org-hide-emphasis-markers t)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-image-actual-width nil)
  (org-pretty-entities t)
  (org-highlight-latex-and-related '(latex script entities))
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)
  (org-log-done 'time)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)))

(with-eval-after-load 'org
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1))
  (define-key org-mode-map (kbd "C-c C-d") #'my-org-clean-latex-trash))

(use-package org-download
  :ensure t
  :after org
  :config (org-download-enable))

(defun my-org-clean-latex-trash ()
  (interactive)
  (let* ((trash-regex "\\.\\(aux\\|log\\|out\\|fdb_latexmk\\|fls\\|toc\\|bbl\\|bcf\\|run\\.xml\\|blg\\|tex\\|tex\\.pdf\\)\\'")
         (target-dir (expand-file-name default-directory))
         (files (directory-files target-dir t trash-regex)))
    (dolist (file files) (when (file-regular-p file) (delete-file file)))))

(server-start)

(use-package tex
  :ensure auctex
  :custom
  (TeX-auto-save t) (TeX-parse-self t) (TeX-master nil) (TeX-PDF-mode t)
  (TeX-source-correlate-mode t) (TeX-source-correlate-start-server t)
  :config
  (setq TeX-view-program-selection '((output-pdf "Sioyek")))
  (setq TeX-view-program-list '(("Sioyek" "/Applications/sioyek.app/Contents/MacOS/sioyek %o --reuse-instance --forward-search-file %b.tex --forward-search-line %n")))
  :hook ((LaTeX-mode . turn-on-reftex) (LaTeX-mode . flyspell-mode) (LaTeX-mode . LaTeX-math-mode)))

(use-package cdlatex :ensure t :hook ((LaTeX-mode . turn-on-cdlatex) (org-mode . turn-on-org-cdlatex)))
(use-package reftex :custom (reftex-plug-into-AUCTeX t))
(use-package ox-latex :custom (org-latex-pdf-process '("latexmk -f -pdf -synctex=1 -interaction=nonstopmode -output-directory=%o %f")))

;; ==============================================================================
;; language settings
;; ==============================================================================
(setq-default tab-width 4 indent-tabs-mode nil)

;; C/C++
(use-package cc-mode
  :mode (("\\.c\\'"   . c-ts-mode)
         ("\\.cpp\\'" . c++-ts-mode)
         ("\\.cc\\'"  . c++-ts-mode)
         ("\\.cxx\\'" . c++-ts-mode)
         ("\\.h\\'"   . c-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode)
         ("\\.hh\\'"  . c++-ts-mode))
  :hook ((c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (c-ts-mode . (lambda ()
                        (setq indent-tabs-mode nil tab-width 2)
                        (local-set-key (kbd "C-c h") #'ff-find-other-file)))
         (c++-ts-mode . (lambda ()
                          (setq indent-tabs-mode nil tab-width 2)
                          (local-set-key (kbd "C-c h") #'ff-find-other-file)))))

;; Python
;; python
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :hook ((python-mode . (lambda () (when (buffer-file-name) (eglot-ensure))))
         (python-ts-mode . (lambda () (when (buffer-file-name) (eglot-ensure)))))
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset nil))

;; Haskell
(use-package haskell-mode
  :ensure t
  :mode ("\\.hs\\'" . haskell-mode)
  :hook (haskell-mode . eglot-ensure)
  :custom (haskell-indentation-stylish t) (haskell-indent-spaces 2))

;; OCaml
(use-package tuareg
  :ensure t
  :mode ("\\.ml[ily]?\\'" . tuareg-mode)
  :hook (tuareg-mode . eglot-ensure))

;; Racket
(use-package racket-mode
  :ensure t
  :mode ("\\.rkt\\'" . racket-mode)
  :hook (racket-mode . eglot-ensure)
  :config
  (define-key racket-mode-map (kbd "C-c C-z") #'racket-repl)
  (define-key racket-repl-mode-map (kbd "C-c C-z") #'racket-repl)

  ;; Rust
  (use-package rust-mode
    :ensure t
    :mode ("\\.rs\\'" . rust-mode)
    :hook (rust-mode . eglot-ensure))

  ;; Mark
  (use-package markdown-mode
    :ensure t
    :mode ("\\.md\\'" . markdown-mode))

  (defun my/fix-nil-faces ()
    (dolist (face '(error trailing-whitespace highlight region))
      (when (and (facep face) (not (face-attribute face :foreground nil t)))
        (set-face-attribute face nil :foreground 'unspecified))))
  (add-hook 'after-init-hook #'my/fix-nil-faces)

  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file) (load custom-file))
  (provide 'init)
  (put 'downcase-region 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)
