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

(setq use-package-always-ensure t)

;; ==============================================================================
;; macos and german keyboard
;; ==============================================================================
(setq ns-command-modifier 'meta)
(setq ns-option-modifier 'none)
(setq ns-right-alternate-modifier 'none)

(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH"))))

;; ==============================================================================
;; Ui and Defaults
;; ==============================================================================
(setq-default cursor-type 'box)
(setq inhibit-startup-message nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-visual-line-mode 1)
(setq backward-delete-char-untabify-method 'hungry)
(setq initial-buffer-choice nil)

(defun my-highlight-todo () 
  "Highlight TODO, FIXME, and NOTE keywords."
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\|TODO\\)\\>" 1 'font-lock-warning-face t)
     ("\\<\\(NOTE\\)\\>" 0 'font-lock-doc-face t))))
(add-hook 'prog-mode-hook #'my-highlight-todo)

(use-package avy
  :ensure t
  :bind ("C--" . avy-goto-char-timer))

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

(set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 170 :weight 'regular)
(set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono" :height 170)
(set-face-attribute 'variable-pitch nil :family "Iosevka Etoile" :height 170)

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-night t))
(set-face-attribute 'font-lock-string-face nil :foreground "#8ABEB7")
(set-face-attribute 'font-lock-keyword-face nil :foreground "#81A2BE")
(set-face-attribute 'font-lock-function-name-face nil :foreground "#DE935F")
(set-face-attribute 'line-number nil :background "#000000")
(set-face-attribute 'fringe nil :background "#000000")
(set-face-attribute 'default nil :background "#000000")
(set-cursor-color "#FFFFFF")


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
  :init
  (setq evil-want-integration t evil-want-keybinding nil)
  :config
  (setq evil-insert-state-cursor '("#FFFFFF" bar)
        evil-normal-state-cursor '("#FFFFFF" box)
        evil-visual-state-cursor '("#FFFFFF" box)
        evil-replace-state-cursor '("#FFFFFF" box))
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

(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "M-0") (lambda () (interactive) (text-scale-set 0)))
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "M-n") 'flycheck-next-error)
(global-set-key (kbd "M-p") 'flycheck-previous-error)
(recentf-mode 1)

;; ==============================================================================
;; Lsp
;; ==============================================================================
(use-package eglot
  :ensure nil
  :custom (eglot-sync-connect nil)
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-events-buffer-config '(:size 0 :format full))
  (add-to-list 'eglot-server-programs
               '(python-mode . ("basedpyright-langserver" "--stdio"))))

(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

(setq read-process-output-max (* 1024 1024))
(setq eldoc-idle-delay 0.2)

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc emacs-lisp-package-lint org-lint)))

(use-package flycheck-eglot
  :after (flycheck eglot)
  :custom (flycheck-eglot-exclusive t)
  :config (global-flycheck-eglot-mode 1))

;; ==============================================================================
;; Completion and tools
;; ==============================================================================
(use-package vertico :init (vertico-mode))
(use-package savehist :init (savehist-mode))
(use-package marginalia :init (marginalia-mode))
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package dired
  :ensure nil
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
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 3)
  (corfu-quit-no-match t)
  :init
  (global-corfu-mode))

(use-package yasnippet :config (yas-global-mode 1))

(dolist (map (list minibuffer-local-map minibuffer-local-ns-map minibuffer-local-completion-map minibuffer-local-must-match-map))
  (define-key map (kbd "M-v") #'yank))

;; ==============================================================================
;; Magit
;; ==============================================================================
(use-package magit :bind ("C-x g" . magit-status))

(use-package apheleia 
  :config 
  (apheleia-global-mode +1)
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff)))

;; ==============================================================================
;; Org-mode and Latex
;; ==============================================================================
(use-package org
  :ensure nil
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

(use-package org-download :after org :config (org-download-enable))

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
(use-package reftex :ensure nil :custom (reftex-plug-into-AUCTeX t))
(use-package ox-latex :ensure nil :custom (org-latex-pdf-process '("latexmk -f -pdf -synctex=1 -interaction=nonstopmode -output-directory=%o %f")))

;; ==============================================================================
;; Language Settings
;; ==============================================================================
(setq-default tab-width 4 indent-tabs-mode nil)

;; C/C++
(defun my-c-style ()
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'access-label '-)
  (c-set-offset 'case-label '+))

(use-package cc-mode
  :ensure nil
  :mode (("\\.c\\'"   . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.cc\\'"  . c++-mode)
         ("\\.cxx\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.hh\\'"  . c++-mode))
  :bind (:map c-mode-base-map
              ("RET" . c-context-line-break))
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c-mode-common . my-c-style))
  :config
  (setq-default c-basic-offset 2))

;; Python
(use-package python
  :ensure mode
  :nil ("\\.py\\'" . python-mode)
  :hook (python-mode . eglot-ensure))

;; Haskell
(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :hook (haskell-mode . eglot-ensure)
  :custom (haskell-indentation-stylish t) (haskell-indent-spaces 2))

;; Rust
(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :hook (rust-mode . eglot-ensure))

;; OCaml
(use-package tuareg
  :mode ("\\.ml[ily]?\\'" . tuareg-mode)
  :hook (tuareg-mode . eglot-ensure))

;; Mark
(use-package markdown-mode :mode ("\\.md\\'" . markdown-mode))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))
(provide 'init)
(put 'downcase-region 'disabled nil)
