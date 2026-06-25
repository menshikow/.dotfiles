;; -*- lexical-binding: t; -*-

;; ==============================================================================
;; 0. PERFORMANCE
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
;; 1. PACKAGE MANAGEMENT
;; ==============================================================================
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-always-ensure t)

;; ==============================================================================
;; 2. MACOS & GERMAN KEYBOARD
;; ==============================================================================
(setq ns-command-modifier 'meta)
(setq ns-option-modifier 'none)
(setq ns-right-alternate-modifier 'none)

(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH"))))

;; ==============================================================================
;; 3. UI & DEFAULTS
;; ==============================================================================
(setq-default cursor-type 'box)
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-visual-line-mode 1)
(electric-pair-mode 1)
(add-to-list 'auto-mode-alist '("/[^./]+\\'" . org-mode) t)
(setq backward-delete-char-untabify-method 'hungry)
(setq initial-buffer-choice "~/")

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

(setq-default display-line-numbers-type 'relative)
(global-display-line-numbers-mode -1)
(setq vc-handled-backends '(git))

;; wanna some function to open ghostty in the directory im in rn

;; (defun my-open-ghostty ()
;;   "Launch Ghostty in the current directory."
;;   (interactive)
;;   (start-process "ghostty-process" nil "/Applications/hostty.app/Contents/MacOS/ghostty"))

;; (global-set-key (kbd "C-c g") 'my-open-ghostty)

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
;; 4. EVIL & KEYBINDINGS
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
;; 5. COMPLETION & TOOLS
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
;; 6. FLYCHECK & EGLOT
;; ==============================================================================
(use-package eglot
  :ensure nil
  :custom (eglot-sync-connect nil)
  :config (fset #'jsonrpc--log-event #'ignore))

(setq eglot-events-buffer-size 0)
(fset #'jsonrpc--log-event #'ignore)

(setq read-process-output-max (* 1024 1024))


(setq eldoc-idle-delay 0)
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc emacs-lisp-package-lint org-lint)))

(use-package flycheck-eglot
  :after (flycheck eglot)
  :custom (flycheck-eglot-exclusive nil)
  :config (global-flycheck-eglot-mode 1))

;; ==============================================================================
;; 7. GIT INTEGRATION
;; ==============================================================================
(use-package magit :bind ("C-x g" . magit-status))
;; (use-package diff-hl
;; :config
;; (global-diff-hl-mode)
;; (diff-hl-flydiff-mode 1)
;; (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
;; (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package apheleia 
  :config 
  (apheleia-global-mode +1)
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff)))

;; ==============================================================================
;; 8. ORG MODE & LATEX
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
;; 9. LANGUAGE SETTINGS
;; ==============================================================================
(setq-default tab-width 4 indent-tabs-mode nil)

;; C/C++
(use-package cc-mode
  :ensure nil
  :mode (("\\.c\\'"   . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.cc\\'"  . c++-mode)
         ("\\.cxx\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.hh\\'"  . c++-mode))
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c-mode-common . my-c-style))
  :config
  (setq-default c-basic-offset 4))

(defun my-c-style ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'access-label '-)
  (c-set-offset 'case-label '+))

;; Python
(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . eglot-ensure))

;; Haskell
(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :custom (haskell-indentation-stylish t) (haskell-indent-spaces 2))

;; OCaml
(use-package tuareg :mode ("\\.ml[ily]?\\'" . tuareg-mode))
(use-package utop :hook (tuareg-mode . utop-minor-mode))

;; Mark
(use-package markdown-mode :mode ("\\.md\\'" . markdown-mode))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))
(provide 'init)
(put 'downcase-region 'disabled nil)
