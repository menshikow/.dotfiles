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

;; theme
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

(setq-default cursor-type 'box)
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-visual-line-mode t)
(electric-pair-mode 1)
(add-to-list 'auto-mode-alist '("/[^./]+\\'" . org-mode))

(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; remove the macos window title bar completely
(add-to-list 'default-frame-alist '(undecorated . t))

(setq visible-bell t
      ring-bell-function 'ignore
      warning-minimum-level :emergency
      native-comp-async-report-warnings-errors nil)

(setq-default display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; don't check for version control on every file
(setq vc-handled-backends '(git))

;; make opening files snappier by disabling unnecessary auto-checks
(setq file-name-handler-alist nil)

(setq backup-directory-alist `(("." . "~/.config/emacs/saves/")))

(set-face-attribute 'default nil
                    :font "CaskaydiaMono Nerd Font"
                    :height 180
                    :weight 'regular)

;; compile command
(setq compile-command "")

(global-set-key [escape] 'keyboard-escape-quit)

;; scrolling
(setq scroll-conservatively 101
      scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)

(pixel-scroll-precision-mode 1)

(add-to-list 'display-buffer-alist
             '("\\*warnings\\*" (display-buffer-no-window)))

;; =============================================================================
;; 4. EVIL & KEYBINDINGS
;; ==============================================================================
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
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
  :config
  (evil-commentary-mode))

;; global emacs adjustments
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "M-0") (lambda () (interactive) (text-scale-set 0)))

;; bind C-x C-b to do the same as C-x b
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

;; error navigation
(global-set-key (kbd "M-n") 'flycheck-next-error)
(global-set-key (kbd "M-p") 'flycheck-previous-error)

(recentf-mode 1)

;; ==============================================================================
;; 5. COMPLETION & TOOLS
;; ==============================================================================

;; enable vertico for a clean, vertical minibuffer ui
(use-package vertico
  :init
  (vertico-mode))

;; persist history over emacs restarts (replaces smex)
(use-package savehist
  :init
  (savehist-mode))

;; add rich annotations in the minibuffer (docstrings, keybindings, etc.)
(use-package marginalia
  :init
  (marginalia-mode))

;; use orderless for space-separated, out-of-order fuzzy matching
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package dired
  :ensure nil
  :bind ("M-d" . dired-jump)
  :custom
  (dired-listing-switches "-algh")
  :config
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "RET") 'dired-find-file)
    (define-key dired-mode-map (kbd "-") 'dired-up-directory)
    (define-key dired-mode-map (kbd "o") 'dired-find-file-other-window)
    (define-key dired-mode-map (kbd "q") 'quit-window)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match t)
  :init (global-corfu-mode))

;; snippets
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; paste with Meta-p in minibuffer
(dolist (map (list minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map))
  (define-key map (kbd "M-v") #'yank))

(setq treesit-extra-load-path '("~/.config/emacs/tree-sitter"))

(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(defvar my/ts-grammar-cache (make-hash-table :test 'equal))

(defun my/treesit-language-available-p-cache (orig-fun lang &rest args)
  (if (gethash lang my/ts-grammar-cache)
      t
    (let ((res (apply orig-fun lang args)))
      (when res
        (puthash lang t my/ts-grammar-cache))
      res)))

(advice-add 'treesit-language-available-p :around #'my/treesit-language-available-p-cache)

(use-package eglot
  :ensure nil
  :custom
  (eglot-sync-connect nil)
  :config
  (fset #'jsonrpc--log-event #'ignore))

;; ==============================================================================
;; 6. FLYCHECK, EGLOT & ERROR DISPLAY
;; ==============================================================================

(setq eldoc-idle-delay 0)

(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  ;; Nuke strict package-author linters for personal config files
  (setq-default flycheck-disabled-checkers
                '(emacs-lisp-checkdoc emacs-lisp-package-lint org-lint)))

(use-package flycheck-eglot
  :after (flycheck eglot)
  :custom
  (flycheck-eglot-exclusive nil)
  :config
  (global-flycheck-eglot-mode 1))

;; ==============================================================================
;; 7. GIT INTEGRATION
;; ==============================================================================
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode 1)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (setq dired-auto-revert-buffer t))

(use-package apheleia
  :config
  (apheleia-global-mode +1))

;; ==============================================================================
;; 8. ORG MODE
;; ==============================================================================
(use-package org
  :ensure nil
  :custom
  ;; visual & ui
  (org-hide-emphasis-markers t)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-image-actual-width nil)
  (org-pretty-entities t)

  ;; latex
  (org-highlight-latex-and-related '(latex script entities))

  ;; org-babel
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)

  ;; workflow
  (org-log-done 'time)

  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)))

;; safely scale latex fragments only after org has completely loaded
(with-eval-after-load 'org
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1)))

(use-package org-download
  :after org
  :config
  (setq-default org-download-image-dir "./images")
  (org-download-enable))

(defun my-org-clean-latex-trash ()
  "Delete LaTeX auxiliary files."
  (interactive)
  (let* ((trash-regex "\\.\\(aux\\|log\\|out\\|fdb_latexmk\\|fls\\|toc\\|bbl\\|bcf\\|run\\.xml\\|blg\\|tex\\|tex\\.pdf\\)\\'")
         (target-dir (expand-file-name default-directory))
         (files (directory-files target-dir t trash-regex))
         (deleted-count 0))
    (dolist (file files)
      (when (file-regular-p file) 
        (delete-file file)
        (setq deleted-count (1+ deleted-count))))
    (message "Cleaned %d files in: %s" deleted-count target-dir)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-d") #'my-org-clean-latex-trash))

;; latex
(server-start)

;; auctex sioyek integration
(use-package tex
  :ensure auctex
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)
  (TeX-PDF-mode t)
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-start-server t)
  
  :config
  (setq TeX-view-program-selection '((output-pdf "Sioyek")))
  (setq TeX-view-program-list
        '(("Sioyek" "/Applications/sioyek.app/Contents/MacOS/sioyek %o --reuse-instance --forward-search-file %b.tex --forward-search-line %n")))
  
  :hook
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . flyspell-mode)
  (LaTeX-mode . LaTeX-math-mode))

(with-eval-after-load 'org
  (setq org-file-apps
        (append '(("\\.pdf\\'" . "/Applications/sioyek.app/Contents/MacOS/sioyek %s"))
                org-file-apps)))

(use-package cdlatex
  :ensure t
  :hook
  (LaTeX-mode . turn-on-cdlatex)
  (org-mode . turn-on-org-cdlatex))

(use-package reftex
  :ensure nil
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-use-external-file-finders t))

(use-package ox-latex
  :ensure nil
  :custom
  (org-latex-pdf-process
   '("latexmk -f -pdf -synctex=1 -interaction=nonstopmode -output-directory=%o %f")))

;; ==============================================================================
;; 9. LANGUAGE SETTINGS
;; ==============================================================================
(setq-default tab-width 4
              indent-tabs-mode nil)

(with-eval-after-load 'evil
  (define-key evil-insert-state-map (kbd "<backspace>") 'backward-delete-char-untabify))

;; c and c++
(use-package c-ts-mode
  :ensure nil
  :mode (("\\.c\\'" . c-ts-mode)
         ("\\.h\\'" . c++-ts-mode)
         ("\\.cpp\\'" . c++-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode))
  :custom
  (c-ts-mode-indent-offset 2)
  (c-ts-mode-indent-style 'gnu)
  :hook ((c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)))

;; haskell
(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :custom
  (haskell-indentation-stylish t)
  (haskell-indent-spaces 2))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(haskell-mode . ("haskell-language-server-wrapper")))
  (add-to-list 'eglot-server-programs
               '(haskell-ts-mode . ("haskell-language-server-wrapper"))))

;; ocaml
(use-package tuareg
  :mode ("\\.ml[ily]?\\'" . tuareg-mode)
  :custom
  (tuareg-default-indent 2)
  (tuareg-indent-align-with-first-arg nil))

(use-package dune
  :mode ("dune\\(?:-project\\|-workspace\\)?\\'" . dune-mode))

(use-package utop
  :hook (tuareg-mode . utop-minor-mode)
  :custom
  (utop-command "opam exec -- utop -emacs"))

;; python
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)))

;; markdown
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :custom
  (markdown-command "multimarkdown"))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
(put 'downcase-region 'disabled nil)
