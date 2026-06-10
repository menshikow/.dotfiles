;; ==============================================================================
;; 0. PERFORMANCE
;; ==============================================================================
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      native-comp-async-report-warnings-errors nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))

;; ==============================================================================
;; 1. PACKAGE MANAGEMENT
;; ==============================================================================
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

(let ((elapsed (float-time (time-subtract (current-time)
                                          (or (bound-and-true-p package--last-refresh)
                                              (current-time))))))
  (when (or (not package-archive-contents) (> elapsed 86400))
    (package-refresh-contents)))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ==============================================================================
;; macOS fix 
;; ==============================================================================
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  (let ((ghcup-bin (expand-file-name "~/.ghcup/bin")))
    (when (file-directory-p ghcup-bin)
      (push ghcup-bin exec-path))))

;; ==============================================================================
;; 2. MACOS & GERMAN KEYBOARD
;; ==============================================================================
(setq ns-command-modifier 'meta)
(setq ns-option-modifier 'none)
(setq ns-right-alternate-modifier 'none)

;; ==============================================================================
;; 3. UI & DEFAULTS
;; ==============================================================================
(setq-default cursor-type 'box)
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Remove the macOS window title bar completely
(add-to-list 'default-frame-alist '(undecorated . t))

(setq visible-bell t
      ring-bell-function 'ignore
      warning-minimum-level :emergency
      native-comp-async-report-warnings-errors nil)

(setq-default display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; double paranthesis
(electric-pair-mode 0)
(setq backup-directory-alist `(("." . "~/.config/emacs/saves/")))

(set-face-attribute 'default nil
                    :font "Source Code Pro"
                    :height 180
                    :weight 'light)

;; load theme
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))
(load-theme 'aanila t)

;; compile comamnd
(setq compile-command "")

(global-set-key [escape] 'keyboard-escape-quit)

;;; scrolling 
(setq scroll-conservatively 101
      scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(1 ((shift) . 1)) 
      mouse-wheel-progressive-speed nil            
      mouse-wheel-follow-mouse 't)                 

(pixel-scroll-precision-mode 1)

(add-to-list 'display-buffer-alist
	           '("\\*Warnings\\*" (display-buffer-no-window)))

;;; status line 

(setq-default mode-line-format
              '(
                " "                  ; Spacing
                (:eval (if (boundp 'evil-mode-line-tag) evil-mode-line-tag ""))
                "  "                  ; Spacing
                "%b"                  ; Buffer name (filename)
                "  "                  ; Spacing
                (:eval vc-mode)       ; Git branch (dynamically evaluated)
                "  "                  ; Spacing
                "(%l-%c)"             ; Line and Column
                " "))                 ; Padding

;; =============================================================================
;; 4. EVIL & KEYBINDINGS
;; ==============================================================================
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :config
  (setq evil-insert-state-cursor 'box
        evil-normal-state-cursor 'box
        evil-visual-state-cursor 'box
        evil-replace-state-cursor 'box)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "M-0") (lambda () (interactive) (text-scale-set 0)))

(recentf-mode 1)

(use-package general
  :after evil
  :config
  (general-create-definer my-leader-def
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (my-leader-def
    "SPC" 'smex

    "cf"  'apheleia-format-buffer

    "ff"  'find-file
    "fs"  'save-buffer
    "fr"  'recentf-open-files

    "bb"  'switch-to-buffer
    "bk"  'kill-buffer
    "bn"  'next-buffer
    "bp"  'previous-buffer

    "sv"  'split-window-right
    "sh"  'split-window-below
    "sd"  'delete-window
    "ss"  'other-window))

(general-define-key
 :states 'normal
 "gl" 'flymake-show-diagnostic-at-point
 "K"  'eldoc-box-help-at-point            
 "[d" 'flymake-goto-prev-error   
 "]d" 'flymake-goto-next-error)

(my-leader-def
  "j" 'flymake-goto-next-error
  "k" 'flymake-goto-prev-error
  "r" 'repeat-complex-command)

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

;; ==============================================================================
;; 5. COMPLETION & TOOLS
;; ==============================================================================
(require 'ido)
(setq ido-enable-flex-matching t
      ido-everywhere t)
(ido-mode 1)

(use-package smex
  :config (smex-initialize)
  :bind ("M-x" . smex))

(use-package dired
  :ensure nil
  :bind ("M-d" . dired-jump)
  :custom
  (dired-listing-switches "-algh")
  (dired-kill-when-opening-new-dired-buffer t)

  :config

  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   "RET" 'dired-find-file
   "-"   'dired-up-directory
   "o"   'dired-find-file-other-window
   "q"   'quit-window))

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

;; paste with meta-p
(dolist (map (list minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map))
  (define-key map (kbd "M-v") #'yank))


(add-to-list 'treesit-extra-load-path "~/.emacs.d/tree-sitter/")

(use-package treesit-auto
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package eglot
  :ensure nil
  :config (fset #'jsonrpc--log-event #'ignore)
  :hook ((python-ts-mode c-ts-mode c++-ts-mode ocaml-ts-mode tuareg-mode haskell-mode haskell-ts-mode) . eglot-ensure))

(use-package eldoc-box
  :custom
  (eldoc-box-max-pixel-width 600)
  (eldoc-box-max-pixel-height 400)
  (eldoc-box-clear-with-C-g t) ; Close the box by pressing Escape/C-g
  :config

  (set-face-attribute 'eldoc-box-border nil :background "#555555"))

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
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package apheleia
  :config
  (apheleia-global-mode +1))

;; ==============================================================================
;; 8. ORG MODE
;; ==============================================================================
(use-package org
  :ensure nil 
  :custom
  (org-hide-emphasis-markers t)           ; Hides the * in *bold* etc.
  (org-startup-indented t)                ; Clean, dynamic indentation for headers
  (org-startup-with-inline-images t)      ; Show images by default
  (org-log-done 'time)                    ; Timestamp when completing a TODO

  ;; Settings for planing 
  (org-directory "~/org/") 
  
  ;; 2. What files to scan with agenda
  (org-agenda-files '("~/org/tasks.org" "~/org/projects.org"))
  
  ;; 3. Custom statuses
  (org-todo-keywords
   '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)))

(with-eval-after-load 'general
  (my-leader-def
    "o"   '(:ignore t :which-key "Org / Agenda")
    "oa"  '(org-agenda :which-key "Open Agenda")
    "oc"  '(org-capture :which-key "Capture note/task")
    "ot"  '(org-todo :which-key "Toggle TODO status")
    "os"  '(org-schedule :which-key "Schedule task")
    "od"  '(org-deadline :which-key "Set deadline")))

(use-package org-resource-download
  :after org :config (setq org-download-image-dir "images") (setq org-download-heading nil))

;; ==============================================================================
;; 9. LANGUAGE SETTINGS
;; ==============================================================================

;; C and C++ 
(setq-default c-ts-mode-indent-offset 2
              c-ts-mode-indent-style 'gnu
              c-basic-offset 2) ;;; fallback for classic c-mode




;; haskell
(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :custom
  (haskell-indentation-stylish t)
  (haskell-indent-spaces 2))

;; Use ghcup's HLS (matches GHC 9.6.7) instead of Homebrew's (GHC 9.14)
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

;; Global default for any mode that respects standard offset variables
(setq-default tab-width 2
              indent-tabs-mode nil) ; Use spaces instead of tabs


;; loading the custom file (should always be at the end)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(put 'dired-find-alternate-file 'disabled nil)
