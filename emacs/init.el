;; ==============================================================================
;; 1. PACKAGE MANAGEMENT
;; ==============================================================================
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;; ==============================================================================
;; 2. MACOS & GERMAN KEYBOARD
;; ==============================================================================
(setq ns-command-modifier 'meta)
(setq ns-option-modifier 'none)
(setq ns-right-alternate-modifier 'none)

;; ==============================================================================
;; 3. UI & DEFAULTS
;; ==============================================================================
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq visible-bell t
      ring-bell-function 'ignore
      warning-minimum-level :emergency
      native-comp-async-report-warnings-errors nil)

(global-display-line-numbers-mode 1)
(electric-pair-mode 1)

;; Fixed path typo: .conifg -> .config
(setq backup-directory-alist `(("." . "~/.config/emacs/saves/")))

(set-face-attribute 'default nil
		    :font "Iosevka Nerd Font Mono"
		    :height 180
		    :weight 'light)

(global-set-key [escape] 'keyboard-escape-quit)

(setq scroll-margin 2
      scroll-conservatively 101
      scroll-preserve-screen-position t)
(pixel-scroll-precision-mode 1)

(add-to-list 'display-buffer-alist
	     '("\\*Warnings\\*" (display-buffer-no-window)))

(use-package gruber-darker-theme
  :config (load-theme 'gruber-darker t))

;; ==============================================================================
;; 4. EVIL & KEYBINDINGS
;; ==============================================================================
(use-package evil
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil)
  :custom
  (evil-insert-state-cursor 'box)
  (evil-normal-state-cursor 'box)
  (evil-visual-state-cursor 'box)
  (evil-replace-state-cursor 'box)
  :config (evil-mode 1))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

(use-package general
  :config
  (general-create-definer my-leader-def
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (my-leader-def
    "SPC" '(smex :which-key "M-x (Smex)")
    "f" '(:ignore t :which-key "Files")
    "ff" '(find-file :which-key "Find file")
    "fs" '(save-buffer :which-key "Save file")
    "fr" '(recentf-open-files :which-key "Recent files")
    "b" '(:ignore t :which-key "Buffers")
    "bb" '(switch-to-buffer :which-key "Switch buffer")
    "bk" '(kill-buffer :which-key "Kill buffer")
    "bn" '(next-buffer :which-key "Next buffer")
    "bp" '(previous-buffer :which-key "Previous buffer")
    "s" '(:ignore t :which-key "Windows")
    "sv" '(split-window-right :which-key "Split vertical")
    "sh" '(split-window-below :which-key "Split horizontal")
    "sd" '(delete-window :which-key "Close window")
    "ss" '(other-window :which-key "Next window")

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
  :custom
  (dired-listing-switches "-algh")
  (dired-kill-when-opening-new-dired-buffer t))

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

(use-package treesit-auto
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package eglot
  :ensure nil
  :config (fset #'jsonrpc--log-event #'ignore)
  :hook ((python-ts-mode c-ts-mode c++-ts-mode ocaml-ts-mode tuareg-mode) . eglot-ensure))

;; ==============================================================================
;; 6. CUSTOM FILE
;; ==============================================================================
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
