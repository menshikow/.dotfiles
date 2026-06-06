;; ==============================================================================
;; 1. MACOS & GERMAN KEYBOARD
;; ==============================================================================
(setq ns-command-modifier 'meta)         ; Make "Command act as Meta"
(setq ns-option-modifier 'none)          ; Free up left Option for symbols  
(setq ns-right-alternate-modifier 'none) ; Free up right Option 

;; ==============================================================================
;; 2. UI 
;; ==============================================================================
(setq inhibit-startup-message t)  
(scroll-bar-mode -1)                
(tool-bar-mode -1)                  
(menu-bar-mode -1)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq visible-bell t)
(global-display-line-numbers-mode 1)

(setq backup-directory-alist `(("." . "~/.conifg/emacs/saves/")))

(set-face-attribute 'default nil
                    :font "Iosevka Nerd Font Mono"
                    :height 200
		            :weight 'light)

(use-package gruber-darker-theme
  :ensure t
  :config
  (load-theme 'gruber-darker t))

(electric-pair-mode 1)

;; ESCAPE ACTUALLY CLOSING SHIT
(global-set-key [escape] 'keyboard-escape-quit)

;; Prevent violent screen jumps when the cursor hits the bottom
(setq scroll-margin 2
      scroll-conservatively 101
      scroll-preserve-screen-position t)

;; Enable native smooth scrolling for Mac trackpads (Emacs 29+)
(pixel-scroll-precision-mode 1)

;; WARNINGS TO SHUT UP
(setq ring-bell-function 'ignore)

(setq warning-minimum-level :emergency)

(setq native-comp-async-report-warnings-errors nil)

(add-to-list 'display-buffer-alist
             '("\\*Warnings\\*"
               (display-buffer-no-window)))

(global-set-key [escape] #'keyboard-escape-quit)

;; ==============================================================================
;; 3. PACKAGE MANAGEMENT (MELPA & use-package)
;; ==============================================================================
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)


;; ==============================================================================
;; 4. EVIL
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
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package general
  :config
  (general-create-definer my-leader-def
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (my-leader-def
    "SPC" '(execute-extended-command :which-key "M-x")

    ;; File operations mapped to 'f'
    "f" '(:ignore t :which-key "Files")
    "ff" '(find-file :which-key "Find file")
    "fs" '(save-buffer :which-key "Save file")
    "fr" '(recentf-open-files :which-key "Recent files")

    ;; Buffer operations mapped to 'b'
    "b" '(:ignore t :which-key "Buffers")
    "bb" '(switch-to-buffer :which-key "Switch buffer")
    "bk" '(kill-buffer :which-key "Kill buffer")
    "bn" '(next-buffer :which-key "Next buffer")
    "bp" '(previous-buffer :which-key "Previous buffer")

    ;; Window (split screen) operations mapped to 'w'
    "s" '(:ignore t :which-key "Windows")
    "sv" '(split-window-right :which-key "Split vertical")
    "sh" '(split-window-below :which-key "Split horizontal")
    "sd" '(delete-window :which-key "Close window")
    "ss" '(other-window :which-key "Next window")))

;; ==============================================================================
;; 5. IDO
;; ==============================================================================

(require 'ido)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(ido-mode 1)

(use-package smex :ensure t
  :config
  (smex-initialize)
  :bind ("M-x" . smex))

;; dired
(use-package dired
  :ensure nil ; Built into Emacs
  :custom
  (dired-listing-switches "-algh") ; Human-readable sizes, Mac compatible
  (dired-kill-when-opening-new-dired-buffer t)) ; Prevent buffer bloat (Emacs 28+)


;; Route auto-generated UI settings to a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))



(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)))

;; Corfu: Modern, lightweight auto-completion UI
(use-package corfu
  :custom
  (corfu-auto t)          ; Enable auto-completion
  (corfu-auto-delay 0.2)  ; Slight delay to keep typing responsive
  (corfu-auto-prefix 2)   ; Trigger popup after 2 characters
  (corfu-quit-no-match t) ; Close popup when no matches remain
  :init
  (global-corfu-mode))

;; Tree-sitter: Modern syntax parsing
;; treesit-auto automatically downloads grammar files when you open a new language
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt) ; Ask before downloading a grammar
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Eglot: Built-in LSP client (Emacs 29+)
(use-package eglot
  :ensure nil
  :custom
  ;; Massive performance boost: stops Eglot from logging every JSON message
  (fset #'jsonrpc--log-event #'ignore)
  :hook
  ;; Tell Eglot to start automatically for your specific languages.
  ;; Note we are using the -ts-mode (Tree-sitter) versions of the modes.
  ((python-ts-mode js-ts-mode typescript-ts-mode c-ts-mode) . eglot-ensure))
