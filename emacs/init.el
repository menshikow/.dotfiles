(setq custom-file "~/.config/emacs/.custom.el")

;;; Packages 

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Fetch package list if we don't have it yet
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if it isn't already installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)


;;; Appearance

;;; Theme 
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t)
  (set-face-background 'default "#212121"))

;;; Font
(set-face-attribute 'default nil :font "Source Code Pro" :height 200 :weight 'normal)

;;; UI

(setq inhibit-startup-message t)        ; Disable the default startup screen
(scroll-bar-mode -1)                    ; Disable the visible scrollbar
(tool-bar-mode -1)                      ; Disable the toolbar
(tooltip-mode -1)                       ; Disable tooltips
(menu-bar-mode -1)                      ; Disable the top menu bar


(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; Scrolling 
(pixel-scroll-precision-mode 1)

(setq scroll-conservatively 101
      scroll-margin 3
      scroll-preserve-screen-position t
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t)

(setq mouse-wheel-scroll-amount
      '(1 ((shift) . 5)
          ((control) . nil)))


(add-to-list 'default-frame-alist '(undecorated . t))

;; German keyboard layout

(setq mac-command-modifier 'meta)       ; Make Command act as Meta (M-)
(setq mac-option-modifier 'super)       ; Make Left Option act as Super (s-)
(setq mac-right-option-modifier 'none)  ; Free up Right Option for typing [], {}, \
(setq mac-right-command-modifier 'left) ; Make Right Command behave like Left Command

;;; Line number

(column-number-mode)                    
(global-display-line-numbers-mode t)    
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;;; Disable line numbers in certain modes

(dolist (mode '(org-mode-hook           
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;;; Evil mode

(use-package evil
  :init
  (setq evil-want-integration t)        ; Required for evil-collection
  (setq evil-want-keybinding nil)       ; Required for evil-collection
  (setq evil-want-C-u-scroll t)         ; Use C-u to scroll up like in Vim
  (setq evil-want-C-i-jump t)           ; Use C-i to jump forward
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))    ; Ensure standard undo/redo works with 'u' and 'C-r'

;; Evil collection gives Vim bindings to the rest of Emacs (like Dired, Magit, etc.)
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(require 'ido)
(ido-mode t)                      ; Enable Ido for buffers and files
(setq ido-everywhere t)           ; Use Ido for nearly all menus
(setq ido-enable-flex-matching t) ; Allows fuzzy searching (e.g., typing "mj" finds "metrics.jai")

;; ------------------------------------------ 6. Evil Multiple Cursors
;; (Vim-compatible) ------------------------------------------
(use-package evil-mc
  :ensure t
  :config
  (global-evil-mc-mode 1)
  
  ;; Make Escape clear all cursors instantly when you are in Normal Mode
  (evil-define-key 'normal evil-mc-key-map (kbd "<escape>") 'evil-mc-undo-all-cursors))

;; ------------------------------------------
;; 7. Keybindings (General, Multi-Cursor & Zoom)
;; ------------------------------------------
(use-package general
  :ensure t
  :config
  ;; Define your Spacebar leader key
  (general-create-definer my-leader-def
    :states '(normal visual emacs)
    :keymaps 'override             
    :prefix "SPC"                  
    :global-prefix "M-SPC")        

  ;; Global multi-cursor overrides for Control+n and Control+p
  (general-define-key
   :states '(normal visual)
   "C-n" 'evil-mc-make-and-goto-next-match   ; Control + n = Next match
   "C-p" 'evil-mc-make-and-goto-prev-match)  ; Control + p = Previous match

  ;; Fast Screen Zoom Controls (+ / - / =) in Normal Mode
  (general-define-key
   :states 'normal
   "+" 'text-scale-increase
   "-" 'text-scale-decrease
   "=" '(lambda () (interactive) (text-scale-set 0)))

  ;; Map your Spacebar shortcuts cleanly (Preserving your exact layout)
  (my-leader-def
    ;; --- DIRED (Space -> d) ---
    "d"  '(dired :which-key "Open Dired File Manager")

    ;; --- FILES ---
    "f"  '(:ignore t :which-key "Files")
    "ff" '(find-file :which-key "Find file")
    "fs" '(save-buffer :which-key "Save file")

    ;; --- BUFFERS ---
    "b"  '(:ignore t :which-key "Buffers")
    "bb" '(switch-to-buffer :which-key "Switch buffer")
    "bd" '(kill-current-buffer :which-key "Delete buffer")

    ;; --- WINDOWS ---
    "w"  '(:ignore t :which-key "Windows")
    "sv" '(split-window-right :which-key "Split window vertically")   ; Kept your exact 'sv' binding
    "sh" '(split-window-below :which-key "Split window horizontally") ; Kept your exact 'sh' binding
    "wq" '(delete-window :which-key "Close current window")
    "w=" '(balance-windows :which-key "Balance window sizes")         ; Fixed ':whichy' typo

    ;; --- MULTI-CURSOR MENU ---
    "mn" '(evil-mc-make-and-goto-next-match :which-key "next match")
    "mp" '(evil-mc-make-and-goto-prev-match :which-key "previous match")
    "ma" '(evil-mc-make-all-cursors :which-key "all matches")

    ;; --- GIT / MAGIT (Space -> g) ---
    "g"  '(:ignore t :which-key "Git")
    "gg" '(magit-status :which-key "Magit Status")
    "gb" '(magit-blame :which-key "Git Blame")

    ;; --- LSP / CODE INTELLIGENCE (Space -> l) ---
    "l"  '(:ignore t :which-key "LSP/Code")
    "ld" '(lsp-find-definition :which-key "Go to Definition")
    "lr" '(lsp-rename :which-key "Rename Symbol")
    "la" '(lsp-execute-code-action :which-key "Code Action")
    "lf" '(lsp-format-buffer :which-key "Format Buffer")
    "lh" '(lsp-describe-thing-at-point :which-key "Hover Docs")

    ;; --- QUIT ---
    "q"  '(:ignore t :which-key "Quit")                               ; Fixed: removed the crashing syntax error paren here!
    "qq" '(save-buffers-kill-terminal :which-key "Quit Emacs")))       ; Fixed: typo ':which-y'

;; ------------------------------------------
;; 8. Global Escape Handling
;; ------------------------------------------
;; Make the ESC key quit out of standard Emacs prompts/minibuffers
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; ------------------------------------------
;; 9. Clean Up Window Splits & Fringes (Fixes Artifacts)
;; ------------------------------------------
(setq-default fringe-indicator-alist nil)
(set-face-foreground 'vertical-border "#3f3f3f")
(set-face-background 'vertical-border "#212121")

;; ------------------------------------------
;; 10. Magit Setup
;; ------------------------------------------
(use-package magit
  :ensure t)

;; ------------------------------------------
;; 11. LSP Mode & UI Setup
;; ------------------------------------------
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((haskell-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred))
  
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-enable nil))

;; ------------------------------------------
;; 12. Mason (Neovim-style Package Manager)
;; ------------------------------------------
(use-package mason
  :ensure t
  :config
  (mason-setup))

;; ------------------------------------------
;; 13. Auto-Format on Save (LSP Languages)
;; ------------------------------------------
(add-hook 'lsp-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-format-buffer nil 'local)))

;; ------------------------------------------
;; 14. Automatic Bracket & Quote Pairing
;; ------------------------------------------
(electric-pair-mode 1)

;; Ensure it pairs cleanly in a few specific edge cases
(setq electric-pair-preserve-balance t)

;; ------------------------------------------
;; 15. Elisp Auto-Formatting & Code Intelligence
;; ------------------------------------------

;; 1. Setup Background Auto-Formatting (via Apheleia)
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode 1)
  ;; Direct apheleia to use Emacs's native lisp-mode formatter on save
  (setf (alist-get 'lisp-mode apheleia-formatters) '(lisp-indent-buffer))
  (setf (alist-get 'emacs-lisp-mode apheleia-mode-alist) 'lisp-mode))

;; 2. Enhance Elisp code-completion with interactive examples
(use-package elisp-demos
  :ensure t
  :init
  (advice-add 'elisp-get-fnsym-args-string :around #'elisp-demos-advice-get-fnsym-args-string)
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)

;; ------------------------------------------
;; 16. Indentation: Spaces vs. Tabs
;; ------------------------------------------

;; 1. Use spaces instead of tabs everywhere
(setq-default indent-tabs-mode nil)

;; 2. Set the default indentation width to 4 spaces
(setq-default tab-width 4)
(setq-default c-basic-offset 2)     ; Specific to C/C++ style indentation
(setq-default lisp-body-indent 2)   ; Specific to Lisp body indentation


;; ------------------------------------------
;; 17. Vim-style LSP Navigation (K and gl)
;; ------------------------------------------
(my-leader-def
  "ld" '(lsp-find-definition :which-key "definition")
  "lr" '(lsp-find-references :which-key "references")
  "li" '(lsp-find-implementation :which-key "implementation")
  "lt" '(lsp-find-type-definition :which-key "type definition")
  "la" '(lsp-execute-code-action :which-key "code action")
  "ln" '(lsp-rename :which-key "rename")
  "le" '(lsp-show-workspace-diagnostics :which-key "diagnostics")
  "lh" '(lsp-describe-thing-at-point :which-key "hover"))
