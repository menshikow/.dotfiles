;;; -*- lexical-binding: t; -*-

;; --- Basic Settings ---
(setq inhibit-startup-screen t
      visible-bell nil
      ring-bell-function 'ignore
      auto-save-default nil
      create-lockfiles nil
      gc-cons-threshold (* 50 1000 1000)
      read-process-output-max (* 1024 1024)
      scroll-conservatively 101
      scroll-margin 3
      mouse-wheel-scroll-amount '(2 ((shift) . 1))
      mouse-wheel-progressive-speed nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(fset 'yes-or-no-p 'y-or-n-p)
(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(global-hl-line-mode 1)

;; --- Line Numbers (global with exceptions) ---
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(dolist (mode '(term-mode vterm-mode shell-mode compilation-mode))
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda () (display-line-numbers-mode 0))))

;; --- Font ---
(when (member "JetBrains Mono" (font-family-list))
  (set-face-attribute 'default nil :font "JetBrains Mono" :height 130))
(setq-default cursor-type 'box)

;; --- Package Management ---
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; --- Windows-specific tweaks ---
(defconst my/is-windows (eq system-type 'windows-nt))
(when my/is-windows
  ;; Use Windows native file dialogs
  (setq use-dialog-box t
        use-file-dialog t)
  
  ;; Better performance on Windows
  (setq w32-pipe-read-delay 0
        w32-get-true-file-attributes nil)
  
  ;; Fix path separators - use forward slashes everywhere
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix)
  
  ;; Make sure exec-path includes common Windows tool locations
  (add-to-list 'exec-path "C:/Program Files/LLVM/bin")
  (add-to-list 'exec-path "C:/msys64/mingw64/bin")
  (add-to-list 'exec-path "C:/msys64/usr/bin")
  
  ;; Convert Windows paths to Unix-style for consistency
  (defun my/windows-path-to-unix (path)
    "Convert Windows path to Unix-style path."
    (replace-regexp-in-string "\\\\" "/" path))
  
  ;; Fix environment variables
  (setenv "PATH" (my/windows-path-to-unix (getenv "PATH")))
  
  ;; Handle Git Bash if installed
  (when (file-exists-p "C:/Program Files/Git/bin/bash.exe")
    (setq explicit-shell-file-name "C:/Program Files/Git/bin/bash.exe")
    (setq shell-file-name "bash")
    (setq explicit-bash.exe-args '("--login" "-i"))
    (add-to-list 'exec-path "C:/Program Files/Git/bin"))
  
  ;; Use PowerShell as alternative (uncomment if preferred)
  ;; (setq explicit-shell-file-name "powershell.exe")
  ;; (setq explicit-powershell.exe-args '("-NoLogo" "-NoExit"))
  
  ;; Fix TLS issues on Windows
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
  
  ;; Use UTF-8 everywhere
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8-unix)
  (set-terminal-coding-system 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix))

;; --- Evil ---
(setq evil-want-keybinding nil
      evil-want-C-u-scroll t
      evil-undo-system 'undo-redo)
(use-package evil
  :config
  (evil-mode 1)
  ;; box cursor in all modes
  (setq evil-normal-state-cursor 'box
        evil-insert-state-cursor 'box
        evil-visual-state-cursor 'box
        evil-replace-state-cursor 'box))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

;; --- Theme ---
(use-package badger-theme
  :config (load-theme 'badger t))

;; --- Keybindings ---
(global-set-key (kbd "M-=") #'text-scale-increase)
(global-set-key (kbd "M--") #'text-scale-decrease)
(global-set-key (kbd "M-0") (lambda () (interactive) (text-scale-set 0)))
(global-set-key (kbd "M-w") #'kill-current-buffer)
(global-set-key (kbd "M-W") #'kill-buffer-and-window)
(global-set-key (kbd "C-c s") #'shell-command)
(global-set-key (kbd "M-e") #'find-file)
(global-set-key (kbd "M-b") #'switch-to-buffer)
(global-set-key (kbd "M-c") #'compile)
(global-set-key (kbd "C-c r") #'recompile)
(global-set-key (kbd "M-.") #'xref-find-definitions)
(global-set-key (kbd "M-,") #'xref-pop-marker-stack)
(global-set-key (kbd "M-/") #'comment-line)
(global-set-key (kbd "M-d") #'dired-jump)

;; --- Eglot & C/C++ ---
(use-package eglot
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode) . ("clangd")))
  (setq eglot-connect-timeout 30))

(defun my/format-buffer ()
  (when (and (eglot-managed-p)
             (eglot--server-capable :documentFormattingProvider))
    (eglot-format-buffer)))
(add-hook 'before-save-hook #'my/format-buffer)

;; --- Compilation Improvements ---
(use-package ansi-color
  :ensure nil
  :config
  (defun my/colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook #'my/colorize-compilation-buffer))

;; --- C/C++ Style ---
(setq c-default-style "windows"
      c-basic-offset 4)

;; --- Mode Line ---
(setq-default mode-line-format
              '("%e" mode-line-buffer-identification "   " mode-line-position))

;; --- Vim-style :wq ---
(defun my/evil-wq ()
  "Save buffer and kill it, like :wq in Vim."
  (interactive)
  (save-buffer)
  (kill-buffer))

(with-eval-after-load 'evil
  (evil-ex-define-cmd "wq" 'my/evil-wq))
