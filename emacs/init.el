;;; init.el --- Neovim config adapted to Emacs  -*- lexical-binding: t; -*-
;;;
;;; Ported from ~/macdotfiles/nvim/
;;; Runs on Emacs 29+ (requires tree-sitter + eglot built-in)

;; ────────────────────────────────────────────────────────────────────────────
;; 1. Package management (use-package + package.el from MELPA)
;; ────────────────────────────────────────────────────────────────────────────

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

(eval-when-compile (require 'use-package))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

;; ────────────────────────────────────────────────────────────────────────────
;; 2. Evil — Vim emulation (replaces vim core)
;; ────────────────────────────────────────────────────────────────────────────

(use-package evil
  :demand t
  :init
  (setq evil-want-keybinding nil         ; for evil-collection
        evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-want-Y-yank-to-eol nil)     ; Y yanks to end of line (matches Neovim)
  :config
  (evil-mode 1)
  (evil-set-leader nil (kbd "SPC"))
  (evil-set-local-leader nil (kbd ",")))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :demand t
  :config
  (general-create-definer my-leader
    :prefix "SPC"
    :non-normal-prefix "C-SPC"
    :states '(normal visual))

  (general-create-definer my-local-leader
    :prefix ","
    :states '(normal visual)))

;; ────────────────────────────────────────────────────────────────────────────
;; 3. Editor settings (ported from nvim/lua/config/settings.lua)
;; ────────────────────────────────────────────────────────────────────────────

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

(setq split-height-threshold nil)
(setq split-width-threshold nil)

(setq case-fold-search t)
(setq select-enable-clipboard t)
(setq-default tab-width 4 indent-tabs-mode nil)
(setq-default truncate-lines t)

(setq make-backup-files nil auto-save-default nil create-lockfiles nil)

(setq undo-limit 8000000 undo-strong-limit 12000000 undo-outer-limit 120000000)

(setq-default left-fringe-width 8 right-fringe-width 0)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode -1)

(electric-pair-mode 1)

;; ────────────────────────────────────────────────────────────────────────────
;; 4. Completion UI — Vertico + Consult + Orderless (replaces telescope)
;; ────────────────────────────────────────────────────────────────────────────

(use-package vertico
  :demand t
  :config
  (vertico-mode 1)
  (setq vertico-cycle t))

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package marginalia
  :demand t
  :config
  (marginalia-mode 1))

(use-package consult
  :demand t
  :config
  (setq consult-ripgrep-args "rg --hidden --no-heading --line-number --color=never --smart-case"
        consult-preview-key "M-."))

(use-package embark
  :demand t
  :config
  (global-set-key (kbd "C-.") #'embark-act)
  (global-set-key (kbd "M-.") #'embark-dwim))

(use-package embark-consult
  :after (embark consult))

;; In-buffer completion popup (replaces nvim-cmp)
(use-package corfu
  :demand t
  :init
  (global-corfu-mode 1)
  :config
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-delay 0.2
        corfu-auto-prefix 2
        corfu-preselect-first t
        corfu-on-exact-match nil))

(use-package cape
  :demand t
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))

;; Snippets (replaces LuaSnip)
(use-package yasnippet
  :demand t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

;; Completion bindings (replaces cmp keymaps from lsp.lua)
(use-package corfu
  :bind (:map corfu-map
         ("RET" . corfu-confirm)
         ("<tab>" . corfu-next)
         ("S-<tab>" . corfu-previous)
         ("<down>" . corfu-next)
         ("<up>" . corfu-previous)
         ("C-SPC" . corfu-insert-selected)))

;; ────────────────────────────────────────────────────────────────────────────
;; 5. LSP — Eglot (replaces nvim-lspconfig + mason)
;; ────────────────────────────────────────────────────────────────────────────

(use-package eglot
  :demand t
  :config
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0)
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode c-ts-mode c++-ts-mode) .
                 ("clangd" "--query-driver=/usr/bin/clang")))
  (add-to-list 'eglot-server-programs '(python-mode python-ts-mode . ("basedpyright")))
  (add-to-list 'eglot-server-programs '(lua-mode . ("lua-language-server")))
  (add-to-list 'eglot-server-programs '(tuareg-mode . ("ocamllsp")))
  (add-to-list 'eglot-server-programs '((rust-mode rust-ts-mode) . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(zig-mode . ("zls")))
  (add-to-list 'eglot-server-programs '(tex-mode latex-mode . ("texlab"))))

;; Diagnostic config (port from lsp.lua diagnostics setup)
(setq eldoc-echo-area-prefer-doc-buffer t)
(setq eldoc-idle-delay 0.3)
(setq flymake-show-diagnostics-at-end-of-line nil)

;; ────────────────────────────────────────────────────────────────────────────
;; 6. Tree-sitter (replaces nvim-treesitter + treesitter-textobjects)
;; ────────────────────────────────────────────────────────────────────────────

(use-package treesit
  :demand t
  :config
  (setq treesit-font-lock-level 4)
  (global-set-key (kbd "M-i") #'treesit-forward-sexp))

;; Use tree-sitter modes where available
(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (python-mode . python-ts-mode)
        (rust-mode . rust-ts-mode)
        (javascript-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (html-mode . html-ts-mode)
        (bash-mode . bash-ts-mode)))

;; Function context header (replaces nvim-treesitter-context)
(use-package treesit-context
  :after treesit
  :config
  (global-treesit-context-mode 1))

;; ────────────────────────────────────────────────────────────────────────────
;; 7. Formatting — Apheleia (replaces conform.nvim)
;; ────────────────────────────────────────────────────────────────────────────

(use-package apheleia
  :demand t
  :config
  (apheleia-global-mode 1)
  (setq apheleia-mode-alist
        '((c-mode . clang-format)
          (c++-mode . clang-format)
          (c-ts-mode . clang-format)
          (c++-ts-mode . clang-format)
          (python-mode . ruff-format)
          (python-ts-mode . ruff-format)
          (js-mode . prettier)
          (js-ts-mode . prettier)
          (typescript-mode . prettier)
          (typescript-ts-mode . prettier)
          (css-mode . prettier)
          (css-ts-mode . prettier)
          (html-mode . prettier)
          (json-mode . prettier)
          (json-ts-mode . prettier)
          (lua-mode . stylua)
          (lua-ts-mode . stylua)
          (tuareg-mode . ocamlformat)
          (rust-mode . rustfmt)
          (rust-ts-mode . rustfmt)
          (zig-mode . zigfmt))))

;; ────────────────────────────────────────────────────────────────────────────
;; 8. Git — Magit + diff-hl (replaces neogit + gitsigns + diffview)
;; ────────────────────────────────────────────────────────────────────────────

(use-package magit
  :defer t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package diff-hl
  :demand t
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; ────────────────────────────────────────────────────────────────────────────
;; 9. Surround (replaces vim-surround)
;; ────────────────────────────────────────────────────────────────────────────

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; ────────────────────────────────────────────────────────────────────────────
;; 10. Multi-cursor (replaces vim-visual-multi)
;; ────────────────────────────────────────────────────────────────────────────

(use-package evil-multiedit
  :after evil
  :commands (evil-multiedit-default-keybinds)
  :config
  (evil-multiedit-default-keybinds))

;; ────────────────────────────────────────────────────────────────────────────
;; 11. Auto-pair tags (replaces nvim-ts-autotag)
;; ────────────────────────────────────────────────────────────────────────────

(use-package emmet-mode
  :hook ((html-mode css-mode js-ts-mode typescript-ts-mode) . emmet-mode)
  :config
  (setq emmet-move-cursor-between-quotes t))

;; ────────────────────────────────────────────────────────────────────────────
;; 12. Icons (replaces nvim-web-devicons)
;; ────────────────────────────────────────────────────────────────────────────

(use-package nerd-icons
  :defer t)

(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :config
  (nerd-icons-completion-mode 1))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; ────────────────────────────────────────────────────────────────────────────
;; 13. Which-key (same as Neovim plugin)
;; ────────────────────────────────────────────────────────────────────────────

(use-package which-key
  :demand t
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5
        which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-width 0.33
        which-key-side-window-max-height 0.5
        which-key-show-docstrings nil))

;; ────────────────────────────────────────────────────────────────────────────
;; 14. Statusline (replaces express_line.nvim)
;; ────────────────────────────────────────────────────────────────────────────

(use-package doom-modeline
  :demand t
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-bar-width 2
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-icon t))

;; ────────────────────────────────────────────────────────────────────────────
;; 15. Theme (replaces github-nvim-theme)
;; ────────────────────────────────────────────────────────────────────────────

(use-package github-theme
  :demand t
  :config
  (load-theme 'github t))

;; ────────────────────────────────────────────────────────────────────────────
;; 16. WakaTime (same plugin)
;; ────────────────────────────────────────────────────────────────────────────

(use-package wakatime-mode
  :demand t
  :config
  (global-wakatime-mode 1))

;; ────────────────────────────────────────────────────────────────────────────
;; 17. File explorer — Dirvish (replaces oil.nvim)
;; ────────────────────────────────────────────────────────────────────────────

(use-package dirvish
  :defer t
  :config
  (dirvish-override-dired-mode 1))

;; ────────────────────────────────────────────────────────────────────────────
;; 18. Debugger — Dape (replaces nvim-dap + nvim-dap-view + dap-virtual-text)
;; ────────────────────────────────────────────────────────────────────────────

(use-package dape
  :defer t
  :config
  (setq dape-buffer-window-arrangement 'right)

  ;; Python debugpy
  (when (executable-find "python3")
    (setq dape-adapter-path
          (plist-put dape-adapter-path :python
                     '("python3" "-m" "debugpy.adapter"))))

  ;; codelldb (for C/C++/Rust) — adjust path if needed
  (let ((codelldb (expand-file-name "~/.local/share/nvim/mason/bin/codelldb")))
    (when (file-executable-p codelldb)
      (setq dape-cdb-adapter (cons "codelldb" codelldb))))

  ;; OCaml debugging
  (when (executable-find "ocamlearlybird")
    (setq dape-adapter-path
          (plist-put dape-adapter-path :ocaml
                     '("ocamlearlybird")))))

;; ────────────────────────────────────────────────────────────────────────────
;; 19. Harpoon replacement (registers + consult-bookmark)
;; ────────────────────────────────────────────────────────────────────────────

(defun my-harpoon-add ()
  "Add current file to harpoon-style register ring."
  (interactive)
  (let* ((file (buffer-file-name))
         (ring (when file (seq-take (cons file (when (boundp 'my-harpoon-ring) my-harpoon-ring)) 4))))
    (setq my-harpoon-ring ring)
    (message "Harpooned: %s" (file-name-nondirectory file))))

(defun my-harpoon-jump (n)
  "Jump to harpoon slot N (1-4)."
  (interactive "P")
  (let* ((idx (1- (or n 1)))
         (file (when (boundp 'my-harpoon-ring) (nth idx my-harpoon-ring))))
    (if (and file (file-exists-p file))
        (find-file file)
      (message "Harpoon slot %d is empty" (or n 1)))))

(defun my-harpoon-menu ()
  "Show harpoon list via consult."
  (interactive)
  (if (and (boundp 'my-harpoon-ring) my-harpoon-ring)
      (consult--read my-harpoon-ring
                     :prompt "Harpoon: "
                     :require-match t
                     :category 'file
                     :state (consult--file-preview))
    (message "Harpoon ring is empty")))

;; Search anywhere (replaces telescope <leader>fa — no gitignore, hidden files)
(defun my-consult-search-anywhere ()
  "Search all files ignoring .gitignore."
  (interactive)
  (let ((consult-ripgrep-args "rg --hidden --no-ignore --no-heading --line-number --color=never --smart-case"))
    (consult-ripgrep)))

;; ────────────────────────────────────────────────────────────────────────────
;; 20. System-specific paths (ported from init.lua PATH additions)
;; ────────────────────────────────────────────────────────────────────────────

(when (file-directory-p "/opt/homebrew/bin")
  (setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH")))
  (add-to-list 'exec-path "/opt/homebrew/bin"))

;; ────────────────────────────────────────────────────────────────────────────
;; 21. Keybindings (ported from config/keymaps.lua and plugin keymaps)
;; ────────────────────────────────────────────────────────────────────────────

;; Window navigation in normal mode (replaces C-h/j/k/l — don't use global-set-key
;; as it conflicts with evil's default C-h for backward-char)
(evil-define-key 'normal 'global
  (kbd "C-h") #'windmove-left
  (kbd "C-j") #'windmove-down
  (kbd "C-k") #'windmove-up
  (kbd "C-l") #'windmove-right)

;; SPC leader keybindings
(my-leader
  ;; Find group (replaces telescope <leader>f*)
  "f"   '(:ignore t :which-key "find")
  "ff"  '(consult-find :which-key "find file")
  "fg"  '(consult-ripgrep :which-key "grep")
  "fw"  '(consult-grep :which-key "grep word")
  "fb"  '(consult-buffer :which-key "buffers")
  "fl"  '(consult-line :which-key "buffer lines")
  "fh"  '(apropos :which-key "help")
  "fr"  '(consult-recent-file :which-key "recent files")
  "fc"  '(consult-command :which-key "commands")
  "fk"  '(consult-keymap :which-key "keymaps")
  "fa"  '(my-consult-search-anywhere :which-key "search anywhere")

  ;; Format (replaces <leader>f from lsp.lua — moved to "F" to avoid conflict with find group)
  "F"   '(apheleia-format-buffer :which-key "format buffer")

  ;; Yank/paste to system clipboard (replaces <leader>y, <leader>Y, <leader>p)
  ;; <leader>d is reserved for debug prefix group below
  "y"   '(lambda () (interactive) (clipboard-kill-ring-save (region-beginning) (region-end)) :which-key "yank clipboard")
  "Y"   '(lambda () (interactive) (clipboard-kill-ring-save (line-beginning-position) (line-end-position)) :which-key "yank line clipboard")
  "p"   '(lambda () (interactive) (clipboard-yank) :which-key "paste clipboard")

  ;; Window split group (replaces <leader>s*)
  "s"   '(:ignore t :which-key "window")
  "sv"  '(split-window-right :which-key "vertical split")
  "sh"  '(split-window-below :which-key "horizontal split")
  "se"  '(balance-windows :which-key "equalize windows")
  "sx"  '(delete-window :which-key "close window")
  "so"  '(delete-other-windows :which-key "keep only current")

  ;; Window resize (replaces <leader>r*)
  "r"   '(:ignore t :which-key "resize")
  "rh"  '(shrink-window-horizontally 2 :which-key "shrink width")
  "rl"  '(enlarge-window-horizontally 2 :which-key "grow width")
  "rj"  '(enlarge-window 2 :which-key "grow height")
  "rk"  '(shrink-window 2 :which-key "shrink height")

  ;; Git group (replaces neogit + telescope git)
  "g"   '(:ignore t :which-key "git")
  "gs"  '(magit-status :which-key "status")
  "gl"  '(magit-log :which-key "log")
  "gd"  '(magit-diff :which-key "diff")
  "gC"  '(magit-log-all :which-key "commits")
  "gS"  '(magit-status :which-key "status")
  "gb"  '(magit-checkout :which-key "branch")

  ;; Harpoon group (replaces <leader>h*)
  "h"   '(:ignore t :which-key "harpoon")
  "hh"  '(my-harpoon-menu :which-key "harpoon menu")
  "aa"  '(my-harpoon-add :which-key "add file")
  "h1"  '(lambda () (interactive) (my-harpoon-jump 1) :which-key "slot 1")
  "h2"  '(lambda () (interactive) (my-harpoon-jump 2) :which-key "slot 2")
  "h3"  '(lambda () (interactive) (my-harpoon-jump 3) :which-key "slot 3")
  "h4"  '(lambda () (interactive) (my-harpoon-jump 4) :which-key "slot 4")

  ;; Debug group (replaces <leader>d*)
  "d"   '(:ignore t :which-key "debug")
  "db"  '(dape-breakpoint-toggle :which-key "toggle breakpoint")
  "dd"  '(dape :which-key "debug run")

  ;; File explorer (replaces oil.nvim)
  "e"   '(dirvish :which-key "file explorer")
  "o"   '(dirvish :which-key "file explorer")

  ;; Navigate diagnostics (replaces <leader>j, <leader>k)
  "j"   '(flymake-goto-next-error :which-key "next diagnostic")
  "k"   '(flymake-goto-prev-error :which-key "prev diagnostic")

  ;; Config search
  "sn"  '(lambda () (interactive) (consult-find user-emacs-directory) :which-key "search config"))

;; Normal mode evil keybindings
(evil-define-key 'normal 'global
  ;; Diagnostics / hover (replaces gl, [d, ]d)
  "gl"  #'flymake-show-buffer-diagnostics
  "[d"  #'flymake-goto-prev-error
  "]d"  #'flymake-goto-next-error

  ;; Go to definition / go back (replaces gd, gb)
  "gd"  #'xref-find-definitions
  "gb"  #'xref-pop-marker-stack

  ;; Search: center on next/prev (replaces n/N behavior from keymaps.lua)
  "n"   (lambda ()
          (interactive)
          (call-interactively #'evil-ex-search-forward)
          (recenter))
  "N"   (lambda ()
          (interactive)
          (call-interactively #'evil-ex-search-backward)
          (recenter))

  ;; Join lines keeping cursor (replaces n_J from keymaps.lua)
  "J"   #'evil-join

  ;; Escape clears search highlight (replaces <Esc> from keymaps.lua)
  [escape] (lambda ()
             (interactive)
             (evil-ex-nohighlight)
             (keyboard-quit))

  ;; Scroll and center (replaces C-d/C-u behavior from keymaps.lua)
  ;; evil already does C-d/C-u, but we enhance to center
  (kbd "C-d") (lambda () (interactive) (evil-scroll-down nil) (recenter))
  (kbd "C-u") (lambda () (interactive) (evil-scroll-up nil) (recenter)))

;; Insert mode keybindings
(evil-define-key 'insert 'global
  (kbd "C-SPC") #'corfu-insert-selected)

;; Visual mode keybindings (replaces v_J, v_K line/selection move from keymaps.lua)
(evil-define-key 'visual 'global
  "J" (lambda ()
        (interactive)
        (let ((lines (count-lines (region-beginning) (region-end))))
          (evil-operator-line-down lines)
          (evil-normal-state)))
  "K" (lambda ()
        (interactive)
        (let ((lines (count-lines (region-beginning) (region-end))))
          (evil-line-move (- lines))
          (evil-normal-state))))

;; OCaml dune promote/destruct (ported from after/ftplugin/ocaml.lua)
;; Uses local leader (",") to match the Neovim <space>cp pattern
(with-eval-after-load 'tuareg
  (my-local-leader
    :keymaps 'tuareg-mode-map
    "cp" '(compile :which-key "dune promote")
    "cd" '(eglot-code-actions :which-key "code actions (destruct)")))

;; ────────────────────────────────────────────────────────────────────────────
;; 22. Filetype-specific settings (ported from after/ftplugin/)
;; ────────────────────────────────────────────────────────────────────────────

;; C/C++: tabstop=2, shiftwidth=2 (like after/ftplugin/c.lua / cpp.lua)
(use-package c-ts-mode
  :hook (c-ts-mode-hook . (lambda ()
                            (setq-local tab-width 2 c-basic-offset 2))))
(use-package c++-ts-mode
  :hook (c++-ts-mode-hook . (lambda ()
                              (setq-local tab-width 2 c-basic-offset 2))))

;; OCaml: shiftwidth=2 (like after/ftplugin/ocaml.lua)
(use-package tuareg
  :hook (tuareg-mode-hook . (lambda ()
                              (setq-local tab-width 2 c-basic-offset 2))))

;; ────────────────────────────────────────────────────────────────────────────
;; 23. Org-mode setup (ported from nvim org-guide config + treesitter)
;; ────────────────────────────────────────────────────────────────────────────

(use-package org
  :defer t
  :config
  (setq org-directory "~/org"
        org-agenda-files (list "~/org")
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "WAITING(w)"
                    "|" "DONE(d)" "CANCELLED(c)" "DEFERRED(f)"))
        org-todo-keyword-faces
        '(("TODO" :foreground "orange" :weight bold)
          ("NEXT" :foreground "blue" :weight bold)
          ("STARTED" :foreground "cyan" :weight bold)
          ("WAITING" :foreground "yellow" :weight bold)
          ("DONE" :foreground "green" :weight bold)
          ("CANCELLED" :foreground "red" :weight bold)
          ("DEFERRED" :foreground "purple" :weight bold))))

;; Org keybindings (ported from org-guide.org)
(my-leader
  "tt" '(org-todo :which-key "toggle todo")
  "tn" '(org-todo-next :which-key "next todo state")
  "tp" '(org-todo-previous :which-key "previous todo state")
  "ts" '(org-schedule :which-key "schedule")
  "da" '(org-deadline :which-key "deadline anchor")
  "ds" '(org-schedule :which-key "date schedule")
  "dd" '(org-deadline :which-key "deadline date")
  "oa" '(org-agenda :which-key "agenda")
  "ot" '(org-todo-list :which-key "todo list")
  "oc" '(org-capture :which-key "capture")
  "os" '(org-search-view :which-key "search"))

;; ────────────────────────────────────────────────────────────────────────────
;; 24. Autocommands (ported from Neovim config)
;; ────────────────────────────────────────────────────────────────────────────

;; Hide mode-line in minibuffer completion buffers (like TelescopePrompt fix)
(add-hook 'minibuffer-setup-hook (lambda () (setq mode-line-format nil)))

;; ────────────────────────────────────────────────────────────────────────────
;; 25. Startup optimizations
;; ────────────────────────────────────────────────────────────────────────────

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 80000000
                  gc-cons-percentage 0.1)))

(provide 'init)
;;; init.el ends here
