;;; void-theme.el --- color theme  -*- lexical-binding: t; -*-

;; Author: Adrian Menschikow <github.com/menshikow>
;; Version: 0.2
;; Filename: void-theme.el
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/
;; License: MIT

;;; Commentary:

;; Dark monochrome colorscheme.  Based on <https://github.com/nickav/naysayer-theme.el>

;;; Code:

(unless (>= emacs-major-version 24)
  (error "The void theme requires Emacs 24 or later!"))

(deftheme void "The void color theme")

(let ((background "#000000")
      (gutters    "#000000")
      (gutter-fg  "#000000")
      (gutters-active "#000000")
      (builtin      "#a0a0a0")
      (selection  "#0000ff")
      (text       "#cccccc")
      (comments   "#6b9f6b")
      (punctuation "#a0a0a0")
      (keywords "#ffffff")
      (variables "#888888")
      (functions "#e8e8e8")
      (methods    "#b8b8b8")
      (strings    "#b08f5a")
      (constants "#888888")
      (macros "#888888")
      (numbers "#888888")
      (white     "#ffffff")
      (error "#ff4444")
      (warning "#cccccc")
      (highlight-line "#1a1a1a")
      (line-fg "#4d4d4d")
      (green "#8fdf8f"))

  (custom-theme-set-faces
   'void

   ;; Default colors
   ;; *****************************************************************************

   `(default                          ((t (:foreground ,text :background ,background :weight normal))))
   `(region                           ((t (:foreground nil :background ,selection))))
   `(cursor                           ((t (:background ,white))))
   `(fringe                           ((t (:background ,background :foreground ,white))))
   `(linum                            ((t (:background ,background :foreground ,gutter-fg))))
   `(highlight ((t (:foreground nil :background ,selection))))

   ;; Font lock faces
   ;; *****************************************************************************

   `(font-lock-keyword-face           ((t (:foreground ,keywords :weight bold))))
   `(font-lock-type-face              ((t (:foreground ,punctuation))))
   `(font-lock-constant-face          ((t (:foreground ,constants))))
   `(font-lock-variable-name-face     ((t (:foreground ,variables))))
   `(font-lock-builtin-face           ((t (:foreground ,builtin))))
   `(font-lock-string-face            ((t (:foreground ,strings))))
   `(font-lock-comment-face           ((t (:foreground ,comments))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comments))))
   `(font-lock-doc-face               ((t (:foreground ,comments))))
   `(font-lock-function-name-face     ((t (:foreground ,functions :weight bold))))
   `(font-lock-doc-string-face        ((t (:foreground ,strings))))
   `(font-lock-preprocessor-face      ((t (:foreground ,macros))))
   `(font-lock-warning-face           ((t (:foreground ,warning :weight bold :underline t))))

   ;; Plugins
   ;; *****************************************************************************
   `(trailing-whitespace ((t (:foreground nil :background ,warning))))
   `(whitespace-trailing ((t (:background nil :foreground ,warning :inverse-video t))))

   `(linum ((t (:foreground ,line-fg :background ,background))))
   `(linum-relative-current-face ((t (:foreground ,white :background ,background))))
   `(line-number ((t (:foreground ,line-fg :background ,background))))
   `(line-number-current-line ((t (:foreground ,white :background ,background))))

   ;; compilation
   `(compilation-info ((t (:foreground ,green :weight bold))))
   `(compilation-warning ((t (:foreground "#cccccc" :weight bold))))
   `(compilation-error ((t (:foreground ,error :weight bold :underline t))))
   `(compilation-mode-line-fail ((t (:foreground ,error :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,green :weight bold))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,highlight-line))))
   `(hl-line-face ((t (:background ,highlight-line))))

   ;; dired
   `(dired-directory ((t (:foreground "#ffffff" :weight bold))))

   ;; which-func
   `(which-func ((t (:inverse-video unspecified
                                    :underline unspecified
                                    :foreground ,text
                                    :weight bold
                                    :box nil))))

   ;; mode-line and powerline
   `(mode-line-buffer-id ((t (:foreground ,background :distant-foreground ,text :weight bold))))
   `(mode-line ((t (:inverse-video unspecified
                                   :underline unspecified
                                   :foreground ,background
                                   :background "#d4d4d4"
                                   :box nil))))
   `(powerline-active1 ((t (:background "#d4d4d4" :foreground ,background))))
   `(powerline-active2 ((t (:background "#d4d4d4" :foreground ,background))))
   `(mode-line-inactive ((t (:inverse-video unspecified
                                            :underline unspecified
                                            :foreground "#808080"
                                            :background "#1a1a1a"
                                            :box nil))))
   `(powerline-inactive1 ((t (:background "#1a1a1a" :foreground "#808080"))))
   `(powerline-inactive2 ((t (:background "#1a1a1a" :foreground "#808080"))))

   ;; js2-mode
   `(js2-function-call ((t (:inherit (font-lock-function-name-face)))))
   `(js2-function-param ((t (:foreground ,methods))))
   `(js2-jsdoc-tag ((t (:foreground ,keywords))))
   `(js2-jsdoc-type ((t (:foreground ,constants))))
   `(js2-jsdoc-value((t (:foreground ,text))))
   `(js2-object-property ((t (:foreground ,text))))
   `(js2-external-variable ((t (:foreground ,constants))))
   `(js2-error ((t (:foreground ,error :weight bold :underline t))))
   `(js2-warning ((t (:foreground ,warning :underline t))))

   ;; highlight numbers
   `(highlight-numbers-number ((t (:foreground ,numbers))))

   ;; tab-bar-mode
   `(tab-bar ((t (:inherit modeline))))
   `(tab-bar-tab ((t (:foreground ,background :background ,text))))
   `(tab-bar-tab-inactive ((t (:foreground ,text :background ,background))))
   )

  )

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; *****************************************************************************

(provide-theme 'void)

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'void-theme)

;;; void-theme.el ends here
