;;; void-theme.el --- color theme  -*- lexical-binding: t; -*-

;; Author: Adrian Menschikow <github.com/menshikow>
;; Version: 0.1
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

;; Grayscale accent colors , ordered lightest to darkest
(defcustom -voidtheme-yellow "#f5f5f5" "Accent colors - lightest gray" :type 'string :group 'monokai)
(defcustom -voidtheme-orange "#d9d9d9" "Accent colors - light gray" :type 'string :group 'monokai)
(defcustom -voidtheme-red "#bfbfbf" "Accent colors - light-medium gray" :type 'string :group 'monokai)
(defcustom -voidtheme-magenta "#a6a6a6" "Accent colors - medium gray" :type 'string :group 'monokai)
(defcustom -voidtheme-blue "#8c8c8c" "Accent colors - medium-dark gray" :type 'string :group 'monokai)
(defcustom -voidtheme-green "#737373" "Accent colors - dark-medium gray" :type 'string :group 'monokai)
(defcustom -voidtheme-cyan "#595959" "Accent colors - dark gray" :type 'string :group 'monokai)
(defcustom -voidtheme-violet "#404040" "Accent colors - darkest gray" :type 'string :group 'monokai)

(let ((background "#000000")
      (gutters    "#1a1a1a")
      (gutter-fg  "#1a1a1a")
      (gutters-active "#1a1a1a")
      (builtin      "#c0c0c0")
      (selection  "#0000ff")
      (text       "#ffffff")
      (comments   "#808080")
      (punctuation "#b0b0b0")
      (keywords "#e0e0e0")
      (variables "#d0d0d0")
      (functions "#e0e0e0")
      (methods    "#d0d0d0")
      (strings    "#a0a0a0")
      (constants "#909090")
      (macros "#b0b0b0")
      (numbers "#909090")
      (white     "#c0c0c0")
      (error      "#ff0000")
      (success    "#00ff00")
      (warning "#ffea00")
      (highlight-line "#1c1c1c")
      (line-fg "#4d4d4d"))

  (custom-theme-set-faces
   'void

   ;; Default colors
   ;; *****************************************************************************

   `(default                          ((t (:foreground ,text :background ,background :weight normal))))
   `(region                           ((t (:foreground nil :background ,selection))))
   `(cursor                           ((t (:background ,white                        ))))
   `(fringe                           ((t (:background ,background   :foreground ,white))))
   `(linum                            ((t (:background ,background :foreground ,gutter-fg))))
   `(highlight ((t (:foreground nil :background ,selection))))

   ;; Font lock faces
   ;; *****************************************************************************

   `(font-lock-keyword-face           ((t (:foreground ,keywords :weight bold))))
   `(font-lock-type-face              ((t (:foreground ,punctuation))))
   `(font-lock-constant-face          ((t (:foreground ,constants))))
   `(font-lock-variable-name-face     ((t (:foreground ,variables))))
   `(font-lock-builtin-face           ((t (:foreground ,builtin))))
   `(font-lock-string-face            ((t (:foreground ,strings :slant italic))))
   `(font-lock-comment-face           ((t (:foreground ,comments :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comments :slant italic))))
   `(font-lock-doc-face               ((t (:foreground ,comments :slant italic))))
   `(font-lock-function-name-face     ((t (:foreground ,functions :weight bold))))
   `(font-lock-doc-string-face        ((t (:foreground ,strings :slant italic))))
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
   `(compilation-info ((t ,(list :foreground -voidtheme-yellow
                                 :inherit 'unspecified))))
   `(compilation-warning ((t ,(list :foreground -voidtheme-orange
                                    :bold t
                                    :inherit 'unspecified))))
   `(compilation-error ((t (:foreground ,error :weight bold :underline t))))
   `(compilation-mode-line-fail ((t ,(list :foreground error
                                           :weight 'bold
                                           :underline t
                                           :inherit 'unspecified))))
   `(compilation-mode-line-exit ((t ,(list :foreground -voidtheme-yellow
                                           :weight 'bold
                                           :inherit 'unspecified))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,highlight-line))))
   `(hl-line-face ((t (:background ,highlight-line))))

   ;; rainbow-delimiters (grayscale, cycling lightest to darkest)
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,-voidtheme-yellow))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,-voidtheme-orange))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,-voidtheme-red))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,-voidtheme-magenta))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,-voidtheme-blue))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,-voidtheme-green))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,-voidtheme-cyan))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,-voidtheme-violet))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,-voidtheme-yellow))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,-voidtheme-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,-voidtheme-red))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,-voidtheme-magenta))))

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
                                   :background ,text
                                   :box nil))))
   `(powerline-active1 ((t (:background ,text :foreground ,background))))
   `(powerline-active2 ((t (:background ,text :foreground ,background))))

   `(mode-line-inactive ((t (:inverse-video unspecified
                                            :underline unspecified
                                            :foreground ,text
                                            :background ,background
                                            :box nil))))
   `(powerline-inactive1 ((t (:background ,background :foreground ,text))))
   `(powerline-inactive2 ((t (:background ,background :foreground ,text))))

   ;; js2-mode
   `(js2-function-call ((t (:inherit (font-lock-function-name-face)))))
   `(js2-function-param ((t (:foreground ,text))))
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

  (custom-theme-set-variables
   'void
   '(linum-format " %5i ")
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

;;; Code:

(unless (>= emacs-major-version 24)
  (error "The void theme requires Emacs 24 or later!"))

(deftheme void "The void color theme")

;; Grayscale accent colors (formerly Monokai colors), ordered lightest to darkest
(defcustom -voidtheme-yellow "#f5f5f5" "Accent colors - lightest gray" :type 'string :group 'monokai)
(defcustom -voidtheme-orange "#d9d9d9" "Accent colors - light gray" :type 'string :group 'monokai)
(defcustom -voidtheme-red "#bfbfbf" "Accent colors - light-medium gray" :type 'string :group 'monokai)
(defcustom -voidtheme-magenta "#a6a6a6" "Accent colors - medium gray" :type 'string :group 'monokai)
(defcustom -voidtheme-blue "#8c8c8c" "Accent colors - medium-dark gray" :type 'string :group 'monokai)
(defcustom -voidtheme-green "#737373" "Accent colors - dark-medium gray" :type 'string :group 'monokai)
(defcustom -voidtheme-cyan "#595959" "Accent colors - dark gray" :type 'string :group 'monokai)
(defcustom -voidtheme-violet "#404040" "Accent colors - darkest gray" :type 'string :group 'monokai)

(let ((background "#000000")
      (gutters    "#1a1a1a")
      (gutter-fg  "#1a1a1a")
      (gutters-active "#1a1a1a")
      (builtin      "#c0c0c0")
      (selection  "#0000ff")
      (text       "#ffffff")
      (comments   "#808080")
      (punctuation "#b0b0b0")
      (keywords "#e0e0e0")
      (variables "#d0d0d0")
      (functions "#e0e0e0")
      (methods    "#d0d0d0")
      (strings    "#a0a0a0")
      (constants "#909090")
      (macros "#b0b0b0")
      (numbers "#909090")
      (white     "#c0c0c0")
      (error "#ffffff")
      (warning "#cccccc")
      (highlight-line "#1c1c1c")
      (line-fg "#4d4d4d"))

  (custom-theme-set-faces
   'void

   ;; Default colors
   ;; *****************************************************************************

   `(default                          ((t (:foreground ,text :background ,background :weight normal))))
   `(region                           ((t (:foreground nil :background ,selection))))
   `(cursor                           ((t (:background ,white                        ))))
   `(fringe                           ((t (:background ,background   :foreground ,white))))
   `(linum                            ((t (:background ,background :foreground ,gutter-fg))))
   `(highlight ((t (:foreground nil :background ,selection))))

   ;; Font lock faces
   ;; *****************************************************************************

   `(font-lock-keyword-face           ((t (:foreground ,keywords :weight bold))))
   `(font-lock-type-face              ((t (:foreground ,punctuation))))
   `(font-lock-constant-face          ((t (:foreground ,constants))))
   `(font-lock-variable-name-face     ((t (:foreground ,variables))))
   `(font-lock-builtin-face           ((t (:foreground ,builtin))))
   `(font-lock-string-face            ((t (:foreground ,strings :slant italic))))
   `(font-lock-comment-face           ((t (:foreground ,comments :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comments :slant italic))))
   `(font-lock-doc-face               ((t (:foreground ,comments :slant italic))))
   `(font-lock-function-name-face     ((t (:foreground ,functions :weight bold))))
   `(font-lock-doc-string-face        ((t (:foreground ,strings :slant italic))))
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
   `(compilation-info ((t ,(list :foreground -voidtheme-yellow
                                 :inherit 'unspecified))))
   `(compilation-warning ((t ,(list :foreground -voidtheme-orange
                                    :bold t
                                    :inherit 'unspecified))))
   `(compilation-error ((t (:foreground ,error :weight bold :underline t))))
   `(compilation-mode-line-fail ((t ,(list :foreground error
                                           :weight 'bold
                                           :underline t
                                           :inherit 'unspecified))))
   `(compilation-mode-line-exit ((t ,(list :foreground -voidtheme-yellow
                                           :weight 'bold
                                           :inherit 'unspecified))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,highlight-line))))
   `(hl-line-face ((t (:background ,highlight-line))))

   ;; rainbow-delimiters (grayscale, cycling lightest to darkest)
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,-voidtheme-yellow))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,-voidtheme-orange))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,-voidtheme-red))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,-voidtheme-magenta))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,-voidtheme-blue))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,-voidtheme-green))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,-voidtheme-cyan))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,-voidtheme-violet))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,-voidtheme-yellow))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,-voidtheme-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,-voidtheme-red))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,-voidtheme-magenta))))

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
                                   :background ,text
                                   :box nil))))
   `(powerline-active1 ((t (:background ,text :foreground ,background))))
   `(powerline-active2 ((t (:background ,text :foreground ,background))))

   `(mode-line-inactive ((t (:inverse-video unspecified
                                            :underline unspecified
                                            :foreground ,text
                                            :background ,background
                                            :box nil))))
   `(powerline-inactive1 ((t (:background ,background :foreground ,text))))
   `(powerline-inactive2 ((t (:background ,background :foreground ,text))))

   ;; js2-mode
   `(js2-function-call ((t (:inherit (font-lock-function-name-face)))))
   `(js2-function-param ((t (:foreground ,text))))
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

  (custom-theme-set-variables
   'void
   '(linum-format " %5i ")
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
