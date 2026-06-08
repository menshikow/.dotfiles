;;; emacs-gruvbuddy.el --- gruvbuddy theme for Emacs

;; Copyright (C) 2026 Adrian Menschikow a.k.a menshikow
;; Copyright (C) 2020 TjDevries

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;; Gruvbuddy theme by TjDevries (github.com/tjdevries). 
;; Adapted for emacs by Adrian Menschikow a.k.a. menshikow.

(deftheme emacs-gruvbuddy
  "Gruvbuddy theme for Emacs.")

(let ((gb-bg           "#111111") ; Normal bg
      (gb-fg           "#e0e0e0") ; Normal fg, Keywords, Variables, Types
      (gb-bg-alt       "#2b2b2b") ; CursorLine, LspReference
      (gb-bg-dark      "#000000") ; FloatBorder bg
      (gb-comment      "#b0b1b0") ; Comment
      (gb-red          "#cc6666") ; Number, Character, Error
      (gb-orange       "#de935f") ; Boolean, Constant
      (gb-yellow       "#f8fe7a") ; Label, PreProc, Search, Warning
      (gb-green        "#99cc99") ; String, diffadded
      (gb-blue         "#81a2be") ; Property, StatusLine
      (gb-cyan         "#8abeb7") ; Define, MatchParen
      (gb-white        "#ffffff") ; Replaced Purples (Functions, Parameters)
      (gb-visual       "#5f89ad") ; Visual bg
      (gb-status-bg    "#81a2be") ; StatusLine bg
      (gb-status-fg    "#373b41") ; StatusLine fg
      (gb-status-nc-bg "#404349") ; StatusLineNC bg
      (gb-status-nc-fg "#969896") ; StatusLineNC fg
      (gb-diff-add     "#b3f6c0") ; DiagnosticOk, Added
      (gb-diff-chg     "#8cf8f7") ; DiagnosticInfo, Changed
      (gb-diff-del     "#ffc0b9") ; Removed, ErrorMsg
      (gb-line-nr      "#282a2e") ; LineNr
      (gb-non-text     "#4e545d") ; NonText, Conceal
      (gb-pmenu-bg     "#373b41") ; Pmenu bg
      (gb-pmenu-fg     "#b4b7b4") ; Pmenu fg
      (gb-pmenu-sel-bg "#fbfead") ; PmenuSel bg
      (gb-pmenu-sel-fg "#111111") ; PmenuSel fg
      )
  
  (custom-theme-set-variables
   'emacs-gruvbuddy
   '(frame-background-mode 'dark))

  (custom-theme-set-faces
   'emacs-gruvbuddy

   ;; Basic Coloring
   `(default ((t (:foreground ,gb-fg :background ,gb-bg))))
   `(cursor ((t (:background ,gb-fg :foreground ,gb-bg))))
   `(fringe ((t (:background ,gb-bg :foreground ,gb-non-text))))
   `(vertical-border ((t (:foreground ,gb-bg-alt))))
   `(link ((t (:foreground ,gb-blue :underline t))))
   `(link-visited ((t (:foreground ,gb-cyan :underline t))))
   `(match ((t (:background ,gb-visual))))
   `(shadow ((t (:foreground ,gb-non-text))))
   `(minibuffer-prompt ((t (:foreground ,gb-yellow))))
   `(region ((t (:background ,gb-visual :foreground nil))))
   `(secondary-selection ((t (:background ,gb-bg-alt :foreground nil))))
   `(trailing-whitespace ((t (:foreground ,gb-bg :background ,gb-red))))
   `(tooltip ((t (:background ,gb-pmenu-bg :foreground ,gb-pmenu-fg))))

   ;; Font Lock (Standard Syntax Highlighting)
   `(font-lock-builtin-face ((t (:foreground ,gb-white))))
   `(font-lock-comment-face ((t (:foreground ,gb-comment))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,gb-comment))))
   `(font-lock-constant-face ((t (:foreground ,gb-orange))))
   `(font-lock-doc-face ((t (:foreground ,gb-green))))
   `(font-lock-doc-string-face ((t (:foreground ,gb-green))))
   `(font-lock-function-name-face ((t (:foreground ,gb-yellow))))
   `(font-lock-keyword-face ((t (:foreground ,gb-white))))
   `(font-lock-preprocessor-face ((t (:foreground ,gb-yellow))))
   `(font-lock-reference-face ((t (:foreground ,gb-cyan))))
   `(font-lock-string-face ((t (:foreground ,gb-green))))
   `(font-lock-type-face ((t (:foreground ,gb-blue))))
   `(font-lock-variable-name-face ((t (:foreground ,gb-white))))
   `(font-lock-warning-face ((t (:foreground ,gb-yellow))))

   ;; Tree-Sitter (Emacs 29+)
   `(treesit-font-lock-builtin-face ((t (:foreground ,gb-white))))
   `(treesit-font-lock-comment-face ((t (:foreground ,gb-comment))))
   `(treesit-font-lock-constant-face ((t (:foreground ,gb-orange))))
   `(treesit-font-lock-doc-face ((t (:foreground ,gb-green))))
   `(treesit-font-lock-function-face ((t (:foreground ,gb-yellow))))
   `(treesit-font-lock-function-call-face ((t (:foreground ,gb-yellow))))
   `(treesit-font-lock-keyword-face ((t (:foreground ,gb-white))))
   `(treesit-font-lock-number-face ((t (:foreground ,gb-red))))
   `(treesit-font-lock-operator-face ((t (:foreground ,gb-cyan))))
   `(treesit-font-lock-property-face ((t (:foreground ,gb-blue))))
   `(treesit-font-lock-punctuation-face ((t (:foreground ,gb-non-text))))
   `(treesit-font-lock-bracket-face ((t (:foreground ,gb-non-text))))
   `(treesit-font-lock-string-face ((t (:foreground ,gb-green))))
   `(treesit-font-lock-type-face ((t (:foreground ,gb-blue))))
   `(treesit-font-lock-variable-face ((t (:foreground ,gb-white))))

   ;; Line Numbers
   `(line-number ((t (:foreground ,gb-line-nr :background ,gb-bg))))
   `(line-number-current-line ((t (:foreground ,gb-yellow :background ,gb-bg))))

   ;; Highlighting
   `(highlight ((t (:background ,gb-bg-alt :foreground nil))))
   `(hl-line ((t (:background ,gb-bg-alt))))

   ;; Mode Line
   `(mode-line ((t (:background ,gb-status-bg :foreground ,gb-status-fg))))
   `(mode-line-buffer-id ((t (:background ,gb-status-bg :foreground ,gb-status-fg))))
   `(mode-line-inactive ((t (:background ,gb-status-nc-bg :foreground ,gb-status-nc-fg))))

   ;; Search
   `(isearch ((t (:foreground ,gb-line-nr :background ,gb-yellow))))
   `(isearch-fail ((t (:foreground ,gb-bg :background ,gb-red))))
   `(lazy-highlight ((t (:foreground ,gb-bg :background ,gb-orange))))

   ;; Show Paren
   `(show-paren-match ((t (:foreground ,gb-cyan :background ,gb-bg-alt))))
   `(show-paren-mismatch ((t (:foreground ,gb-red :background ,gb-bg-alt))))

   ;; Compilation / Errors
   `(compilation-info ((t (:foreground ,gb-green))))
   `(compilation-warning ((t (:foreground ,gb-yellow))))
   `(compilation-error ((t (:foreground ,gb-red))))

   ;; Flymake / Flycheck
   `(flymake-errline ((t (:underline (:style wave :color ,gb-red)))))
   `(flymake-warnline ((t (:underline (:style wave :color ,gb-yellow)))))
   `(flymake-infoline ((t (:underline (:style wave :color ,gb-diff-chg)))))
   `(flycheck-error ((t (:underline (:style wave :color ,gb-red)))))
   `(flycheck-warning ((t (:underline (:style wave :color ,gb-yellow)))))
   `(flycheck-info ((t (:underline (:style wave :color ,gb-diff-chg)))))

   ;; Dired
   `(dired-directory ((t (:foreground ,gb-blue))))
   `(dired-ignored ((t (:foreground ,gb-non-text))))

   ;; Magit / Git
   `(magit-branch-local ((t (:foreground ,gb-blue))))
   `(magit-branch-remote ((t (:foreground ,gb-green))))
   `(magit-diff-added ((t (:foreground ,gb-diff-add :background "#33423e"))))
   `(magit-diff-added-highlight ((t (:foreground ,gb-diff-add :background "#3e4a47"))))
   `(magit-diff-removed ((t (:foreground ,gb-diff-del :background "#24282f"))))
   `(magit-diff-removed-highlight ((t (:foreground ,gb-diff-del :background "#3a414c"))))
   `(magit-hash ((t (:foreground ,gb-orange))))
   `(magit-section-highlight ((t (:background ,gb-bg-alt))))

   ;; Diff
   `(diff-added ((t (:foreground ,gb-diff-add :background "#33423e"))))
   `(diff-removed ((t (:foreground ,gb-diff-del :background "#24282f"))))

   ;; Company (Completion)
   `(company-tooltip ((t (:foreground ,gb-pmenu-fg :background ,gb-pmenu-bg))))
   `(company-tooltip-selection ((t (:foreground ,gb-pmenu-sel-fg :background ,gb-pmenu-sel-bg))))
   `(company-tooltip-annotation ((t (:foreground ,gb-yellow :background ,gb-pmenu-bg))))
   `(company-scrollbar-fg ((t (:background ,gb-pmenu-fg))))
   `(company-scrollbar-bg ((t (:background ,gb-bg-dark))))

   ;; Helm / Vertico / Telescope equivalents
   `(helm-selection ((t (:background ,gb-visual))))
   `(helm-match ((t (:foreground ,gb-yellow))))
   `(vertico-current ((t (:background ,gb-visual))))
   `(ido-first-match ((t (:foreground ,gb-yellow))))
   `(ido-only-match ((t (:foreground ,gb-green))))
   `(ido-subdir ((t (:foreground ,gb-blue))))

   ;; Orderless
   `(orderless-match-face-0 ((t (:foreground ,gb-yellow))))
   `(orderless-match-face-1 ((t (:foreground ,gb-cyan))))
   `(orderless-match-face-2 ((t (:foreground ,gb-orange))))
   `(orderless-match-face-3 ((t (:foreground ,gb-green))))

   ;; Org Mode
   `(org-level-1 ((t (:foreground ,gb-blue))))
   `(org-level-2 ((t (:foreground ,gb-cyan))))
   `(org-level-3 ((t (:foreground ,gb-yellow))))
   `(org-level-4 ((t (:foreground ,gb-green))))
   `(org-todo ((t (:foreground ,gb-red))))
   `(org-done ((t (:foreground ,gb-diff-add))))
   `(org-agenda-structure ((t (:foreground ,gb-blue))))
   `(org-column ((t (:background ,gb-bg-alt))))

   ;; Tab Bar
   `(tab-bar ((t (:background ,gb-line-nr :foreground ,gb-visual))))
   `(tab-bar-tab ((t (:background ,gb-line-nr :foreground ,gb-white))))
   `(tab-bar-tab-inactive ((t (:background ,gb-non-text :foreground ,gb-bg))))

   ;; Term Colors
   `(term-color-black ((t (:foreground ,gb-bg-alt :background ,gb-bg-alt))))
   `(term-color-red ((t (:foreground ,gb-red :background ,gb-red))))
   `(term-color-green ((t (:foreground ,gb-green :background ,gb-green))))
   `(term-color-yellow ((t (:foreground ,gb-yellow :background ,gb-yellow))))
   `(term-color-blue ((t (:foreground ,gb-blue :background ,gb-blue))))
   `(term-color-magenta ((t (:foreground ,gb-white :background ,gb-white)))) 
   `(term-color-cyan ((t (:foreground ,gb-cyan :background ,gb-cyan))))
   `(term-color-white ((t (:foreground ,gb-fg :background ,gb-fg))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'emacs-gruvbuddy)

;;; emacs-gruvbuddy.el ends here
