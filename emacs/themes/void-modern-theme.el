;;; void-modern-theme.el --- Void color theme (modern structure)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Adrian Menschikow <github.com/menshikow>
;; Version: 0.1
;; Filename: void-modern-theme.el
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/
;; License: MIT

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
;; Void monochrome color theme with modern file structure.
;; Based on void-theme by Adrian Menschikow.
;; Structure adapted from void-gruber-theme, covering all major packages.

(deftheme void-modern "Void monochrome color theme")

(let (;; Void-modern UI colors (from void-theme palette)
      (void-modern-fg        "#cccccc")
      (void-modern-fg+1      "#ffffff")
      (void-modern-fg+2      "#ffffff")
      (void-modern-white     "#ffffff")
      (void-modern-black     "#000000")
      (void-modern-bg-1      "#000000")
      (void-modern-bg        "#000000")
      (void-modern-bg+1      "#1a1a1a")
      (void-modern-bg+2      "#4d4d4d")
      (void-modern-bg+3      "#484848")
      (void-modern-bg+4      "#4d4d4d")
      (void-modern-red-1     "#ff4444")
      (void-modern-red       "#ff4444")
      (void-modern-red+1     "#ff4444")
      (void-modern-green     "#8fdf8f")
      (void-modern-yellow    "#cccccc")
      (void-modern-brown     "#a0a0a0")
      (void-modern-quartz    "#a0a0a0")
      (void-modern-niagara-2 "#a0a0a0")
      (void-modern-niagara-1 "#888888")
      (void-modern-niagara   "#a0a0a0")
      (void-modern-wisteria  "#a0a0a0")

      ;; Void syntax colors
      (void-builtin      "#a0a0a0")
      (void-text         "#cccccc")
      (void-comments     "#6b9f6b")
      (void-punctuation  "#a0a0a0")
      (void-keywords     "#ffffff")
      (void-variables    "#888888")
      (void-functions    "#e8e8e8")
      (void-methods      "#b8b8b8")
      (void-strings      "#b08f5a")
      (void-constants    "#888888")
      (void-macros       "#888888")
      (void-numbers      "#888888")
      (void-warning      "#cccccc")
      (void-error        "#ff4444")
      (void-green        "#8fdf8f"))

  (custom-theme-set-variables
   'void-modern
   '(frame-background-mode (quote dark)))

  (custom-theme-set-faces
   'void-modern

   ;; Agda2
   `(agda2-highlight-datatype-face ((t (:foreground ,void-modern-quartz))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,void-modern-quartz))))
   `(agda2-highlight-function-face ((t (:foreground ,void-modern-niagara))))
   `(agda2-highlight-keyword-face ((t ,(list :foreground void-modern-yellow
                                              :bold t))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,void-modern-green))))
   `(agda2-highlight-number-face ((t (:foreground ,void-modern-wisteria))))

   ;; AUCTeX
   `(font-latex-bold-face ((t (:foreground ,void-modern-quartz :bold t))))
   `(font-latex-italic-face ((t (:foreground ,void-modern-quartz :italic t))))
   `(font-latex-math-face ((t (:foreground ,void-modern-green))))
   `(font-latex-sectioning-5-face ((t ,(list :foreground void-modern-niagara
                                              :bold t))))
   `(font-latex-slide-title-face ((t (:foreground ,void-modern-niagara))))
   `(font-latex-string-face ((t (:foreground ,void-modern-green))))
   `(font-latex-warning-face ((t (:foreground ,void-modern-red))))

   ;; Basic Coloring (or Uncategorized)
   `(border ((t ,(list :background void-modern-bg-1
                        :foreground void-modern-bg+2))))
   `(cursor ((t (:background ,void-modern-white))))
   `(default ((t ,(list :foreground void-modern-fg
                         :background void-modern-bg))))
   `(fringe ((t ,(list :background nil
                        :foreground void-modern-bg+2))))
   `(vertical-border ((t ,(list :foreground void-modern-bg+2))))
   `(link ((t (:foreground ,void-modern-niagara :underline t))))
   `(link-visited ((t (:foreground ,void-modern-wisteria :underline t))))
   `(match ((t (:background ,void-modern-bg+4))))
   `(shadow ((t (:foreground ,void-modern-bg+4))))
   `(minibuffer-prompt ((t (:foreground ,void-modern-niagara))))
   `(region ((t (:background ,void-modern-bg+3 :foreground nil))))
   `(secondary-selection ((t ,(list :background void-modern-bg+3
                                     :foreground nil))))
   `(trailing-whitespace ((t ,(list :foreground void-modern-black
                                     :background void-modern-red))))
   `(tooltip ((t ,(list :background void-modern-bg+4
                         :foreground void-modern-white))))

   ;; Calendar
   `(holiday-face ((t (:foreground ,void-modern-red))))

   ;; Compilation
   `(compilation-info ((t ,(list :foreground void-green
                                  :inherit 'unspecified))))
   `(compilation-warning ((t ,(list :foreground void-modern-brown
                                     :bold t
                                     :inherit 'unspecified))))
   `(compilation-error ((t (:foreground ,void-error))))
   `(compilation-mode-line-fail ((t ,(list :foreground void-error
                                            :weight 'bold
                                            :inherit 'unspecified))))
   `(compilation-mode-line-exit ((t ,(list :foreground void-green
                                            :weight 'bold
                                            :inherit 'unspecified))))

   ;; Completion
   `(completions-annotations ((t (:inherit 'shadow))))

   ;; Custom
   `(custom-state ((t (:foreground ,void-modern-green))))

   ;; Diff
   `(diff-removed ((t ,(list :foreground void-modern-red+1
                              :background nil))))
   `(diff-added ((t ,(list :foreground void-modern-green
                            :background nil))))

   ;; Dired
   `(dired-directory ((t (:foreground ,void-modern-white :weight bold))))
   `(dired-subtree-depth-1-face ((t (:background nil :foreground ,void-modern-fg))))
   `(dired-subtree-depth-2-face ((t (:background nil :foreground ,void-modern-fg+1))))
   `(dired-subtree-depth-3-face ((t (:background nil :foreground ,void-modern-fg))))
   `(dired-ignored ((t ,(list :foreground void-modern-quartz
                               :inherit 'unspecified))))

   ;; Ebrowse
   `(ebrowse-root-class ((t (:foreground ,void-modern-niagara :weight bold))))
   `(ebrowse-progress ((t (:background ,void-modern-niagara))))

   ;; Egg
   `(egg-branch ((t (:foreground ,void-modern-yellow))))
   `(egg-branch-mono ((t (:foreground ,void-modern-yellow))))
   `(egg-diff-add ((t (:foreground ,void-modern-green))))
   `(egg-diff-del ((t (:foreground ,void-modern-red))))
   `(egg-diff-file-header ((t (:foreground ,void-modern-wisteria))))
   `(egg-help-header-1 ((t (:foreground ,void-modern-yellow))))
   `(egg-help-header-2 ((t (:foreground ,void-modern-niagara))))
   `(egg-log-HEAD-name ((t (:box (:color ,void-modern-fg)))))
   `(egg-reflog-mono ((t (:foreground ,void-modern-niagara-1))))
   `(egg-section-title ((t (:foreground ,void-modern-yellow))))
   `(egg-text-base ((t (:foreground ,void-modern-fg))))
   `(egg-term ((t (:foreground ,void-modern-yellow))))

   ;; ERC
   `(erc-notice-face ((t (:foreground ,void-modern-wisteria))))
   `(erc-timestamp-face ((t (:foreground ,void-modern-green))))
   `(erc-input-face ((t (:foreground ,void-modern-red+1))))
   `(erc-my-nick-face ((t (:foreground ,void-modern-red+1))))

   ;; EShell
   `(eshell-ls-backup ((t (:foreground ,void-modern-quartz))))
   `(eshell-ls-directory ((t (:foreground ,void-modern-niagara))))
   `(eshell-ls-executable ((t (:foreground ,void-modern-green))))
   `(eshell-ls-symlink ((t (:foreground ,void-modern-yellow))))

   ;; Font Lock — syntax highlighting from Void
   `(font-lock-builtin-face           ((t (:foreground ,void-builtin))))
   `(font-lock-comment-face           ((t (:foreground ,void-comments))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,void-comments))))
   `(font-lock-constant-face          ((t (:foreground ,void-constants))))
   `(font-lock-doc-face               ((t (:foreground ,void-comments))))
   `(font-lock-doc-string-face        ((t (:foreground ,void-strings))))
   `(font-lock-function-name-face     ((t (:foreground ,void-functions :weight bold))))
   `(font-lock-keyword-face           ((t (:foreground ,void-keywords :weight bold))))
   `(font-lock-preprocessor-face      ((t (:foreground ,void-macros))))
   `(font-lock-reference-face         ((t (:foreground ,void-constants))))
   `(font-lock-string-face            ((t (:foreground ,void-strings))))
   `(font-lock-type-face              ((t (:foreground ,void-punctuation))))
   `(font-lock-variable-name-face     ((t (:foreground ,void-variables))))
   `(font-lock-warning-face           ((t (:foreground ,void-warning :weight bold :underline t))))

   ;; Flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,void-modern-red)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,void-modern-red :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,void-modern-yellow)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,void-modern-yellow :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,void-modern-green)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,void-modern-green :weight bold :underline t))))

   ;; Flyspell
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,void-modern-red) :inherit unspecified))
      (t (:foreground ,void-modern-red :weight bold :underline t))))
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,void-modern-yellow) :inherit unspecified))
      (t (:foreground ,void-modern-yellow :weight bold :underline t))))

   ;; Helm
   `(helm-candidate-number ((t ,(list :background void-modern-bg+2
                                       :foreground void-modern-yellow
                                       :bold t))))
   `(helm-ff-directory ((t ,(list :foreground void-modern-niagara
                                   :background void-modern-bg
                                   :bold t))))
   `(helm-ff-executable ((t (:foreground ,void-modern-green))))
   `(helm-ff-file ((t (:foreground ,void-modern-fg :inherit unspecified))))
   `(helm-ff-invalid-symlink ((t ,(list :foreground void-modern-bg
                                         :background void-modern-red))))
   `(helm-ff-symlink ((t (:foreground ,void-modern-yellow :bold t))))
   `(helm-selection-line ((t (:background ,void-modern-bg+1))))
   `(helm-selection ((t (:background ,void-modern-bg+1 :underline nil))))
   `(helm-source-header ((t ,(list :foreground void-modern-yellow
                                    :background void-modern-bg
                                    :box (list :line-width -1
                                               :style 'released-button)))))

   ;; Ido
   `(ido-first-match ((t (:foreground ,void-modern-yellow :bold nil))))
   `(ido-only-match ((t (:foreground ,void-modern-brown :weight bold))))
   `(ido-subdir ((t (:foreground ,void-modern-niagara :weight bold))))

   ;; Info
   `(info-xref ((t (:foreground ,void-modern-niagara))))
   `(info-visited ((t (:foreground ,void-modern-wisteria))))

   ;; Jabber
   `(jabber-chat-prompt-foreign ((t ,(list :foreground void-modern-quartz
                                            :bold nil))))
   `(jabber-chat-prompt-local ((t (:foreground ,void-modern-yellow))))
   `(jabber-chat-prompt-system ((t (:foreground ,void-modern-green))))
   `(jabber-rare-time-face ((t (:foreground ,void-modern-green))))
   `(jabber-roster-user-online ((t (:foreground ,void-modern-green))))
   `(jabber-activity-face ((t (:foreground ,void-modern-red))))
   `(jabber-activity-personal-face ((t (:foreground ,void-modern-yellow :bold t))))

   ;; Line Highlighting
   `(highlight ((t (:background ,void-modern-bg+1 :foreground nil))))
   `(highlight-current-line-face ((t ,(list :background void-modern-bg+1
                                             :foreground nil))))

   ;; line numbers
   `(line-number ((t (:inherit default :foreground ,void-modern-bg+4))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,void-modern-yellow))))

   ;; Linum
   `(linum ((t (:foreground ,void-modern-quartz
                             :background ,void-modern-bg))))

   ;; Magit
   `(magit-branch ((t (:foreground ,void-modern-niagara))))
   `(magit-diff-hunk-header ((t (:background ,void-modern-bg+2))))
   `(magit-diff-file-header ((t (:background ,void-modern-bg+4))))
   `(magit-log-sha1 ((t (:foreground ,void-modern-red+1))))
   `(magit-log-author ((t (:foreground ,void-modern-brown))))
   `(magit-log-head-label-remote ((t ,(list :foreground void-modern-green
                                             :background void-modern-bg+1))))
   `(magit-log-head-label-local ((t ,(list :foreground void-modern-niagara
                                            :background void-modern-bg+1))))
   `(magit-log-head-label-tags ((t ,(list :foreground void-modern-yellow
                                           :background void-modern-bg+1))))
   `(magit-log-head-label-head ((t ,(list :foreground void-modern-fg
                                           :background void-modern-bg+1))))
   `(magit-item-highlight ((t (:background ,void-modern-bg+1))))
   `(magit-tag ((t ,(list :foreground void-modern-yellow
                           :background void-modern-bg))))
   `(magit-blame-heading ((t ,(list :background void-modern-bg+1
                                     :foreground void-modern-fg))))

   ;; Message
   `(message-header-name ((t (:foreground ,void-modern-green))))

   ;; Mode Line
   `(mode-line ((t ,(list :background void-modern-bg+1
                           :foreground void-modern-white))))
   `(mode-line-buffer-id ((t ,(list :background void-modern-bg+1
                                     :foreground void-modern-white))))
   `(mode-line-inactive ((t ,(list :background void-modern-bg+1
                                    :foreground void-modern-quartz))))

   ;; Neo Dir
   `(neo-dir-link-face ((t (:foreground ,void-modern-niagara))))

   ;; Org Mode
   `(org-agenda-structure ((t (:foreground ,void-modern-niagara))))
   `(org-column ((t (:background ,void-modern-bg-1))))
   `(org-column-title ((t (:background ,void-modern-bg-1 :underline t :weight bold))))
   `(org-done ((t (:foreground ,void-modern-green))))
   `(org-todo ((t (:foreground ,void-modern-red-1))))
   `(org-upcoming-deadline ((t (:foreground ,void-modern-yellow))))

   ;; Search
   `(isearch ((t ,(list :foreground void-modern-black
                         :background void-modern-fg+2))))
   `(isearch-fail ((t ,(list :foreground void-modern-black
                              :background void-modern-red))))
   `(isearch-lazy-highlight-face ((t ,(list
                                        :foreground void-modern-fg+1
                                        :background void-modern-niagara-1))))

   ;; Sh
   `(sh-quoted-exec ((t (:foreground ,void-modern-red+1))))

   ;; Show Paren
   `(show-paren-match-face ((t (:background ,void-modern-bg+4))))
   `(show-paren-mismatch-face ((t (:background ,void-modern-red-1))))

   ;; Slime
   `(slime-repl-inputed-output-face ((t (:foreground ,void-modern-red))))

   ;; Tuareg
   `(tuareg-font-lock-governing-face ((t (:foreground ,void-modern-yellow))))

   ;; Speedbar
   `(speedbar-directory-face ((t ,(list :foreground void-modern-niagara
                                         :weight 'bold))))
   `(speedbar-file-face ((t (:foreground ,void-modern-fg))))
   `(speedbar-highlight-face ((t (:background ,void-modern-bg+1))))
   `(speedbar-selected-face ((t (:foreground ,void-modern-red))))
   `(speedbar-tag-face ((t (:foreground ,void-modern-yellow))))

   ;; Which Function
   `(which-func ((t (:foreground ,void-modern-wisteria))))

   ;; Whitespace
   `(whitespace-space ((t ,(list :background void-modern-bg
                                  :foreground void-modern-bg+1))))
   `(whitespace-tab ((t ,(list :background void-modern-bg
                                :foreground void-modern-bg+1))))
   `(whitespace-hspace ((t ,(list :background void-modern-bg
                                   :foreground void-modern-bg+2))))
   `(whitespace-line ((t ,(list :background void-modern-bg+2
                                 :foreground void-modern-red+1))))
   `(whitespace-newline ((t ,(list :background void-modern-bg
                                    :foreground void-modern-bg+2))))
   `(whitespace-trailing ((t ,(list :background void-modern-red
                                     :foreground void-modern-red))))
   `(whitespace-empty ((t ,(list :background void-modern-yellow
                                  :foreground void-modern-yellow))))
   `(whitespace-indentation ((t ,(list :background void-modern-yellow
                                        :foreground void-modern-red))))
   `(whitespace-space-after-tab ((t ,(list :background void-modern-yellow
                                            :foreground void-modern-yellow))))
   `(whitespace-space-before-tab ((t ,(list :background void-modern-brown
                                             :foreground void-modern-brown))))

   ;; tab-bar
   `(tab-bar ((t (:background ,void-modern-bg+1 :foreground ,void-modern-bg+4))))
   `(tab-bar-tab ((t (:background nil :foreground ,void-modern-yellow :weight bold))))
   `(tab-bar-tab-inactive ((t (:background nil))))

   ;; vterm / ansi-term
   `(term-color-black ((t (:foreground ,void-modern-bg+3 :background ,void-modern-bg+4))))
   `(term-color-red ((t (:foreground ,void-modern-red-1 :background ,void-modern-red-1))))
   `(term-color-green ((t (:foreground ,void-modern-green :background ,void-modern-green))))
   `(term-color-blue ((t (:foreground ,void-modern-niagara :background ,void-modern-niagara))))
   `(term-color-yellow ((t (:foreground ,void-modern-yellow :background ,void-modern-yellow))))
   `(term-color-magenta ((t (:foreground ,void-modern-wisteria :background ,void-modern-wisteria))))
   `(term-color-cyan ((t (:foreground ,void-modern-quartz :background ,void-modern-quartz))))
   `(term-color-white ((t (:foreground ,void-modern-fg :background ,void-modern-white))))

   ;; company-mode
   `(company-tooltip ((t (:foreground ,void-modern-fg :background ,void-modern-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,void-modern-brown :background ,void-modern-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,void-modern-brown :background ,void-modern-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,void-modern-fg :background ,void-modern-bg-1))))
   `(company-tooltip-mouse ((t (:background ,void-modern-bg-1))))
   `(company-tooltip-common ((t (:foreground ,void-modern-green))))
   `(company-tooltip-common-selection ((t (:foreground ,void-modern-green))))
   `(company-scrollbar-fg ((t (:background ,void-modern-bg-1))))
   `(company-scrollbar-bg ((t (:background ,void-modern-bg+2))))
   `(company-preview ((t (:background ,void-modern-green))))
   `(company-preview-common ((t (:foreground ,void-modern-green :background ,void-modern-bg-1))))

   ;; Proof General
   `(proof-locked-face ((t (:background ,void-modern-niagara-2))))

   ;; Orderless
   `(orderless-match-face-0 ((t (:foreground ,void-modern-yellow))))
   `(orderless-match-face-1 ((t (:foreground ,void-modern-green))))
   `(orderless-match-face-2 ((t (:foreground ,void-modern-brown))))
   `(orderless-match-face-3 ((t (:foreground ,void-modern-quartz))))

   ;; js2-mode — syntax from Void
   `(js2-function-call    ((t (:inherit (font-lock-function-name-face)))))
   `(js2-function-param   ((t (:foreground ,void-methods))))
   `(js2-jsdoc-tag        ((t (:foreground ,void-keywords))))
   `(js2-jsdoc-type       ((t (:foreground ,void-constants))))
   `(js2-jsdoc-value      ((t (:foreground ,void-text))))
   `(js2-object-property  ((t (:foreground ,void-text))))
   `(js2-external-variable ((t (:foreground ,void-constants))))
   `(js2-error            ((t (:foreground ,void-error :weight bold :underline t))))
   `(js2-warning          ((t (:foreground ,void-warning :underline t))))

   ;; highlight-numbers — syntax from Void
   `(highlight-numbers-number ((t (:foreground ,void-numbers))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,void-modern-bg+1))))
   `(hl-line-face ((t (:background ,void-modern-bg+1))))

   ;; powerline
   `(powerline-active1 ((t (:background ,void-modern-bg+1 :foreground ,void-modern-white))))
   `(powerline-active2 ((t (:background ,void-modern-bg+1 :foreground ,void-modern-white))))
   `(powerline-inactive1 ((t (:background ,void-modern-bg+1 :foreground ,void-modern-quartz))))
   `(powerline-inactive2 ((t (:background ,void-modern-bg+1 :foreground ,void-modern-quartz))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'void-modern)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; void-modern-theme.el ends here
