;;; void-gruber-theme.el --- Void syntax + Gruber Darker UI theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: combined from gruber-darker-theme (Alexey Kutepov) and void-theme (Adrian Menschikow)

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
;; Void syntax highlighting colors combined with Gruber Darker UI/background colors.
;; Syntax (font-lock) colors from void-theme by Adrian Menschikow.
;; All other UI colors from gruber-darker-theme by Alexey Kutepov / Jason Blevins.

(deftheme void-gruber "Void syntax + Gruber Darker UI theme")

(let ((gruber-darker-fg        "#e4e4ef")
      (gruber-darker-fg+1      "#f4f4ff")
      (gruber-darker-fg+2      "#f5f5f5")
      (gruber-darker-white     "#ffffff")
      (gruber-darker-black     "#000000")
      (gruber-darker-bg-1      "#101010")
      (gruber-darker-bg        "#181818")
      (gruber-darker-bg+1      "#282828")
      (gruber-darker-bg+2      "#453d41")
      (gruber-darker-bg+3      "#484848")
      (gruber-darker-bg+4      "#52494e")
      (gruber-darker-red-1     "#c73c3f")
      (gruber-darker-red       "#f43841")
      (gruber-darker-red+1     "#ff4f58")
      (gruber-darker-green     "#73c936")
      (gruber-darker-yellow    "#ffdd33")
      (gruber-darker-brown     "#cc8c3c")
      (gruber-darker-quartz    "#95a99f")
      (gruber-darker-niagara-2 "#303540")
      (gruber-darker-niagara-1 "#565f73")
      (gruber-darker-niagara   "#96a6c8")
      (gruber-darker-wisteria  "#9e95c7")

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
   'void-gruber
   '(frame-background-mode (quote dark)))

  (custom-theme-set-faces
   'void-gruber

   ;; Agda2
   `(agda2-highlight-datatype-face ((t (:foreground ,gruber-darker-quartz))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,gruber-darker-quartz))))
   `(agda2-highlight-function-face ((t (:foreground ,gruber-darker-niagara))))
   `(agda2-highlight-keyword-face ((t ,(list :foreground gruber-darker-yellow
                                             :bold t))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,gruber-darker-green))))
   `(agda2-highlight-number-face ((t (:foreground ,gruber-darker-wisteria))))

   ;; AUCTeX
   `(font-latex-bold-face ((t (:foreground ,gruber-darker-quartz :bold t))))
   `(font-latex-italic-face ((t (:foreground ,gruber-darker-quartz :italic t))))
   `(font-latex-math-face ((t (:foreground ,gruber-darker-green))))
   `(font-latex-sectioning-5-face ((t ,(list :foreground gruber-darker-niagara
                                             :bold t))))
   `(font-latex-slide-title-face ((t (:foreground ,gruber-darker-niagara))))
   `(font-latex-string-face ((t (:foreground ,gruber-darker-green))))
   `(font-latex-warning-face ((t (:foreground ,gruber-darker-red))))

   ;; Basic Coloring (or Uncategorized)
   `(border ((t ,(list :background gruber-darker-bg-1
                       :foreground gruber-darker-bg+2))))
   `(cursor ((t (:background ,gruber-darker-white))))
   `(default ((t ,(list :foreground gruber-darker-fg
                        :background gruber-darker-bg))))
   `(fringe ((t ,(list :background nil
                       :foreground gruber-darker-bg+2))))
   `(vertical-border ((t ,(list :foreground gruber-darker-bg+2))))
   `(link ((t (:foreground ,gruber-darker-niagara :underline t))))
   `(link-visited ((t (:foreground ,gruber-darker-wisteria :underline t))))
   `(match ((t (:background ,gruber-darker-bg+4))))
   `(shadow ((t (:foreground ,gruber-darker-bg+4))))
   `(minibuffer-prompt ((t (:foreground ,gruber-darker-niagara))))
   `(region ((t (:background ,gruber-darker-bg+3 :foreground nil))))
   `(secondary-selection ((t ,(list :background gruber-darker-bg+3
                                    :foreground nil))))
   `(trailing-whitespace ((t ,(list :foreground gruber-darker-black
                                    :background gruber-darker-red))))
   `(tooltip ((t ,(list :background gruber-darker-bg+4
                        :foreground gruber-darker-white))))

   ;; Calendar
   `(holiday-face ((t (:foreground ,gruber-darker-red))))

   ;; Compilation
   `(compilation-info ((t ,(list :foreground void-green
                                 :inherit 'unspecified))))
   `(compilation-warning ((t ,(list :foreground gruber-darker-brown
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
   `(custom-state ((t (:foreground ,gruber-darker-green))))

   ;; Diff
   `(diff-removed ((t ,(list :foreground gruber-darker-red+1
                             :background nil))))
   `(diff-added ((t ,(list :foreground gruber-darker-green
                           :background nil))))

   ;; Dired
   `(dired-directory ((t (:foreground ,gruber-darker-niagara :weight bold))))
   `(dired-ignored ((t ,(list :foreground gruber-darker-quartz
                              :inherit 'unspecified))))

   ;; Ebrowse
   `(ebrowse-root-class ((t (:foreground ,gruber-darker-niagara :weight bold))))
   `(ebrowse-progress ((t (:background ,gruber-darker-niagara))))

   ;; Egg
   `(egg-branch ((t (:foreground ,gruber-darker-yellow))))
   `(egg-branch-mono ((t (:foreground ,gruber-darker-yellow))))
   `(egg-diff-add ((t (:foreground ,gruber-darker-green))))
   `(egg-diff-del ((t (:foreground ,gruber-darker-red))))
   `(egg-diff-file-header ((t (:foreground ,gruber-darker-wisteria))))
   `(egg-help-header-1 ((t (:foreground ,gruber-darker-yellow))))
   `(egg-help-header-2 ((t (:foreground ,gruber-darker-niagara))))
   `(egg-log-HEAD-name ((t (:box (:color ,gruber-darker-fg)))))
   `(egg-reflog-mono ((t (:foreground ,gruber-darker-niagara-1))))
   `(egg-section-title ((t (:foreground ,gruber-darker-yellow))))
   `(egg-text-base ((t (:foreground ,gruber-darker-fg))))
   `(egg-term ((t (:foreground ,gruber-darker-yellow))))

   ;; ERC
   `(erc-notice-face ((t (:foreground ,gruber-darker-wisteria))))
   `(erc-timestamp-face ((t (:foreground ,gruber-darker-green))))
   `(erc-input-face ((t (:foreground ,gruber-darker-red+1))))
   `(erc-my-nick-face ((t (:foreground ,gruber-darker-red+1))))

   ;; EShell
   `(eshell-ls-backup ((t (:foreground ,gruber-darker-quartz))))
   `(eshell-ls-directory ((t (:foreground ,gruber-darker-niagara))))
   `(eshell-ls-executable ((t (:foreground ,gruber-darker-green))))
   `(eshell-ls-symlink ((t (:foreground ,gruber-darker-yellow))))

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
       (:underline (:style wave :color ,gruber-darker-red)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,gruber-darker-red :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,gruber-darker-yellow)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:forground ,gruber-darker-yellow :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,gruber-darker-green)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:forground ,gruber-darker-green :weight bold :underline t))))

   ;; Flyspell
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,gruber-darker-red) :inherit unspecified))
      (t (:foreground ,gruber-darker-red :weight bold :underline t))))
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,gruber-darker-yellow) :inherit unspecified))
      (t (:foreground ,gruber-darker-yellow :weight bold :underline t))))

   ;; Helm
   `(helm-candidate-number ((t ,(list :background gruber-darker-bg+2
                                      :foreground gruber-darker-yellow
                                      :bold t))))
   `(helm-ff-directory ((t ,(list :foreground gruber-darker-niagara
                                  :background gruber-darker-bg
                                  :bold t))))
   `(helm-ff-executable ((t (:foreground ,gruber-darker-green))))
   `(helm-ff-file ((t (:foreground ,gruber-darker-fg :inherit unspecified))))
   `(helm-ff-invalid-symlink ((t ,(list :foreground gruber-darker-bg
                                        :background gruber-darker-red))))
   `(helm-ff-symlink ((t (:foreground ,gruber-darker-yellow :bold t))))
   `(helm-selection-line ((t (:background ,gruber-darker-bg+1))))
   `(helm-selection ((t (:background ,gruber-darker-bg+1 :underline nil))))
   `(helm-source-header ((t ,(list :foreground gruber-darker-yellow
                                   :background gruber-darker-bg
                                   :box (list :line-width -1
                                              :style 'released-button)))))

   ;; Ido
   `(ido-first-match ((t (:foreground ,gruber-darker-yellow :bold nil))))
   `(ido-only-match ((t (:foreground ,gruber-darker-brown :weight bold))))
   `(ido-subdir ((t (:foreground ,gruber-darker-niagara :weight bold))))

   ;; Info
   `(info-xref ((t (:foreground ,gruber-darker-niagara))))
   `(info-visited ((t (:foreground ,gruber-darker-wisteria))))

   ;; Jabber
   `(jabber-chat-prompt-foreign ((t ,(list :foreground gruber-darker-quartz
                                           :bold nil))))
   `(jabber-chat-prompt-local ((t (:foreground ,gruber-darker-yellow))))
   `(jabber-chat-prompt-system ((t (:foreground ,gruber-darker-green))))
   `(jabber-rare-time-face ((t (:foreground ,gruber-darker-green))))
   `(jabber-roster-user-online ((t (:foreground ,gruber-darker-green))))
   `(jabber-activity-face ((t (:foreground ,gruber-darker-red))))
   `(jabber-activity-personal-face ((t (:foreground ,gruber-darker-yellow :bold t))))

   ;; Line Highlighting
   `(highlight ((t (:background ,gruber-darker-bg+1 :foreground nil))))
   `(highlight-current-line-face ((t ,(list :background gruber-darker-bg+1
                                            :foreground nil))))

   ;; line numbers
   `(line-number ((t (:inherit default :foreground ,gruber-darker-bg+4))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,gruber-darker-yellow))))

   ;; Linum
   `(linum ((t (:foreground ,gruber-darker-quartz
                            :background ,gruber-darker-bg))))

   ;; Magit
   `(magit-branch ((t (:foreground ,gruber-darker-niagara))))
   `(magit-diff-hunk-header ((t (:background ,gruber-darker-bg+2))))
   `(magit-diff-file-header ((t (:background ,gruber-darker-bg+4))))
   `(magit-log-sha1 ((t (:foreground ,gruber-darker-red+1))))
   `(magit-log-author ((t (:foreground ,gruber-darker-brown))))
   `(magit-log-head-label-remote ((t ,(list :foreground gruber-darker-green
                                            :background gruber-darker-bg+1))))
   `(magit-log-head-label-local ((t ,(list :foreground gruber-darker-niagara
                                           :background gruber-darker-bg+1))))
   `(magit-log-head-label-tags ((t ,(list :foreground gruber-darker-yellow
                                          :background gruber-darker-bg+1))))
   `(magit-log-head-label-head ((t ,(list :foreground gruber-darker-fg
                                          :background gruber-darker-bg+1))))
   `(magit-item-highlight ((t (:background ,gruber-darker-bg+1))))
   `(magit-tag ((t ,(list :foreground gruber-darker-yellow
                          :background gruber-darker-bg))))
   `(magit-blame-heading ((t ,(list :background gruber-darker-bg+1
                                    :foreground gruber-darker-fg))))

   ;; Message
   `(message-header-name ((t (:foreground ,gruber-darker-green))))

   ;; Mode Line
   `(mode-line ((t ,(list :background gruber-darker-bg+1
                          :foreground gruber-darker-white))))
   `(mode-line-buffer-id ((t ,(list :background gruber-darker-bg+1
                                    :foreground gruber-darker-white))))
   `(mode-line-inactive ((t ,(list :background gruber-darker-bg+1
                                   :foreground gruber-darker-quartz))))

   ;; Neo Dir
   `(neo-dir-link-face ((t (:foreground ,gruber-darker-niagara))))

   ;; Org Mode
   `(org-agenda-structure ((t (:foreground ,gruber-darker-niagara))))
   `(org-column ((t (:background ,gruber-darker-bg-1))))
   `(org-column-title ((t (:background ,gruber-darker-bg-1 :underline t :weight bold))))
   `(org-done ((t (:foreground ,gruber-darker-green))))
   `(org-todo ((t (:foreground ,gruber-darker-red-1))))
   `(org-upcoming-deadline ((t (:foreground ,gruber-darker-yellow))))

   ;; Search
   `(isearch ((t ,(list :foreground gruber-darker-black
                        :background gruber-darker-fg+2))))
   `(isearch-fail ((t ,(list :foreground gruber-darker-black
                             :background gruber-darker-red))))
   `(isearch-lazy-highlight-face ((t ,(list
                                       :foreground gruber-darker-fg+1
                                       :background gruber-darker-niagara-1))))

   ;; Sh
   `(sh-quoted-exec ((t (:foreground ,gruber-darker-red+1))))

   ;; Show Paren
   `(show-paren-match-face ((t (:background ,gruber-darker-bg+4))))
   `(show-paren-mismatch-face ((t (:background ,gruber-darker-red-1))))

   ;; Slime
   `(slime-repl-inputed-output-face ((t (:foreground ,gruber-darker-red))))

   ;; Tuareg
   `(tuareg-font-lock-governing-face ((t (:foreground ,gruber-darker-yellow))))

   ;; Speedbar
   `(speedbar-directory-face ((t ,(list :foreground gruber-darker-niagara
                                        :weight 'bold))))
   `(speedbar-file-face ((t (:foreground ,gruber-darker-fg))))
   `(speedbar-highlight-face ((t (:background ,gruber-darker-bg+1))))
   `(speedbar-selected-face ((t (:foreground ,gruber-darker-red))))
   `(speedbar-tag-face ((t (:foreground ,gruber-darker-yellow))))

   ;; Which Function
   `(which-func ((t (:foreground ,gruber-darker-wisteria))))

   ;; Whitespace
   `(whitespace-space ((t ,(list :background gruber-darker-bg
                                 :foreground gruber-darker-bg+1))))
   `(whitespace-tab ((t ,(list :background gruber-darker-bg
                               :foreground gruber-darker-bg+1))))
   `(whitespace-hspace ((t ,(list :background gruber-darker-bg
                                  :foreground gruber-darker-bg+2))))
   `(whitespace-line ((t ,(list :background gruber-darker-bg+2
                                :foreground gruber-darker-red+1))))
   `(whitespace-newline ((t ,(list :background gruber-darker-bg
                                   :foreground gruber-darker-bg+2))))
   `(whitespace-trailing ((t ,(list :background gruber-darker-red
                                    :foreground gruber-darker-red))))
   `(whitespace-empty ((t ,(list :background gruber-darker-yellow
                                 :foreground gruber-darker-yellow))))
   `(whitespace-indentation ((t ,(list :background gruber-darker-yellow
                                       :foreground gruber-darker-red))))
   `(whitespace-space-after-tab ((t ,(list :background gruber-darker-yellow
                                           :foreground gruber-darker-yellow))))
   `(whitespace-space-before-tab ((t ,(list :background gruber-darker-brown
                                            :foreground gruber-darker-brown))))

   ;; tab-bar
   `(tab-bar ((t (:background ,gruber-darker-bg+1 :foreground ,gruber-darker-bg+4))))
   `(tab-bar-tab ((t (:background nil :foreground ,gruber-darker-yellow :weight bold))))
   `(tab-bar-tab-inactive ((t (:background nil))))

   ;; vterm / ansi-term
   `(term-color-black ((t (:foreground ,gruber-darker-bg+3 :background ,gruber-darker-bg+4))))
   `(term-color-red ((t (:foreground ,gruber-darker-red-1 :background ,gruber-darker-red-1))))
   `(term-color-green ((t (:foreground ,gruber-darker-green :background ,gruber-darker-green))))
   `(term-color-blue ((t (:foreground ,gruber-darker-niagara :background ,gruber-darker-niagara))))
   `(term-color-yellow ((t (:foreground ,gruber-darker-yellow :background ,gruber-darker-yellow))))
   `(term-color-magenta ((t (:foreground ,gruber-darker-wisteria :background ,gruber-darker-wisteria))))
   `(term-color-cyan ((t (:foreground ,gruber-darker-quartz :background ,gruber-darker-quartz))))
   `(term-color-white ((t (:foreground ,gruber-darker-fg :background ,gruber-darker-white))))

   ;; company-mode
   `(company-tooltip ((t (:foreground ,gruber-darker-fg :background ,gruber-darker-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,gruber-darker-brown :background ,gruber-darker-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,gruber-darker-brown :background ,gruber-darker-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,gruber-darker-fg :background ,gruber-darker-bg-1))))
   `(company-tooltip-mouse ((t (:background ,gruber-darker-bg-1))))
   `(company-tooltip-common ((t (:foreground ,gruber-darker-green))))
   `(company-tooltip-common-selection ((t (:foreground ,gruber-darker-green))))
   `(company-scrollbar-fg ((t (:background ,gruber-darker-bg-1))))
   `(company-scrollbar-bg ((t (:background ,gruber-darker-bg+2))))
   `(company-preview ((t (:background ,gruber-darker-green))))
   `(company-preview-common ((t (:foreground ,gruber-darker-green :background ,gruber-darker-bg-1))))

   ;; Proof General
   `(proof-locked-face ((t (:background ,gruber-darker-niagara-2))))

   ;; Orderless
   `(orderless-match-face-0 ((t (:foreground ,gruber-darker-yellow))))
   `(orderless-match-face-1 ((t (:foreground ,gruber-darker-green))))
   `(orderless-match-face-2 ((t (:foreground ,gruber-darker-brown))))
   `(orderless-match-face-3 ((t (:foreground ,gruber-darker-quartz))))

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
   `(hl-line ((t (:background ,gruber-darker-bg+1))))
   `(hl-line-face ((t (:background ,gruber-darker-bg+1))))

   ;; powerline
   `(powerline-active1 ((t (:background ,gruber-darker-bg+1 :foreground ,gruber-darker-white))))
   `(powerline-active2 ((t (:background ,gruber-darker-bg+1 :foreground ,gruber-darker-white))))
   `(powerline-inactive1 ((t (:background ,gruber-darker-bg+1 :foreground ,gruber-darker-quartz))))
   `(powerline-inactive2 ((t (:background ,gruber-darker-bg+1 :foreground ,gruber-darker-quartz))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'void-gruber)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; void-gruber-theme.el ends here
