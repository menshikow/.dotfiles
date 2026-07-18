;;; dark-void-theme.el --- Dark void colorscheme -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Adrian Menschikow <github.com/menshikow>
;; Version: 0.2
;; Filename: dark-void-theme.el
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/menshikow/void-theme
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
;; Dark void monochrome color theme.

;;; Code:

(deftheme dark-void "Dark monochrome theme for Emacs.")

(let (;; UI colors
      (vm-fg        "#cccccc")
      (vm-fg+1      "#ffffff")
      (vm-fg+2      "#ffffff")
      (vm-white     "#ffffff")
      (vm-black     "#000000")
      (vm-bg-1      "#000000")
      (vm-bg        "#000000")
      (vm-bg+1      "#1a1a1a")
      (vm-bg+2      "#4d4d4d")
      (vm-bg+3      "#484848")
      (vm-bg+4      "#4d4d4d")
      (vm-red-1     "#ff4444")
      (vm-red       "#ff4444")
      (vm-red+1     "#ff4444")
      (vm-green     "#8fdf8f")
      (vm-yellow    "#cccccc")
      (vm-brown     "#a0a0a0")
      (vm-quartz    "#a0a0a0")
      (vm-niagara-2 "#a0a0a0")
      (vm-niagara-1 "#888888")
      (vm-niagara   "#a0a0a0")
      (vm-wisteria  "#a0a0a0")

      ;; Syntax colors
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
   'dark-void
   '(frame-background-mode (quote dark)))

  (custom-theme-set-faces
   'dark-void

   ;; Agda2
   `(agda2-highlight-datatype-face ((t (:foreground ,vm-quartz))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,vm-quartz))))
   `(agda2-highlight-function-face ((t (:foreground ,vm-niagara))))
   `(agda2-highlight-keyword-face ((t ,(list :foreground vm-yellow
                                              :bold t))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,vm-green))))
   `(agda2-highlight-number-face ((t (:foreground ,vm-wisteria))))

   ;; AUCTeX
   `(font-latex-bold-face ((t (:foreground ,vm-quartz :bold t))))
   `(font-latex-italic-face ((t (:foreground ,vm-quartz :italic t))))
   `(font-latex-math-face ((t (:foreground ,vm-green))))
   `(font-latex-sectioning-5-face ((t ,(list :foreground vm-niagara
                                              :bold t))))
   `(font-latex-slide-title-face ((t (:foreground ,vm-niagara))))
   `(font-latex-string-face ((t (:foreground ,vm-green))))
   `(font-latex-warning-face ((t (:foreground ,vm-red))))

   ;; Basic Coloring (or Uncategorized)
   `(border ((t ,(list :background vm-bg-1
                        :foreground vm-bg+2))))
   `(cursor ((t (:background ,vm-white))))
   `(default ((t ,(list :foreground vm-fg
                         :background vm-bg))))
   `(fringe ((t ,(list :background nil
                        :foreground vm-bg+2))))
   `(vertical-border ((t ,(list :foreground vm-bg+2))))
   `(link ((t (:foreground ,vm-niagara :underline t))))
   `(link-visited ((t (:foreground ,vm-wisteria :underline t))))
   `(match ((t (:background ,vm-bg+4))))
   `(shadow ((t (:foreground ,vm-bg+4))))
   `(minibuffer-prompt ((t (:foreground ,vm-niagara))))
   `(region ((t (:background ,vm-bg+3 :foreground nil))))
   `(secondary-selection ((t ,(list :background vm-bg+3
                                     :foreground nil))))
   `(trailing-whitespace ((t ,(list :foreground vm-black
                                     :background vm-red))))
   `(tooltip ((t ,(list :background vm-bg+4
                         :foreground vm-white))))

   ;; Calendar
   `(holiday-face ((t (:foreground ,vm-red))))

   ;; Compilation
   `(compilation-info ((t ,(list :foreground void-green
                                  :inherit 'unspecified))))
   `(compilation-warning ((t ,(list :foreground vm-brown
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
   `(custom-state ((t (:foreground ,vm-green))))

   ;; Diff
   `(diff-removed ((t ,(list :foreground vm-red+1
                              :background nil))))
   `(diff-added ((t ,(list :foreground vm-green
                            :background nil))))

   ;; Dired
   `(dired-directory ((t (:foreground ,vm-white :weight bold))))
   `(dired-subtree-depth-1-face ((t (:background nil :foreground ,vm-fg))))
   `(dired-subtree-depth-2-face ((t (:background nil :foreground ,vm-fg+1))))
   `(dired-subtree-depth-3-face ((t (:background nil :foreground ,vm-fg))))
   `(dired-ignored ((t ,(list :foreground vm-quartz
                               :inherit 'unspecified))))

   ;; Ebrowse
   `(ebrowse-root-class ((t (:foreground ,vm-niagara :weight bold))))
   `(ebrowse-progress ((t (:background ,vm-niagara))))

   ;; Egg
   `(egg-branch ((t (:foreground ,vm-yellow))))
   `(egg-branch-mono ((t (:foreground ,vm-yellow))))
   `(egg-diff-add ((t (:foreground ,vm-green))))
   `(egg-diff-del ((t (:foreground ,vm-red))))
   `(egg-diff-file-header ((t (:foreground ,vm-wisteria))))
   `(egg-help-header-1 ((t (:foreground ,vm-yellow))))
   `(egg-help-header-2 ((t (:foreground ,vm-niagara))))
   `(egg-log-HEAD-name ((t (:box (:color ,vm-fg)))))
   `(egg-reflog-mono ((t (:foreground ,vm-niagara-1))))
   `(egg-section-title ((t (:foreground ,vm-yellow))))
   `(egg-text-base ((t (:foreground ,vm-fg))))
   `(egg-term ((t (:foreground ,vm-yellow))))

   ;; ERC
   `(erc-notice-face ((t (:foreground ,vm-wisteria))))
   `(erc-timestamp-face ((t (:foreground ,vm-green))))
   `(erc-input-face ((t (:foreground ,vm-red+1))))
   `(erc-my-nick-face ((t (:foreground ,vm-red+1))))

   ;; EShell
   `(eshell-ls-backup ((t (:foreground ,vm-quartz))))
   `(eshell-ls-directory ((t (:foreground ,vm-niagara))))
   `(eshell-ls-executable ((t (:foreground ,vm-green))))
   `(eshell-ls-symlink ((t (:foreground ,vm-yellow))))

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
       (:underline (:style wave :color ,vm-red)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,vm-red :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,vm-yellow)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,vm-yellow :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,vm-green)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,vm-green :weight bold :underline t))))

   ;; Flyspell
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,vm-red) :inherit unspecified))
      (t (:foreground ,vm-red :weight bold :underline t))))
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,vm-yellow) :inherit unspecified))
      (t (:foreground ,vm-yellow :weight bold :underline t))))

   ;; Helm
   `(helm-candidate-number ((t ,(list :background vm-bg+2
                                       :foreground vm-yellow
                                       :bold t))))
   `(helm-ff-directory ((t ,(list :foreground vm-niagara
                                   :background vm-bg
                                   :bold t))))
   `(helm-ff-executable ((t (:foreground ,vm-green))))
   `(helm-ff-file ((t (:foreground ,vm-fg :inherit unspecified))))
   `(helm-ff-invalid-symlink ((t ,(list :foreground vm-bg
                                         :background vm-red))))
   `(helm-ff-symlink ((t (:foreground ,vm-yellow :bold t))))
   `(helm-selection-line ((t (:background ,vm-bg+1))))
   `(helm-selection ((t (:background ,vm-bg+1 :underline nil))))
   `(helm-source-header ((t ,(list :foreground vm-yellow
                                    :background vm-bg
                                    :box (list :line-width -1
                                               :style 'released-button)))))

   ;; Ido
   `(ido-first-match ((t (:foreground ,vm-yellow :bold nil))))
   `(ido-only-match ((t (:foreground ,vm-brown :weight bold))))
   `(ido-subdir ((t (:foreground ,vm-niagara :weight bold))))

   ;; Info
   `(info-xref ((t (:foreground ,vm-niagara))))
   `(info-visited ((t (:foreground ,vm-wisteria))))

   ;; Jabber
   `(jabber-chat-prompt-foreign ((t ,(list :foreground vm-quartz
                                            :bold nil))))
   `(jabber-chat-prompt-local ((t (:foreground ,vm-yellow))))
   `(jabber-chat-prompt-system ((t (:foreground ,vm-green))))
   `(jabber-rare-time-face ((t (:foreground ,vm-green))))
   `(jabber-roster-user-online ((t (:foreground ,vm-green))))
   `(jabber-activity-face ((t (:foreground ,vm-red))))
   `(jabber-activity-personal-face ((t (:foreground ,vm-yellow :bold t))))

   ;; Line Highlighting
   `(highlight ((t (:background ,vm-bg+1 :foreground nil))))
   `(highlight-current-line-face ((t ,(list :background vm-bg+1
                                             :foreground nil))))

   ;; line numbers
   `(line-number ((t (:inherit default :foreground ,vm-bg+4))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,vm-yellow))))

   ;; Linum
   `(linum ((t (:foreground ,vm-quartz
                             :background ,vm-bg))))

   ;; Magit
   `(magit-branch ((t (:foreground ,vm-niagara))))
   `(magit-diff-hunk-header ((t (:background ,vm-bg+2))))
   `(magit-diff-file-header ((t (:background ,vm-bg+4))))
   `(magit-log-sha1 ((t (:foreground ,vm-red+1))))
   `(magit-log-author ((t (:foreground ,vm-brown))))
   `(magit-log-head-label-remote ((t ,(list :foreground vm-green
                                             :background vm-bg+1))))
   `(magit-log-head-label-local ((t ,(list :foreground vm-niagara
                                            :background vm-bg+1))))
   `(magit-log-head-label-tags ((t ,(list :foreground vm-yellow
                                           :background vm-bg+1))))
   `(magit-log-head-label-head ((t ,(list :foreground vm-fg
                                           :background vm-bg+1))))
   `(magit-item-highlight ((t (:background ,vm-bg+1))))
   `(magit-tag ((t ,(list :foreground vm-yellow
                           :background vm-bg))))
   `(magit-blame-heading ((t ,(list :background vm-bg+1
                                     :foreground vm-fg))))

   ;; Message
   `(message-header-name ((t (:foreground ,vm-green))))

   ;; Mode Line
   `(mode-line ((t ,(list :background vm-bg+1
                           :foreground vm-white))))
   `(mode-line-buffer-id ((t ,(list :background vm-bg+1
                                     :foreground vm-white))))
   `(mode-line-inactive ((t ,(list :background vm-bg+1
                                    :foreground vm-quartz))))

   ;; Neo Dir
   `(neo-dir-link-face ((t (:foreground ,vm-niagara))))

   ;; Org Mode
   `(org-agenda-structure ((t (:foreground ,vm-niagara))))
   `(org-column ((t (:background ,vm-bg-1))))
   `(org-column-title ((t (:background ,vm-bg-1 :underline t :weight bold))))
   `(org-done ((t (:foreground ,vm-green))))
   `(org-todo ((t (:foreground ,vm-red-1))))
   `(org-upcoming-deadline ((t (:foreground ,vm-yellow))))

   ;; Search
   `(isearch ((t ,(list :foreground vm-black
                         :background vm-fg+2))))
   `(isearch-fail ((t ,(list :foreground vm-black
                              :background vm-red))))
   `(isearch-lazy-highlight-face ((t ,(list
                                        :foreground vm-fg+1
                                        :background vm-niagara-1))))

   ;; Sh
   `(sh-quoted-exec ((t (:foreground ,vm-red+1))))

   ;; Show Paren
   `(show-paren-match-face ((t (:background ,vm-bg+4))))
   `(show-paren-mismatch-face ((t (:background ,vm-red-1))))

   ;; Slime
   `(slime-repl-inputed-output-face ((t (:foreground ,vm-red))))

   ;; Tuareg
   `(tuareg-font-lock-governing-face ((t (:foreground ,vm-yellow))))

   ;; Speedbar
   `(speedbar-directory-face ((t ,(list :foreground vm-niagara
                                         :weight 'bold))))
   `(speedbar-file-face ((t (:foreground ,vm-fg))))
   `(speedbar-highlight-face ((t (:background ,vm-bg+1))))
   `(speedbar-selected-face ((t (:foreground ,vm-red))))
   `(speedbar-tag-face ((t (:foreground ,vm-yellow))))

   ;; Which Function
   `(which-func ((t (:foreground ,vm-wisteria))))

   ;; Whitespace
   `(whitespace-space ((t ,(list :background vm-bg
                                  :foreground vm-bg+1))))
   `(whitespace-tab ((t ,(list :background vm-bg
                                :foreground vm-bg+1))))
   `(whitespace-hspace ((t ,(list :background vm-bg
                                   :foreground vm-bg+2))))
   `(whitespace-line ((t ,(list :background vm-bg+2
                                 :foreground vm-red+1))))
   `(whitespace-newline ((t ,(list :background vm-bg
                                    :foreground vm-bg+2))))
   `(whitespace-trailing ((t ,(list :background vm-red
                                     :foreground vm-red))))
   `(whitespace-empty ((t ,(list :background vm-yellow
                                  :foreground vm-yellow))))
   `(whitespace-indentation ((t ,(list :background vm-yellow
                                        :foreground vm-red))))
   `(whitespace-space-after-tab ((t ,(list :background vm-yellow
                                            :foreground vm-yellow))))
   `(whitespace-space-before-tab ((t ,(list :background vm-brown
                                             :foreground vm-brown))))

   ;; tab-bar
   `(tab-bar ((t (:background ,vm-bg+1 :foreground ,vm-bg+4))))
   `(tab-bar-tab ((t (:background nil :foreground ,vm-yellow :weight bold))))
   `(tab-bar-tab-inactive ((t (:background nil))))

   ;; vterm / ansi-term
   `(term-color-black ((t (:foreground ,vm-bg+3 :background ,vm-bg+4))))
   `(term-color-red ((t (:foreground ,vm-red-1 :background ,vm-red-1))))
   `(term-color-green ((t (:foreground ,vm-green :background ,vm-green))))
   `(term-color-blue ((t (:foreground ,vm-niagara :background ,vm-niagara))))
   `(term-color-yellow ((t (:foreground ,vm-yellow :background ,vm-yellow))))
   `(term-color-magenta ((t (:foreground ,vm-wisteria :background ,vm-wisteria))))
   `(term-color-cyan ((t (:foreground ,vm-quartz :background ,vm-quartz))))
   `(term-color-white ((t (:foreground ,vm-fg :background ,vm-white))))

   ;; company-mode
   `(company-tooltip ((t (:foreground ,vm-fg :background ,vm-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,vm-brown :background ,vm-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,vm-brown :background ,vm-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,vm-fg :background ,vm-bg-1))))
   `(company-tooltip-mouse ((t (:background ,vm-bg-1))))
   `(company-tooltip-common ((t (:foreground ,vm-green))))
   `(company-tooltip-common-selection ((t (:foreground ,vm-green))))
   `(company-scrollbar-fg ((t (:background ,vm-bg-1))))
   `(company-scrollbar-bg ((t (:background ,vm-bg+2))))
   `(company-preview ((t (:background ,vm-green))))
   `(company-preview-common ((t (:foreground ,vm-green :background ,vm-bg-1))))

   ;; Proof General
   `(proof-locked-face ((t (:background ,vm-niagara-2))))

   ;; Orderless
   `(orderless-match-face-0 ((t (:foreground ,vm-yellow))))
   `(orderless-match-face-1 ((t (:foreground ,vm-green))))
   `(orderless-match-face-2 ((t (:foreground ,vm-brown))))
   `(orderless-match-face-3 ((t (:foreground ,vm-quartz))))

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
   `(hl-line ((t (:background ,vm-bg+1))))
   `(hl-line-face ((t (:background ,vm-bg+1))))

   ;; powerline
   `(powerline-active1 ((t (:background ,vm-bg+1 :foreground ,vm-white))))
   `(powerline-active2 ((t (:background ,vm-bg+1 :foreground ,vm-white))))
   `(powerline-inactive1 ((t (:background ,vm-bg+1 :foreground ,vm-quartz))))
   `(powerline-inactive2 ((t (:background ,vm-bg+1 :foreground ,vm-quartz))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dark-void)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; dark-void-theme.el ends here
