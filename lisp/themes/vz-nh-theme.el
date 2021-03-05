;; -*- lexical-binding: t; -*-
;; Don't mind the name

(deftheme vz-nh "A custom theme")

(let ((class '((class color) (min-colors 89)))
      (bg "#ffffff")
      (fg "#0d0d0d")
      (red "#ff9994")
      (blue "#79a2ed")
      (grey "#cccccc")
      (green "#c1e1c1")
      (cursor "#505152")
      (light-fg "#a9a9a9")
      (darker-bg "#a9a9a9")
      (bar-bg "#f0f0f0")
      (bar-ufg "#6e6e6e")
      (dark-bg "#d9d9d9"))
  (custom-theme-set-faces
   'vz-nh
   `(default                          ((,class :background ,bg :foreground ,fg)))
   `(font-lock-comment-face           ((,class :inherit italic)))

   `(show-paren-match                 ((,class :background ,dark-bg :foreground ,fg)))
   `(show-paren-match-expression      ((,class :background ,dark-bg :foreground ,fg)))
   `(show-paren-mismatch              ((,class :background ,red :foreground ,fg)))

   ;; Emacs >=27 only
   `(fill-column-indicator            ((,class :background ,dark-bg :foreground ,dark-bg)))

   `(error                            ((,class :background ,bg :foreground ,red)))
   `(link                             ((,class :background ,bg :foreground ,blue :underline t)))

   `(cursor                           ((,class :background ,cursor :foreground ,fg)))
   `(fringe                           ((,class :background ,bg :foreground ,light-fg)))

   `(region                           ((,class :background ,dark-bg :foreground ,fg)))
   `(match                            ((,class :background ,dark-bg :foreground ,fg)))
   `(secondary-selection              ((,class :background ,darker-bg :foreground ,fg)))
   `(minibuffer-prompt                ((,class :background ,bg :foreground ,fg)))
   `(isearch                          ((,class :background ,dark-bg :foreground ,fg)))
   `(lazy-highlight                   ((,class :background ,dark-bg :foreground ,fg)))
   `(highlight                        ((,class :background ,dark-bg :foreground ,fg)))
   `(trailing-whitespace              ((,class :background ,darker-bg :foreground ,fg)))

   `(flyspell-incorrect               ((,class :underline (:color ,red :style wave))))
   `(flymake-error                    ((,class :underline (:color ,red :style wave))))
   `(flymake-warning                  ((,class :underline (:color ,green :style wave))))

   `(beacon-fallback-background       ((,class :background ,darker-bg :foreground ,darker-bg)))

   `(line-number                      ((,class :background ,bg :foreground ,light-fg)))
   `(line-number-current-line         ((,class :background ,bg :foreground ,fg)))

   `(evil-mc-region-face              ((,class :background ,dark-bg :foreground ,fg)))

   `(evil-ex-info                     ((,class :background ,bg :foreground ,red)))
   `(evil-ex-substitute-matches       ((,class :background ,dark-bg :foreground ,fg)))
   `(evil-ex-substitute-replacement   ((,class :background ,bg :foreground ,fg)))
   `(evil-ex-search                   ((,class :background ,bg :foreground ,fg)))

   `(ivy-grep-info                    ((,class :background ,bg :foreground ,fg)))
   `(ivy-yanked-word                  ((,class :background ,bg :foreground ,fg)))
   `(ivy-cursor                       ((,class :background ,cursor :foreground ,fg)))
   `(ivy-highlight-face               ((,class :background ,bg :foreground ,fg)))
   `(ivy-minibuffer-match-highlight   ((,class :background ,dark-bg :foreground ,fg)))
   `(ivy-prompt-match                 ((,class :background ,dark-bg :foreground ,fg)))
   `(ivy-completions-annotations      ((,class :background ,bg :foreground ,light-fg)))
   `(ivy-confirm-face                 ((,class :background ,bg :foreground ,light-fg)))

   `(ivy-current-match                ((,class :background ,dark-bg :foreground ,fg)))
   `(ivy-minibuffer-match-face-1      ((,class :background ,bg :foreground ,light-fg)))
   `(ivy-minibuffer-match-face-2      ((,class :background ,bg :foreground ,light-fg)))
   `(ivy-minibuffer-match-face-3      ((,class :background ,bg :foreground ,light-fg)))
   `(ivy-minibuffer-match-face-4      ((,class :background ,bg :foreground ,light-fg)))
   `(ivy-match-required-face          ((,class :background ,bg :foreground ,red)))

   `(ivy-modified-buffer              ((,class :background ,bg :foreground ,fg)))
   `(ivy-modified-outside-buffer      ((,class :background ,bg :foreground ,fg)))
   `(ivy-org                          ((,class :inherit default)))

   `(ivy-separator                    ((,class :background ,bg :foreground ,light-fg)))
   `(ivy-remote                       ((,class :background ,bg :foreground ,light-fg)))

   `(swiper-background-match-face-1   ((,class :background ,bg :foreground ,light-fg)))
   `(swiper-background-match-face-2   ((,class :background ,bg :foreground ,light-fg)))
   `(swiper-background-match-face-3   ((,class :background ,bg :foreground ,light-fg)))
   `(swiper-background-match-face-4   ((,class :background ,bg :foreground ,light-fg)))

   `(swiper-line-face                 ((,class :background ,dark-bg :foreground ,fg)))
   `(swiper-match-face-3              ((,class :background ,bg :foreground ,light-fg)))
   `(swiper-match-face-4              ((,class :background ,bg :foreground ,light-fg)))
   `(swiper-match-face-1              ((,class :background ,bg :foreground ,light-fg)))
   `(swiper-match-face-2              ((,class :background ,bg :foreground ,light-fg)))

   `(completions-common-part          ((,class :background ,bg :foreground ,light-fg)))

   `(company-echo-common                  ((,class :background ,dark-bg :foreground ,fg)))
   `(company-preview                      ((,class :background ,bg :foreground ,light-fg)))
   `(company-preview-common               ((,class :background ,bg :foreground ,light-fg)))
   `(company-preview-search               ((,class :background ,bg :foreground ,light-fg)))
   `(company-scrollbar-bg                 ((,class :background ,dark-bg :foreground ,fg)))
   `(company-scrollbar-fg                 ((,class :background ,darker-bg :foreground ,fg)))
   `(company-template-field               ((,class :background ,bg :foreground ,light-fg)))
   `(company-tooltip                      ((,class :background ,darker-bg :foreground ,fg)))
   `(company-tooltip-annotation           ((,class :background ,darker-bg :foreground ,fg)))
   `(company-tooltip-annotation-selection ((,class :background ,dark-bg :foreground ,light-fg)))
   `(company-tooltip-common               ((,class :background ,darker-bg :foreground ,dark-bg)))
   `(company-tooltip-common-selection     ((,class :background ,dark-bg :foreground ,fg)))
   `(company-tooltip-selection            ((,class :background ,dark-bg :foreground ,fg)))

   `(avy-goto-char-timer-face         ((,class :background ,darker-bg :foreground ,fg)))

   `(avy-lead-face                    ((,class :background ,bg :foreground ,red)))
   `(avy-lead-face-0                  ((,class :background ,bg :foreground ,red)))
   `(avy-lead-face-1                  ((,class :background ,bg :foreground ,red)))
   `(avy-lead-face-2                  ((,class :background ,bg :foreground ,red)))

   `(org-todo                         ((,class :background ,bg :foreground ,red)))
   `(org-done                         ((,class :background ,bg :foreground ,fg)))
   `(org-table                        ((,class :background ,bg :foreground ,fg)))
   `(org-block                        ((,class :background ,bg :foreground ,fg)))
   `(org-code                         ((,class :background ,bg :foreground ,fg)))
   `(org-headline-done                ((,class :background ,bg :foreground ,fg)))
   `(org-latex-and-related            ((,class :background ,bg :foreground ,fg)))
   `(org-document-title               ((,class :background ,bg :foreground ,fg)))
   `(org-document-info                ((,class :background ,bg :foreground ,light-fg)))
   `(org-drawer                       ((,class :background ,bg :foreground ,light-fg)))
   `(org-date                         ((,class :background ,bg :foreground ,light-fg)))
   `(org-meta-line                    ((,class :background ,bg :foreground ,fg :weight bold)))

   `(org-agenda-done                  ((,class :background ,bg :foreground ,light-fg)))
   `(org-agenda-date                  ((,class :background ,bg :foreground ,fg)))
   `(org-agenda-date-today            ((,class :background ,bg :foreground ,fg :weight bold)))
   `(org-agenda-date-weekend          ((,class :background ,bg :foreground ,light-fg)))
   `(org-agenda-structure             ((,class :background ,bg :foreground ,fg)))
   `(org-scheduled-today              ((,class :background ,bg :foreground ,fg)))
   `(org-time-grid                    ((,class :background ,bg :foreground ,light-fg)))

   `(makefile-space                   ((,class :background ,darker-bg :foreground ,bg)))

   `(header-line                      ((,class :box (:line-width 4 :color ,bg) :background ,bar-bg :foreground ,fg)))
   `(header-line-highlight            ((,class :box (:line-width 4 :color ,bg) :background ,bar-bg :foreground ,bar-ufg)))
   `(mode-line                        ((,class :box (:line-width 4 :color ,bg) :background ,bg :foreground ,fg)))
   `(mode-line-highlight              ((,class :box (:line-width 4 :color ,bg) :background ,bar-bg :foreground ,bar-ufg)))
   `(mode-line-inactive               ((,class :box (:line-width 4 :color ,bg) :background ,bg :foreground ,light-fg)))

   `(comint-highlight-prompt          ((,class :backgorund ,bg :foreground ,fg)))

   `(hl-todo                          ((,class :background ,bg :foreground ,light-fg :bold nil)))

   `(lui-highlight-face               ((,class :background ,bg :foreground ,light-fg)))
   `(lui-time-stamp-face              ((,class :background ,bg :foreground ,light-fg)))
   `(lui-button-face                  ((,class :background ,bg :foreground ,light-fg)))
   `(lui-server-face                  ((,class :background ,bg :foreground ,light-fg)))

   `(circe-highlight-nick-face        ((,class :background ,bg :foreground ,light-fg)))
   `(circe-my-message-face            ((,class :background ,bg :foreground ,grey)))
   `(circe-prompt-face                ((,class :background ,bg :foreground ,light-fg)))
   `(circe-server-face                ((,class :background ,bg :foreground ,light-fg)))
   `(circe-originator-face            ((,class :background ,bg :foreground ,light-fg)))

   `(edit-indirect-edited-region      ((,class :background ,dark-bg :foreground ,fg)))

   `(eros-result-overlay-face         ((,class :box ,darker-bg :background ,dark-bg :foreground ,fg))))

  (setq hl-todo-keyword-faces
        `(("TODO"   . ,red)
          ("FIXME"  . ,red)
          ("BROKEN" . ,red)
          ("NOTE"   . ,fg))
        vz/mode-line-fg fg
        vz/mode-line-bg bar-bg
        vz/mode-line-bgi bar-bg
        vz/mode-line-fgi bar-ufg)
  (add-to-list 'default-frame-alist `(cursor-color . ,cursor))

  (dolist (f '(lui-irc-colors-bg-0-face
               lui-irc-colors-bg-1-face
               lui-irc-colors-bg-2-face
               lui-irc-colors-bg-3-face
               lui-irc-colors-bg-4-face
               lui-irc-colors-bg-5-face
               lui-irc-colors-bg-6-face
               lui-irc-colors-bg-7-face
               lui-irc-colors-bg-8-face
               lui-irc-colors-bg-9-face
               lui-irc-colors-bg-10-face
               lui-irc-colors-bg-11-face
               lui-irc-colors-bg-12-face
               lui-irc-colors-bg-13-face
               lui-irc-colors-bg-14-face
               lui-irc-colors-bg-15-face
               lui-irc-colors-fg-0-face
               lui-irc-colors-fg-1-face
               lui-irc-colors-fg-2-face
               lui-irc-colors-fg-3-face
               lui-irc-colors-fg-4-face
               lui-irc-colors-fg-5-face
               lui-irc-colors-fg-6-face
               lui-irc-colors-fg-7-face
               lui-irc-colors-fg-8-face
               lui-irc-colors-fg-9-face
               lui-irc-colors-fg-10-face
               lui-irc-colors-fg-11-face
               lui-irc-colors-fg-12-face
               lui-irc-colors-fg-13-face
               lui-irc-colors-fg-14-face
               lui-irc-colors-fg-15-face))
    (custom-theme-set-faces 'vz-nh `(,f ((,class :inherit default)))))

  (dolist (f '(font-lock-builtin-face
               font-lock-preprocessor-face
               font-lock-constant-face
               font-lock-negation-char-face
               font-lock-constant-face
               font-lock-doc-face
               font-lock-function-name-face
               font-lock-keyword-face
               font-lock-string-face
               font-lock-type-face
               font-lock-variable-name-face
               font-lock-warning-face
               racket-selfeval-face
               racket-keyword-argument-face
               sh-heredoc
               sh-quoted-exec))
    (custom-theme-set-faces 'vz-nh `(,f ((,class :inhert default))))))

(provide-theme 'vz-nh)

(provide 'vz-nh-theme)
