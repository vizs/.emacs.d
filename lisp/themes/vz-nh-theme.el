;; -*- lexical-binding: t; -*-
;; Don't mind the name

(deftheme vz-nh "A custom theme.")

(let ((class '((type graphic) (class color) (min-colors 89)))
      (bg "#ffffff")
      (fg "#0d0d0d")
      (red "#ff9994")
      (blue "#79a2ed")
      (grey "#cccccc")
      (green "#c1e1c1")
      (off-white "#ffffbf")
      (darker-off-white "#ffff9e")
      (dark-red "#ff0000")
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

   `(error                            ((,class :foreground ,red)))
   `(link                             ((,class :foreground ,blue :underline t)))

   `(cursor                           ((,class :background ,cursor :foreground ,fg)))
   `(fringe                           ((,class :background ,bg :foreground ,light-fg)))

   `(region                           ((,class :background ,off-white :foreground ,fg)))
   `(match                            ((,class :background ,dark-bg :foreground ,fg)))
   `(secondary-selection              ((,class :background ,darker-bg :foreground ,fg)))
   `(minibuffer-prompt                ((,class :foreground ,fg)))
   `(isearch                          ((,class :background ,dark-bg :foreground ,fg)))
   `(lazy-highlight                   ((,class :background ,darker-off-white)))
   `(highlight                        ((,class :background ,dark-bg)))
   `(trailing-whitespace              ((,class :background ,darker-bg :foreground ,fg)))

   `(holiday                          ((,class :foreground ,dark-red)))
   `(calendar-today                   ((,class :foreground ,blue)))

   `(Info-quoted                      ((,class :foreground ,light-fg :inherit fixed-pitch)))

   `(flyspell-incorrect               ((,class :underline (:color ,red :style wave))))
   `(flymake-error                    ((,class :underline (:color ,red :style wave))))
   `(flymake-warning                  ((,class :underline (:color ,green :style wave))))

   `(beacon-fallback-background       ((,class :background ,darker-bg :foreground ,darker-bg)))

   `(line-number                      ((,class :foreground ,light-fg)))
   `(line-number-current-line         ((,class :foreground ,fg)))

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

   `(ivy-modified-buffer              ((,class :foreground ,fg)))
   `(ivy-modified-outside-buffer      ((,class :foreground ,fg)))
   `(ivy-org                          ((,class :inherit default)))

   `(ivy-separator                    ((,class :foreground ,light-fg)))
   `(ivy-remote                       ((,class :foreground ,light-fg)))

   `(completions-common-part          ((,class :foreground ,light-fg)))


   `(company-scrollbar-bg ((,class :background ,off-white)))
   `(company-scrollbar-fg ((,class :background ,red)))

   ;; Face used to highlight main company "window"
   `(company-tooltip ((,class :background ,off-white)))

   ;; Face used to highlight common string
   `(company-tooltip-common ((,class :foreground ,dark-red)))

   ;; Face used to highlight the entire selected line
   `(company-tooltip-selection ((,class :background ,dark-bg)))

   ;; Face used to highlight ONLY the common string but the selected one instead
   `(company-tooltip-common-selection ((,class :background ,dark-bg :foreground ,dark-red)))

   `(company-preview-common ((,class :foreground ,fg)))
   `(company-preview        ((,class :foreground ,fg)))

   `(avy-goto-char-timer-face         ((,class :background ,darker-bg :foreground ,fg)))

   `(avy-lead-face                    ((,class :background ,bg :foreground ,red)))
   `(avy-lead-face-0                  ((,class :background ,bg :foreground ,red)))
   `(avy-lead-face-1                  ((,class :background ,bg :foreground ,red)))
   `(avy-lead-face-2                  ((,class :background ,bg :foreground ,red)))

   `(org-todo                         ((,class :foreground ,red)))
   `(org-done                         ((,class :foreground ,fg)))
   `(org-table                        ((,class :foreground ,fg)))
   `(org-block                        ((,class :foreground ,fg)))
   `(org-code                         ((,class :foreground ,fg)))
   `(org-headline-done                ((,class :foreground ,fg)))
   `(org-latex-and-related            ((,class :foreground ,fg)))
   `(org-document-title               ((,class :foreground ,fg)))
   `(org-document-info                ((,class :foreground ,light-fg)))
   `(org-drawer                       ((,class :foreground ,light-fg)))
   `(org-date                         ((,class :foreground ,light-fg)))
   `(org-meta-line                    ((,class :foreground ,fg :weight bold)))

   `(org-agenda-done                  ((,class :foreground ,light-fg)))
   `(org-agenda-date                  ((,class :foreground ,fg)))
   `(org-agenda-date-today            ((,class :foreground ,fg :weight bold)))
   `(org-agenda-date-weekend          ((,class :foreground ,light-fg)))
   `(org-agenda-structure             ((,class :foreground ,fg)))
   `(org-scheduled                    ((,class :foreground ,fg)))
   `(org-scheduled-today              ((,class :foreground ,fg)))
   `(org-scheduled-previously         ((,class :foreground ,dark-red)))
   `(org-time-grid                    ((,class :foreground ,light-fg)))

   ;; `(org-deadline-today               ((,class :foreground ,dark-red)))
   `(org-upcoming-deadline            ((,class :foreground ,fg)))
   `(org-upcoming-distant-deadline    ((,class :foreground ,fg)))

   `(makefile-space                   ((,class :background ,darker-bg :foreground ,bg)))

   ;; `(header-line                      ((,class :box (:line-width 4 :color ,bg) :background ,bar-bg :foreground ,fg)))
   ;; `(header-line-highlight            ((,class :box (:line-width 4 :color ,bg) :background ,bar-bg :foreground ,bar-ufg)))
   ;; `(mode-line                        ((,class :box (:line-width 4 :color ,bg) :background ,bg :foreground ,fg)))
   ;; `(mode-line-highlight              ((,class :box (:line-width 4 :color ,bg) :background ,bar-bg :foreground ,bar-ufg)))
   ;; `(mode-line-inactive               ((,class :box (:line-width 4 :color ,bg) :background ,bg :foreground ,light-fg)))

   `(comint-highlight-prompt          ((,class :foreground ,fg)))
   `(comint-highlight-input           ((,class :weight normal)))

   `(hl-todo                          ((,class :foreground ,light-fg :bold nil)))

   `(lui-highlight-face               ((,class :foreground ,light-fg)))
   `(lui-time-stamp-face              ((,class :foreground ,light-fg)))
   `(lui-button-face                  ((,class :foreground ,light-fg)))
   `(lui-server-face                  ((,class :foreground ,light-fg)))

   `(circe-highlight-nick-face        ((,class :foreground ,light-fg)))
   `(circe-my-message-face            ((,class :foreground ,grey)))
   `(circe-prompt-face                ((,class :foreground ,light-fg)))
   `(circe-server-face                ((,class :foreground ,light-fg)))
   `(circe-originator-face            ((,class :foreground ,light-fg)))

   `(edit-indirect-edited-region      ((,class :background ,dark-bg :foreground ,fg)))

   `(eros-result-overlay-face         ((,class :box ,darker-bg :background ,dark-bg :foreground ,fg)))

   `(siege-preview-face               ((,class :foreground ,light-fg))))

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
