;; -*- lexical-binding: t; -*-

(deftheme vz "A custom theme")

(let ((class '((class color) (min-colors 89)))
      (bg "#ffffea")
      (fg "#1c1c1d")
      (re "#ef0606")
      (gr "#cccccc")
      (lg "#e9fee9")
      (GR "#438743")
      (bl "#000000")
      (lf "#434343")
      (yw "#99994c")
      (bu "#ed2553")
      (ac "#eaed8d")
      (BG "#ebebdf"))
  (custom-theme-set-faces
   'vz
   `(default                          ((,class :background ,bg :foreground ,fg)))
   `(font-lock-comment-face           ((,class :inherit italic)))

   `(show-paren-match                 ((,class :background ,ac :foreground ,fg)))
   `(show-paren-match-expression      ((,class :background ,ac :foreground ,fg)))
   `(show-paren-mismatch              ((,class :background ,re :foreground ,fg)))

   ;; Emacs >=27 only
   `(fill-column-indicator            ((,class :background ,ac :foreground ,ac)))

   `(error                            ((,class :background ,bg :foreground ,re)))

   `(cursor                           ((,class :background ,gr :foreground ,fg)))
   `(fringe                           ((,class :background ,bg :foreground ,yw)))

   `(region                           ((,class :background ,yw :foreground ,fg)))
   `(match                            ((,class :background ,ac :foreground ,fg)))
   `(minibuffer-prompt                ((,class :background ,bg :foreground ,fg)))
   `(isearch                          ((,class :background ,ac :foreground ,fg)))
   `(lazy-highlight                   ((,class :background ,ac :foreground ,fg)))
   `(highlight                        ((,class :background ,ac :foreground ,fg)))
   `(trailing-whitespace              ((,class :background ,yw :foreground ,fg)))
   ;;`(help-argument-name

   `(flyspell-incorrect               ((,class :underline (:color ,re :style wave))))
   `(flymake-error                    ((,class :underline (:color ,re :style wave))))
   `(flymake-warning                  ((,class :underline (:color ,GR :style wave))))

   `(beacon-fallback-background       ((,class :background ,yw :foregruond ,yw)))

   `(line-number                      ((,class :background ,bg :foreground ,lf)))
   `(line-number-current-line         ((,class :background ,bg :foreground ,fg)))

   `(evil-mc-region-face              ((,class :background ,ac :foreground ,fg)))

   `(evil-ex-info                     ((,class :background ,bg :foreground ,re)))
   `(evil-ex-substitute-matches       ((,class :background ,ac :foreground ,fg)))
   `(evil-ex-substitute-replacement   ((,class :background ,bg :foreground ,fg)))
   `(evil-ex-search                   ((,class :background ,bg :foreground ,fg)))

   `(ivy-grep-info                    ((,class :background ,bg :foreground ,fg)))
   `(ivy-yanked-word                  ((,class :background ,bg :foreground ,fg)))
   `(ivy-cursor                       ((,class :background ,gr :foreground ,fg)))
   `(ivy-highlight-face               ((,class :background ,GR :foreground ,lg)))
   `(ivy-minibuffer-match-highlight   ((,class :background ,GR :foreground ,lg)))
   `(ivy-prompt-match                 ((,class :background ,GR :foreground ,lg)))
   `(ivy-completions-annotations      ((,class :background ,lg :foreground ,bl)))
   `(ivy-confirm-face                 ((,class :background ,lg :foreground ,bl)))

   `(ivy-current-match                ((,class :background ,GR :foreground ,lg)))
   `(ivy-minibuffer-match-face-1      ((,class :background ,lg :foreground ,bl)))
   `(ivy-minibuffer-match-face-2      ((,class :background ,lg :foreground ,bl)))
   `(ivy-minibuffer-match-face-3      ((,class :background ,ac :foreground ,fg)))
   `(ivy-minibuffer-match-face-4      ((,class :background ,ac :foreground ,fg)))
   `(ivy-match-required-face          ((,class :background ,bg :foreground ,re)))

   `(ivy-modified-buffer              ((,class :background ,bg :foreground ,fg)))
   `(ivy-modified-outside-buffer      ((,class :background ,bg :foreground ,fg)))
   `(ivy-org                          ((,class :inherit default)))

   `(ivy-separator                    ((,class :background ,lg :foreground ,bl)))
   `(ivy-remote                       ((,class :background ,bg :foreground ,lf)))

   `(swiper-background-match-face-1   ((,class :background ,lg :foreground ,bl)))
   `(swiper-background-match-face-2   ((,class :background ,lg :foreground ,bl)))
   `(swiper-background-match-face-3   ((,class :background ,ac :foreground ,fg)))
   `(swiper-background-match-face-4   ((,class :background ,ac :foreground ,fg)))

   `(swiper-line-face                 ((,class :background ,GR :foreground ,lg)))
   `(swiper-match-face-3              ((,class :background ,ac :foreground ,fg)))
   `(swiper-match-face-4              ((,class :background ,ac :foreground ,fg)))
   `(swiper-match-face-1              ((,class :background ,lg :foreground ,bl)))
   `(swiper-match-face-2              ((,class :background ,lg :foreground ,bl)))

   `(completions-common-part          ((,class :background ,bg :foreground ,lf)))

   `(company-echo-common              ((,class :background ,ac :foreground ,fg)))
   `(company-preview                  ((,class :background ,bg :foreground ,lf)))
   `(company-preview-common           ((,class :background ,bg :foreground ,lf)))
   `(company-preview-search           ((,class :background ,bg :foreground ,lf)))
   `(company-scrollbar-bg             ((,class :background ,yw :foreground ,lf)))
   `(company-scrollbar-fg             ((,class :background ,bg :foreground ,lf)))
   `(company-template-field           ((,class :background ,bg :foreground ,lf)))
   `(company-tooltip                  ((,class :background ,lg :foreground ,bl)))
   `(company-tooltip-annotation       ((,class :background ,lg :foreground ,lf)))
   `(company-tooltip-common           ((,class :background ,lg :foreground ,lf)))
   `(company-tooltip-common-selection ((,class :background ,lg :foreground ,bl)))
   `(company-tooltip-selection        ((,class :background ,GR :foreground ,lg)))

   `(avy-goto-char-timer-face         ((,class :background ,yw :foreground ,fg)))

   `(avy-lead-face                    ((,class :background ,bg :foreground ,re)))
   `(avy-lead-face-0                  ((,class :background ,bg :foreground ,re)))
   `(avy-lead-face-1                  ((,class :background ,bg :foreground ,re)))
   `(avy-lead-face-2                  ((,class :background ,bg :foreground ,re)))

   `(org-todo                         ((,class :background ,bg :foreground ,re)))
   `(org-done                         ((,class :background ,bg :foreground ,fg)))
   `(org-table                        ((,class :background ,bg :foreground ,fg)))
   `(org-block                        ((,class :background ,bg :foreground ,fg)))
   `(org-code                         ((,class :background ,bg :foreground ,fg)))
   `(org-headline-done                ((,class :background ,bg :foreground ,fg)))
   `(org-latex-and-related            ((,class :background ,bg :foreground ,fg)))
   `(org-document-title               ((,class :background ,bg :foreground ,fg)))
   `(org-document-info                ((,class :background ,bg :foreground ,yw)))
   `(org-drawer                       ((,class :background ,bg :foreground ,lf)))
   `(org-date                         ((,class :background ,bg :foreground ,lf)))
   `(org-meta-line                    ((,class :background ,bg :foreground ,fg :weight bold)))

   `(org-agenda-done                  ((,class :background ,bg :foreground ,lf)))
   `(org-agenda-date                  ((,class :background ,bg :foreground ,fg)))
   `(org-agenda-date-today            ((,class :background ,bg :foreground ,fg :weight bold)))
   `(org-agenda-date-weekend          ((,class :background ,bg :foreground ,lf)))
   `(org-agenda-structure             ((,class :background ,bg :foreground ,fg)))
   `(org-scheduled-today              ((,class :background ,bg :foreground ,fg)))
   `(org-time-grid                    ((,class :background ,bg :foreground ,lf)))

   `(makefile-space                   ((,class :background ,yw :foreground ,bg)))

   `(header-line                      ((,class :background ,bu :foreground ,lf)))
   `(header-line-highlight            ((,class :background ,bu :foreground ,fg)))
   `(mode-line                        ((,class :background ,bg :foreground ,fg)))
   `(mode-line-highlight              ((,class :background ,bu :foreground ,fg)))
   `(mode-line-inactive               ((,class :background ,bg :foreground ,lf)))

   `(comint-highlight-prompt          ((,class :backgorund ,bg :foreground ,lf)))

   `(hl-todo                          ((,class :background ,bg :foreground ,lf :bold nil)))

   `(lui-highlight-face               ((,class :background ,bg :foreground ,lf)))
   `(lui-time-stamp-face              ((,class :background ,bg :foreground ,lf)))
   `(lui-button-face                  ((,class :background ,bg :foreground ,lf)))
   `(lui-server-face                  ((,class :background ,bg :foreground ,lf)))

   `(circe-highlight-nick-face        ((,class :background ,bg :foreground ,lf)))
   `(circe-my-message-face            ((,class :background ,bg :foreground ,gr)))
   `(circe-prompt-face                ((,class :background ,bg :foreground ,lf)))
   `(circe-server-face                ((,class :background ,bg :foreground ,lf)))
   `(circe-originator-face            ((,class :background ,bg :foreground ,lf)))

   `(edit-indirect-edited-region      ((,class :background ,ac :foreground ,fg)))

   `(org-transclusion-source-block    ((,class :background ,BG :foreground ,fg))))

  (setq hl-todo-keyword-faces
        `(("TODO"   . ,re)
          ("FIXME"  . ,re)
          ("BROKEN" . ,re)
          ("NOTE"   . ,fg))
        vz/mode-line-fg fg
        vz/mode-line-bg bu
        vz/mode-line-bgi bu
        vz/mode-line-fgi lf)
  (add-to-list 'default-frame-alist `(cursor-color . ,gr))

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
    (custom-theme-set-faces 'vz `(,f ((,class :inherit default)))))

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
    (custom-theme-set-faces 'vz `(,f ((,class :inhert default))))))

(provide-theme 'vz)

(provide 'vz-theme)
