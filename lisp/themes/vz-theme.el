;; -*- lexical-binding: t; -*-

(deftheme vz "A custom theme")

(let ((bg "#ffffea")
      (fg "#1c1c1d")
      (re "#ef0606")
      (gr "#cccccc")
      (lg "#e9fee9")
      (GR "#438743")
      (bl "#000000")
      (lf "#434343")
      (yw "#99994c")
      (bu "#eaffff")
      (ac "#eaed8d"))
  (custom-theme-set-faces
   'vz
   `(default                          ((t :background ,bg :foreground ,fg)))
   `(font-lock-comment-face           ((t :foreground ,lf)))

   `(show-paren-match                 ((t :background ,ac :foreground ,fg)))
   `(show-paren-match-expression      ((t :background ,ac :foreground ,fg)))
   `(show-paren-mismatch              ((t :background ,re :foreground ,fg)))

   ;; Emacs >=27 only
   `(fill-column-indicator            ((t :background ,ac :foreground ,ac)))

   `(error                            ((t :background ,bg :foreground ,re)))

   `(cursor                           ((t :background ,gr :foreground ,fg)))
   `(fringe                           ((t :background ,bg :foreground ,yw)))

   `(region                           ((t :background ,yw :foreground ,fg)))
   `(match                            ((t :background ,ac :foreground ,fg)))
   `(minibuffer-prompt                ((t :background ,bg :foreground ,fg)))
   `(isearch                          ((t :background ,ac :foreground ,fg)))
   `(lazy-highlight                   ((t :background ,ac :foreground ,fg)))
   `(highlight                        ((t :background ,ac :foreground ,fg)))
   `(trailing-whitespace              ((t :background ,yw :foreground ,fg)))
   `(flyspell-incorrect               ((t :underline (:color ,re :style wave))))

   `(beacon-fallback-background       ((t :background ,yw :foregruond ,yw)))

   `(line-number                      ((t :background ,bg :foreground ,lf)))
   `(line-number-current-line         ((t :background ,bg :foreground ,fg)))

   `(evil-mc-region-face              ((t :background ,ac :foreground ,fg)))

   `(evil-ex-info                     ((t :background ,bg :foreground ,re)))
   `(evil-ex-substitute-matches       ((t :background ,ac :foreground ,fg)))
   `(evil-ex-substitute-replacement   ((t :background ,bg :foreground ,fg)))
   `(evil-ex-search                   ((t :background ,bg :foreground ,fg)))

   `(ivy-grep-info                    ((t :background ,bg :foreground ,fg)))
   `(ivy-yanked-word                  ((t :background ,bg :foreground ,fg)))
   `(ivy-cursor                       ((t :background ,gr :foreground ,fg)))
   `(ivy-highlight-face               ((t :background ,GR :foreground ,lg)))
   `(ivy-minibuffer-match-highlight   ((t :background ,GR :foreground ,lg)))
   `(ivy-prompt-match                 ((t :background ,GR :foreground ,lg)))
   `(ivy-completions-annotations      ((t :background ,lg :foreground ,bl)))
   `(ivy-confirm-face                 ((t :background ,lg :foreground ,bl)))

   `(ivy-current-match                ((t :background ,GR :foreground ,lg)))
   `(ivy-minibuffer-match-face-1      ((t :background ,lg :foreground ,bl)))
   `(ivy-minibuffer-match-face-2      ((t :background ,lg :foreground ,bl)))
   `(ivy-minibuffer-match-face-3      ((t :background ,ac :foreground ,fg)))
   `(ivy-minibuffer-match-face-4      ((t :background ,ac :foreground ,fg)))
   `(ivy-match-required-face          ((t :background ,bg :foreground ,re)))

   `(ivy-separator                    ((t :background ,lg :foreground ,bl)))
   `(ivy-remote                       ((t :background ,bg :foreground ,lf)))

   `(swiper-background-match-face-1   ((t :background ,lg :foreground ,bl)))
   `(swiper-background-match-face-2   ((t :background ,lg :foreground ,bl)))
   `(swiper-background-match-face-3   ((t :background ,ac :foreground ,fg)))
   `(swiper-background-match-face-4   ((t :background ,ac :foreground ,fg)))

   `(swiper-line-face                 ((t :background ,GR :foreground ,lg)))
   `(swiper-line-face                 ((t :background ,GR :foreground ,lg)))
   `(swiper-match-face-3              ((t :background ,ac :foreground ,fg)))
   `(swiper-match-face-4              ((t :background ,ac :foreground ,fg)))
   `(swiper-match-face-1              ((t :background ,lg :foreground ,bl)))
   `(swiper-match-face-2              ((t :background ,lg :foreground ,bl)))

   `(completions-common-part          ((t :background ,bg :foreground ,lf)))

   `(company-echo-common              ((t :background ,ac :foreground ,fg)))
   `(company-preview                  ((t :background ,bg :foreground ,lf)))
   `(company-preview-common           ((t :background ,bg :foreground ,lf)))
   `(company-preview-search           ((t :background ,bg :foreground ,lf)))
   `(company-scrollbar-bg             ((t :background ,yw :foreground ,lf)))
   `(company-scrollbar-fg             ((t :background ,bg :foreground ,lf)))
   `(company-template-field           ((t :background ,bg :foreground ,lf)))
   `(company-tooltip                  ((t :background ,lg :foreground ,bl)))
   `(company-tooltip-annotation       ((t :background ,lg :foreground ,lf)))
   `(company-tooltip-common           ((t :background ,lg :foreground ,lf)))
   `(company-tooltip-common-selection ((t :background ,lg :foreground ,bl)))
   `(company-tooltip-selection        ((t :background ,GR :foreground ,lg)))

   `(avy-goto-char-timer-face         ((t :background ,yw :foreground ,fg)))

   `(avy-lead-face                    ((t :background ,bg :foreground ,re)))
   `(avy-lead-face-0                  ((t :background ,bg :foreground ,re)))
   `(avy-lead-face-1                  ((t :background ,bg :foreground ,re)))
   `(avy-lead-face-2                  ((t :background ,bg :foreground ,re)))

   `(org-todo                         ((t :background ,bg :foreground ,re)))
   `(org-table                        ((t :background ,bg :foreground ,fg)))
   `(org-block                        ((t :background ,bg :foreground ,fg)))
   `(org-code                         ((t :background ,bg :foreground ,fg)))
   `(org-headline-done                ((t :background ,bg :foreground ,fg)))
   `(org-latex-and-related            ((t :background ,bg :foreground ,fg)))
   `(org-document-title               ((t :background ,bg :foreground ,fg)))
   `(org-document-info                ((t :background ,bg :foreground ,yw)))
   `(org-drawer                       ((t :background ,bg :foreground ,lf)))
   `(org-date                         ((t :background ,bg :foreground ,lf)))

   `(makefile-space                   ((t :background ,yw :foreground ,bg)))

   `(header-line                      ((t :background ,bu :foreground ,lf)))
   `(header-line-highlight            ((t :background ,bu :foreground ,fg)))
   `(mode-line                        ((t :background ,bu :foreground ,fg)))
   `(mode-line-highlight              ((t :background ,bu :foreground ,fg)))
   `(mode-line-inactive               ((t :background ,bu :foreground ,lf)))

   `(comint-highlight-prompt          ((t :backgorund ,bg :foreground ,lf)))

   `(hl-todo                          ((t :background ,bg :foreground ,lf :bold nil)))

   `(lui-highlight-face               ((t :background ,bg :foreground ,lf)))
   `(lui-time-stamp-face              ((t :background ,bg :foreground ,lf)))
   `(lui-button-face                  ((t :background ,bg :foreground ,lf)))
   `(lui-server-face                  ((t :background ,bg :foreground ,lf)))

   `(circe-highlight-nick-face        ((t :background ,bg :foreground ,lf)))
   `(circe-my-message-face            ((t :background ,bg :foreground ,gr)))
   `(circe-prompt-face                ((t :background ,bg :foreground ,lf)))
   `(circe-server-face                ((t :background ,bg :foreground ,lf)))
   `(circe-originator-face            ((t :background ,bg :foreground ,lf)))

   `(edit-indirect-edited-region      ((t :background ,ac :foregroung ,fg))))

  (setq hl-todo-keyword-faces
        `(("TODO"   . ,re)
          ("FIXME"  . ,re)
          ("BROKEN" . ,re)
          ("NOTE"   . ,fg)))
  (add-to-list 'default-frame-alist `(cursor-color . ,gr))

  (dotimes (i 16)
    (vz/format-sexp
     (custom-set-faces '(lui-irc-colors-bg-%s-face ((t :inherit 'default))))
     i)
    (vz/format-sexp
     (custom-set-faces '(lui-irc-colors-fg-%s-face ((t :inherit 'default))))
     i))

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
    (custom-theme-set-faces 'vz '(f ((t :inhert default))))))

(provide-theme 'vz)

(provide 'vz-theme)
