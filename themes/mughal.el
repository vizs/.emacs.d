(setq vz/accent    vz/color15
      vz/accent1   vz/color7
      vz/accent2   vz/color7
      vz/highlight vz/color1)

(custom-set-faces
 `(default ((t :background ,vz/color0 :foreground ,vz/color7)))

 `(font-lock-builtin-face       ((t :foreground ,vz/accent1)))
 `(font-lock-preprocessor-face  ((t :foreground ,vz/color7)))
 `(font-lock-constant-face      ((t :foreground ,vz/accent2)))
 `(font-lock-comment-face       ((t :foreground ,vz/color8)))
 `(font-lock-negation-char-face ((t :foreground ,vz/color7)))
 `(font-lock-constant-face      ((t :foreground ,vz/color7)))
 `(font-lock-doc-face           ((t :foreground ,vz/accent2)))
 `(font-lock-function-name-face ((t :foreground ,vz/accent)))
 `(font-lock-keyword-face       ((t :foreground ,vz/accent1)))
 `(font-lock-string-face        ((t :foreground ,vz/accent)))
 `(font-lock-type-face          ((t :foreground ,vz/accent2)))
 `(font-lock-variable-name-face ((t :foreground ,vz/accent1)))
 `(font-lock-warning-face       ((t :foreground ,vz/accent1)))

 `(region    ((t :background ,vz/color8 :foreground ,vz/color15)))
 `(highlight ((t :background ,vz/color8 :foreground ,vz/color15)))
 `(match     ((t :background ,vz/color8 :foreground ,vz/highlight)))

 `(show-paren-match    ((t :background ,vz/color8 :foreground ,vz/color15)))
 `(show-paren-mismatch ((t :background ,vz/color1 :foreground ,vz/color15)))

 `(isearch           ((t :background ,vz/color8 :foreground ,vz/color15)))
 `(minibuffer-prompt ((t :foreground ,vz/color7)))

 `(link    ((t :foreground ,vz/accent)))
 `(warning ((t :foreground ,vz/color3)))

 `(trailing-whitespace ((t :background ,vz/color8)))

 `(line-number              ((t :foreground ,vz/color8)))
 `(line-number-current-line ((t :foreground ,vz/color15)))

 `(window-divider             ((t :foreground ,vz/color7)))
 `(window-divider-first-pixel ((t :foreground ,vz/color7)))
 `(window-divider-last-pixel  ((t :foreground ,vz/color7)))
 `(fringe                     ((t :foreground ,vz/color0)))
 `(vertical-border            ((t :foreground ,vz/color0 :background ,vz/color0)))

 `(org-table                 ((t :foreground ,vz/accent2)))
 `(org-document-info-keyword ((t :foreground ,vz/accent2)))
 `(org-document-title        ((t :foreground ,vz/accent2)))

 `(sh-heredoc     ((t :foreground ,vz/accent2)))
 `(sh-quoted-exec ((t :foreground ,vz/color7)))

 `(rainbow-delimiters-base-face      ((t :foreground ,vz/accent1)))
 `(rainbow-delimiters-depth-1-face   ((t :foreground ,vz/accent1)))
 `(rainbow-delimiters-depth-2-face   ((t :foreground ,vz/accent1)))
 `(rainbow-delimiters-depth-3-face   ((t :foreground ,vz/accent1)))
 `(rainbow-delimiters-depth-4-face   ((t :foreground ,vz/accent1)))
 `(rainbow-delimiters-depth-5-face   ((t :foreground ,vz/accent1)))
 `(rainbow-delimiters-depth-6-face   ((t :foreground ,vz/accent1)))
 `(rainbow-delimiters-depth-7-face   ((t :foreground ,vz/accent1)))
 `(rainbow-delimiters-depth-8-face   ((t :foreground ,vz/accent1)))
 `(rainbow-delimiters-unmatched-face ((t :background ,vz/color1 :foreground ,vz/accent2)))
 `(lazy-highlight                    ((t :background ,vz/color8 :foreground ,vz/highlight)))

 `(evil-ex-substitute-matches      ((t :background ,vz/color0 :foreground ,vz/highlight)))
 `(evil-ex-substitute-replacement  ((t :background ,vz/color0 :foreground ,vz/color1)))
 `(evil-ex-info                    ((t :background ,vz/color0 :foreground ,vz/color7)))
 `(evil-ex-search                  ((t :background ,vz/color0 :foreground ,vz/color7)))

 `(company-tooltip                      ((t :background ,vz/color0 :foreground ,vz/color7)))
 `(company-scrollbar-bg                 ((t :background ,vz/color0)))
 `(company-scrollbar-fg                 ((t :background ,vz/color8)))
 `(company-tooltip-common               ((t :background ,vz/color0 :foreground ,vz/color8)))
 `(company-tooltip-annotation           ((t :background ,vz/color0 :foreground ,vz/color15)))
 `(company-tooltip-selection            ((t :background ,vz/color0 :foreground ,vz/highlight)))
 `(company-tooltip-annotation-selection ((t :background ,vz/color0 :foreground ,vz/color8)))
 `(company-echo-common                  ((t :background ,vz/color0 :foreground ,vz/color7)))
 `(company-preview                      ((t :background ,vz/color0 :foreground ,vz/highlight)))
 `(company-preview-common               ((t :foreground ,vz/color13)))
 `(company-preview-search               ((t :background ,vz/color0 :foreground ,vz/highlight)))

 `(ivy-current-match           ((t :background ,vz/color0 :foreground ,vz/highlight)))
 `(ivy-minibuffer-match-face-1 ((t :background ,vz/color0 :foreground ,vz/color8)))
 `(ivy-minibuffer-match-face-2 ((t :background ,vz/color0 :foreground ,vz/color8)))
 `(ivy-minibuffer-match-face-3 ((t :background ,vz/color0 :foreground ,vz/color8)))
 `(ivy-minibuffer-match-face-4 ((t :background ,vz/color0 :foreground ,vz/color8)))
 `(ivy-current-face            ((t :background ,vz/color0 :foreground ,vz/color15)))
 `(ivy-current-grep-info       ((t :background ,vz/color0 :foreground ,vz/color15)))
 `(ivy-match-required-face     ((t :background ,vz/color0 :foreground ,vz/color8)))
 `(ivy-cursor                  ((t :background ,vz/color8)))

 `(racket-selfeval-face         ((t :foreground ,vz/color7)))
 `(racket-keyword-argument-face ((t :foreground ,vz/color15)))
 `(cursor                       ((t :background ,vz/color8))))
