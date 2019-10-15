(set-face-attribute 'default nil :background vz/color0 :foreground vz/color7)

(set-face-attribute 'font-lock-builtin-face nil :foreground vz/color2)
(set-face-attribute 'font-lock-preprocessor-face nil :foreground vz/color4)
(set-face-attribute 'font-lock-constant-face nil :foreground vz/color1)
(set-face-attribute 'font-lock-comment-face nil :foreground vz/color8)
(set-face-attribute 'font-lock-negation-char-face nil :foreground vz/color1)
(set-face-attribute 'font-lock-constant-face nil :foreground vz/color2)
(set-face-attribute 'font-lock-doc-face nil :foreground vz/color8)
(set-face-attribute 'font-lock-function-name-face nil :foreground vz/color5)
(set-face-attribute 'font-lock-keyword-face nil :foreground vz/color1)
(set-face-attribute 'font-lock-string-face nil :foreground vz/color5)
(set-face-attribute 'font-lock-type-face nil :foreground vz/color10)
(set-face-attribute 'font-lock-variable-name-face nil :foreground vz/color4)
(set-face-attribute 'font-lock-warning-face nil :foreground vz/color3)

(set-face-attribute 'region nil :background vz/color8 :foreground vz/color15)
(set-face-attribute 'highlight nil :background vz/color8 :foreground vz/color15)

(set-face-attribute 'show-paren-match nil :background vz/color8 :foreground vz/color15)
(set-face-attribute 'show-paren-mismatch nil :background vz/color1 :foreground vz/color15)

(set-face-attribute 'isearch nil :background vz/color8 :foreground vz/color15)

(set-face-attribute 'vertical-border nil :foreground vz/color15)

(set-face-attribute 'minibuffer-prompt nil :foreground vz/color5)

(set-face-attribute 'link nil :foreground vz/color5)
(set-face-attribute 'warning nil :foreground vz/color3)

(set-face-attribute 'trailing-whitespace nil :background vz/color8)

(set-face-attribute 'line-number nil :foreground vz/color8)
(set-face-attribute 'line-number-current-line nil :foreground vz/color15)

(set-face-attribute 'window-divider nil :foreground vz/color7)

(defun vz/--sh ()
  (set-face-attribute 'sh-heredoc     nil :foreground vz/color2)
  (set-face-attribute 'sh-quoted-exec nil :foreground vz/color4))
(add-hook 'sh-mode-hook 'vz/--sh)

(defun vz/theme-rainbow-parens ()
  (set-face-attribute 'rainbow-delimiters-base-face nil :foreground vz/color7)
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground vz/color1)
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground vz/color2)
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground vz/color3)
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground vz/color4)
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground vz/color5)
  (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground vz/color6)
  (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground vz/color7)
  (set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground vz/color15)
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil :background vz/color1 :foreground vz/color15)
  (set-face-attribute 'lazy-highlight nil :background vz/color8 :foreground vz/color13))

(defun vz/theme-company ()
  (set-face-attribute 'company-tooltip nil :background vz/color0 :foreground vz/color7)
  (set-face-attribute 'company-scrollbar-bg nil :background vz/color0)
  (set-face-attribute 'company-scrollbar-fg nil :background vz/color8)
  (set-face-attribute 'company-tooltip-common nil :background vz/color8 :foreground vz/color7)
  (set-face-attribute 'company-tooltip-annotation nil :background vz/color8 :foreground vz/color15)
  (set-face-attribute 'company-tooltip-selection nil :background vz/color8 :foreground vz/color5)
  (set-face-attribute 'company-tooltip-annotation-selection nil :background vz/color8 :foreground vz/color15)
  (set-face-attribute 'company-echo-common nil :background vz/color0 :foreground vz/color7)
  (set-face-attribute 'company-preview nil :background vz/color0 :foreground vz/color5)
  (set-face-attribute 'company-preview-common nil :foreground vz/color13)
  (set-face-attribute 'company-preview-search nil :background vz/color0 :foreground vz/color13))

(defun vz/theme-ivy ()
 (set-face-attribute 'ivy-current-match nil :background vz/color0 :foreground vz/color5)
 (set-face-attribute 'ivy-minibuffer-match-face-1 nil :background vz/color8 :foreground vz/color15)
 (set-face-attribute 'ivy-minibuffer-match-face-2 nil :background vz/color8 :foreground vz/color15)
 (set-face-attribute 'ivy-minibuffer-match-face-3 nil :background vz/color8 :foreground vz/color15)
 (set-face-attribute 'ivy-minibuffer-match-face-4 nil :background vz/color8 :foreground vz/color15)
 (set-face-attribute 'ivy-match-required-face nil :background vz/color8 :foreground vz/color15))

(defun vz/theme-racket ()
  (set-face-attribute 'racket-selfeval-face nil :foreground vz/color2)
  (set-face-attribute 'racket-keyword-argument-face nil :foreground vz/color1))

(set-cursor-color vz/color15)
(set-face-attribute 'cursor nil :background nil)
