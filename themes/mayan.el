(set-face-attribute 'default nil :background vz:color0 :foreground vz:color7)

(set-face-attribute 'font-lock-builtin-face nil :foreground vz:color2)
(set-face-attribute 'font-lock-preprocessor-face nil :foreground vz:color4)
(set-face-attribute 'font-lock-constant-face nil :foreground vz:color1)
(set-face-attribute 'font-lock-comment-face nil :foreground vz:color8)
(set-face-attribute 'font-lock-negation-char-face nil :foreground vz:color1)
(set-face-attribute 'font-lock-constant-face nil :foreground vz:color2)
(set-face-attribute 'font-lock-doc-face nil :foreground vz:color8)
(set-face-attribute 'font-lock-function-name-face nil :foreground vz:color5)
(set-face-attribute 'font-lock-keyword-face nil :foreground vz:color1)
(set-face-attribute 'font-lock-string-face nil :foreground vz:color5)
(set-face-attribute 'font-lock-type-face nil :foreground vz:color10)
(set-face-attribute 'font-lock-variable-name-face nil :foreground vz:color4)
(set-face-attribute 'font-lock-warning-face nil :foreground vz:color3)

(set-face-attribute 'region nil :background vz:color8 :foreground vz:color15)
(set-face-attribute 'highlight nil :background vz:color8 :foreground vz:color15)

(set-face-attribute 'show-paren-match nil :background vz:color8 :foreground vz:color15)
(set-face-attribute 'show-paren-mismatch nil :background vz:color1 :foreground vz:color15)

(set-face-attribute 'isearch nil :background vz:color8 :foreground vz:color15)

(set-face-attribute 'vertical-border nil :foreground vz:color15)

(set-face-attribute 'minibuffer-prompt nil :foreground vz:color5)

(set-face-attribute 'link nil :foreground vz:color5)

(set-face-attribute 'warning nil :foreground vz:color3)

(set-face-attribute 'trailing-whitespace nil :background vz:color8)

(set-face-attribute 'line-number nil :foreground vz:color8)
(set-face-attribute 'line-number-current-line nil :foreground vz:color5)
`(add-hook 'sh-mode-hook ,(lambda (x)
                ((set-face-attribute 'sh-heredoc nil :foreground vz:color2)
                (set-face-attribute 'sh-quoted-exec nil :foreground vz:color4))))

(set-cursor-color vz:color8)
(set-face-attribute 'cursor nil :background nil)
