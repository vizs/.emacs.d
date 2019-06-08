;; A E S T H E T I C S
(setq vz:colors (split-string (vz:fread "~/var/cache/tm/colors") "\n"))
(defun vz:get-color (index)
  (nth index vz:colors))

(setq vz:color0 (vz:get-color 0) vz:color1 (vz:get-color 1) vz:color2 (vz:get-color 2)
      vz:color3 (vz:get-color 3) vz:color4 (vz:get-color 3) vz:color5 (vz:get-color 5)
      vz:color6  (vz:get-color 6) vz:color7 (vz:get-color 7) vz:color8 (vz:get-color 8)
      vz:color9  (vz:get-color 9) vz:color10 (vz:get-color 10) vz:color11 (vz:get-color 11)
      vz:color12 (vz:get-color 12) vz:color13 (vz:get-color 13) vz:color14 (vz:get-color 14)
      vz:color15 (vz:get-color 15))

(set-face-attribute 'default nil :background vz:color0   :foreground vz:color7)

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

(defun vz:theme-rainbow-parens ()
  (set-face-attribute 'rainbow-delimiters-base-face nil :foreground vz:color7)
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground vz:color1)
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground vz:color2)
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground vz:color3)
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground vz:color4)
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground vz:color5)
  (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground vz:color6)
  (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground vz:color7)
  (set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground vz:color8)
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil :background vz:color1 :foreground vz:color15)
  (set-face-attribute 'lazy-highlight nil :background vz:color8 :foreground vz:color13))

(defun vz:theme-company ()
  (set-face-attribute 'company-tooltip nil :background vz:color0 :foreground vz:color7)
  (set-face-attribute 'company-scrollbar-bg nil :background vz:color0)
  (set-face-attribute 'company-scrollbar-fg nil :background vz:color8)
  (set-face-attribute 'company-tooltip-common nil :background vz:color8 :foreground vz:color7)
  (set-face-attribute 'company-tooltip-annotation nil :background vz:color8 :foreground vz:color15)
  (set-face-attribute 'company-tooltip-selection nil :background vz:color8 :foreground vz:color5)
  (set-face-attribute 'company-tooltip-annotation-selection nil :background vz:color8 :foreground vz:color15)
  (set-face-attribute 'company-echo-common nil :background vz:color0 :foreground vz:color7)
  (set-face-attribute 'company-preview nil :background vz:color0 :foreground vz:color5)
  (set-face-attribute 'company-preview-common nil :foreground vz:color13)
  (set-face-attribute 'company-preview-search nil :background vz:color0 :foreground vz:color13))

(defun vz:theme-ivy ()
 (set-face-attribute 'ivy-current-match nil :background vz:color0 :foreground vz:color5)
 (set-face-attribute 'ivy-minibuffer-match-face-1 nil :background vz:color8 :foreground vz:color15)
 (set-face-attribute 'ivy-minibuffer-match-face-2 nil :background vz:color8 :foreground vz:color15)
 (set-face-attribute 'ivy-minibuffer-match-face-3 nil :background vz:color8 :foreground vz:color15)
 (set-face-attribute 'ivy-minibuffer-match-face-4 nil :background vz:color8 :foreground vz:color15)
 (set-face-attribute 'ivy-match-required-face nil :background vz:color8 :foreground vz:color15))

(defun vz:style-evil-cursor ()
  (setq evil-default-cursor t)
  (setq-default evil-normal-state-cursor   'box
                evil-emacs-state-cursor    'box
                evil-insert-state-cursor   '(bar . 2)
                evil-visual-state-cursor   '(hbar . 3)
                evil-replace-state-cursor  '(hbar . 3)
                evil-operator-state-cursor '(hbar . 2)
                evil-motion-state-cursor   'box))

(set-cursor-color vz:color8)
(set-face-attribute 'cursor nil :background nil)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative
              display-line-numbers-width 0
              display-line-numbers-current-absolute t)

(add-to-list 'default-frame-alist '(font . "Share Tech Mono-10"))
(add-to-list 'default-frame-alist `(cursor-color . ,vz:color8))

(defun vz:disable-bold-italic-underline ()
  (mapc (lambda (face)
          (set-face-attribute face nil
            :weight 'normal
            :slant 'normal
            :underline nil))
    (face-list)))

(set-face-attribute 'fringe nil :background (vz:get-color 0))
(fringe-mode '(5 . 0))

(setq window-divider-default-places       t
      window-divider-default-bottom-width 1
      window-divider-default-right-width  1)
(window-divider-mode t)

;; "disable" line-wrap char
(set-display-table-slot standard-display-table 'wrap ? )

;; highlight matching parenthesis
(setq show-paren-delay 0
      show-paren-when-point-inside-paren t)
(show-paren-mode t)
