;; A E S T H E T I C S
(setq vz/colors (split-string (vz/fread "~/var/cache/tm/colors") "\n"))
(defun vz/get-color (index)
  (nth index vz/colors))

(setq vz/color0 (vz/get-color 0) vz/color1 (vz/get-color 1) vz/color2 (vz/get-color 2)
      vz/color3 (vz/get-color 3) vz/color4 (vz/get-color 3) vz/color5 (vz/get-color 5)
      vz/color6  (vz/get-color 6) vz/color7 (vz/get-color 7) vz/color8 (vz/get-color 8)
      vz/color9  (vz/get-color 9) vz/color10 (vz/get-color 10) vz/color11 (vz/get-color 11)
      vz/color12 (vz/get-color 12) vz/color13 (vz/get-color 13) vz/color14 (vz/get-color 14)
      vz/color15 (vz/get-color 15))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative
              display-line-numbers-width 0
              display-line-numbers-current-absolute t)

(add-to-list 'default-frame-alist '(font . "ctrld:pixelsize=14"))
(add-to-list 'default-frame-alist `(cursor-color . ,vz/color8))

(defun vz/disable-bold-italic-underline ()
  (mapc (lambda (face)
          (set-face-attribute face nil
            :weight 'normal
            :slant 'normal
            :underline nil))
    (face-list)))

(set-face-attribute 'fringe nil :background (vz/get-color 0))
(fringe-mode '(10 . 10))

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

(defun vz/style-evil-cursor ()
  (setq evil-default-cursor t)
  (setq-default evil-normal-state-cursor   'box
                evil-emacs-state-cursor    'box
                evil-insert-state-cursor   '(bar . 2)
                evil-visual-state-cursor   '(hbar . 3)
                evil-replace-state-cursor  '(hbar . 3)
                evil-operator-state-cursor '(hbar . 2)
                evil-motion-state-cursor   'box))

(vz/load-elfiles '("themes/mayan.el"))
