;; A E S T H E T I C S

(let ((colors "~/var/cache/tm/colors.el"))
  (load-file (if (file-exists-p colors)
                 colors
               (concat user-emacs-directory "/themes/defcolors.el"))))

(when vz/show-numbers?
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(setq-default display-line-numbers-type            'relative
              display-line-numbers-width            0
              display-line-numbers-current-absolute t)

(add-to-list 'default-frame-alist `(font . ,(vz/getenv-or "XFONT" "monospace:pixelsize=12")))
(add-to-list 'default-frame-alist `(cursor-color . ,vz/color8))

(defun vz/disable-bold-italic-underline ()
  (mapc (lambda (face)
          (set-face-attribute face nil
            :weight    'normal
            :slant     'normal
            :underline  nil))
    (face-list)))

(fringe-mode '(20 . 0))

(defun vz/count-windows ()
  (length (mapcar #'window-buffer
                  (window-list nil nil))))

(setq window-divider-default-places       t
      window-divider-default-bottom-width 1
      window-divider-default-right-width  1)

(setq vz/draw-window-dividers? t)

(defun vz/draw-window-dividers ()
  "draw window-dividers only when one window is open"
  (if (= (vz/count-windows) 1)
      (window-divider-mode -1)
    (window-divider-mode t)))

(defun vz/toggle-window-dividers ()
  (interactive)
  (setq vz/draw-window-dividers? (not vz/draw-window-dividers?))
  (if vz/draw-window-dividers?
      (setq window-divider-default-right-width  1
            window-divider-default-bottom-width 1)
    (setq window-divider-default-right-width  0
          window-divider-default-bottom-width 0))
  (vz/draw-window-dividers))

(add-hook 'window-configuration-change-hook 'vz/draw-window-dividers)

;; wrapping
(set-display-table-slot standard-display-table 'wrap ? )

;; don't show curly arrow in fringe
(setf (cdr (assq 'continuation fringe-indicator-alist))
      '(nil nil))

;; highlight matching parenthesis
(when vz/show-paren-highlight?
  (show-paren-mode t))

(setq show-paren-delay                   0
      show-paren-when-point-inside-paren t)

(defun vz/style-evil-cursor ()
  (setq evil-default-cursor t)
  (setq-default evil-normal-state-cursor   'box
                evil-emacs-state-cursor    'box
                evil-insert-state-cursor   '(bar  . 2)
                evil-visual-state-cursor   '(hbar . 3)
                evil-replace-state-cursor  '(hbar . 3)
                evil-operator-state-cursor '(hbar . 2)
                evil-motion-state-cursor   'box))

(vz/load-elfiles '("themes/mayan.el"))
