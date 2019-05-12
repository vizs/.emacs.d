;; A E S T H E T I C S
;; theme TODO: make your own from a file of colors
(use-package twilight-bright-theme)

(setq-default cursor-type '(hbar . 3))
(setq-default blink-cursor-mode nil)
(defun vz:style-evil-cursor ()
  (setq-default evil-normal-state-cursor '(hbar . 3)
                evil-insert-state-cursor '(hbar . 3)
                evil-visual-state-cursor '(hbar . 3)
                evil-operator-state-cursor '(hbar . 3)))

(global-display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative
     	      display-line-numbers-width 0
     	      display-line-numbers-current-absolute t)

(add-to-list 'default-frame-alist '(font . "Share Tech Mono-10"))

;; disable bold
(mapc (lambda (face)
        (set-face-attribute face nil
          :weight 'normal
          :slant 'normal
          :underline nil))
  (face-list))

;; TODO: dont hardcode
(set-face-attribute 'fringe nil
                    :background "#ffffff")
(fringe-mode '(5 . 0))
(set-face-attribute 'mode-line nil
                    :background "#ffffff"
                    :foreground "#5f5a60")

;; "disable" line-wrap char
(set-display-table-slot standard-display-table 'wrap ? )

;; this looks fucking awful but it works so idrc
(defun vz:theme-company ()
  (require 'color)
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 0)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 0)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 8)))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face :background ,(color-lighten-name bg 0)))))
     `(company-tooltip-annotation ((t (:inherit font-lock-constant-face :background ,(color-lighten-name bg 0)))))
     `(company-tooltip-selection ((t (:inherit font-lock-constant-face :background ,(color-lighten-name bg 8)))))
     `(company-tooltip-annotation-selection ((t (:inherit font-lock-constant-face :background ,(color-lighten-name bg 8))))))))

;; highlight matching parenthesis
(setq show-paren-delay 0
      show-paren-when-point-inside-paren t)
(show-paren-mode t)
