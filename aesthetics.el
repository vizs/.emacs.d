;; A E S T H E T I C S
;; theme TODO: make your own from a file of colors
(use-package twilight-bright-theme
  :ensure t)

;; cursor face
(setq-default cursor-type '(hbar . 3))
(setq-default blink-cursor-mode nil)

;; line numbers
(global-display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative
     	      display-line-numbers-width 2
     	      display-line-numbers-current-absolute t)

;; font
(set-face-attribute 'default nil :font "Share Tech Mono-11")

;; disable bold
(mapc (lambda (face)
        (set-face-attribute face nil
          :weight 'normal
          :slant 'normal
          :underline nil
          ;;:inherit nil
          ))
  (face-list))

;; hide fringe
(set-fringe-mode '(0 . 0))

;; company mode aesthetics
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
