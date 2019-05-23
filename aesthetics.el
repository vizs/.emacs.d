;; A E S T H E T I C S
;; theme TODO: make your own from a file of colors
;; TODO: dont hardcode
(use-package twilight-bright-theme)

(setq-default cursor-type '(hbar . 3))
(setq-default blink-cursor-mode nil)
(defun vz:style-evil-cursor ()
  (setq-default evil-normal-state-cursor   'box
                evil-emacs-state-cursor    'box
                evil-insert-state-cursor   '(bar . 2)
                evil-visual-state-cursor   '(hbar . 3)
                evil-replace-state-cursor  '(hbar . 3)
                evil-operator-state-cursor '(hbar . 2)
                evil-motion-state-cursor   'box))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative
              display-line-numbers-width 0
              display-line-numbers-current-absolute t)

(add-to-list 'default-frame-alist '(font . "ttyp0-8"))

;; disable bold, italic
(defun vz:disable-bold-italic ()
  (mapc (lambda (face)
          (set-face-attribute face nil
            :weight 'normal
            :slant 'normal
            :underline nil))
    (face-list)))

(set-face-attribute 'fringe nil :background "#ffffff")
(fringe-mode '(5 . 0))

(setq window-divider-default-places       t
      window-divider-default-bottom-width 1
      window-divider-default-right-width  1)
(window-divider-mode t)

;; "disable" line-wrap char
(set-display-table-slot standard-display-table 'wrap ? )

(defun vz:theme-company ()
  (custom-set-faces
    `(company-tooltip ((t (:inherit default :background "#ffffff"))))
    `(company-scrollbar-bg ((t (:background "#ffffff"))))
    `(company-scrollbar-fg ((t (:background "#a7a7a7"))))
    `(company-tooltip-common ((t (:inherit font-lock-constant-face
                                           :background "#a7a7a7"
                                           :foreground "#5f5a60"))))
    `(company-tooltip-annotation ((t (:inherit font-lock-constant-face
                                               :background "#a7a7a7"
                                               :foreground "#464b50"))))
    `(company-tooltip-selection ((t (:inherit font-lock-constant-face
                                              :background "#a7a7a7"
                                              :foreground "#464b50"))))
    `(company-tooltip-annotation-selection ((t (:inherit font-lock-constant-face
                                                         :background "#a7a7a7"
                                                         :foreground "#464b50"))))))

(defun vz:theme-ivy ()
  (custom-set-faces
   `(ivy-current-match ((t (:background "#ffffff" :foreground "#9b859d"))))
   `(ivy-minibuffer-match-face-1 ((t (:background "#a7a7a7" :foreground "#464b50"))))
   `(ivy-minibuffer-match-face-2 ((t (:inherit ivy-minibuffer-match-face-1))))
   `(ivy-minibuffer-match-face-3 ((t (:inherit ivy-minibuffer-match-face-1))))
   `(ivy-minibuffer-match-face-4 ((t (:inherit ivy-minibuffer-match-face-1))))
   `(ivy-match-required-face ((t (:background "#a7a7a7" :foreground "#464b50"))))))

;; highlight matching parenthesis
(setq show-paren-delay 0
      show-paren-when-point-inside-paren t)
(show-paren-mode t)
