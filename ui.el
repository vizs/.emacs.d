(dolist	(f #'(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (funcall f -1))
(blink-cursor-mode 0)

(setq-default display-line-numbers-type 'relative
	      display-line-numbers-width 0
	      display-line-current-absolute t)
(global-display-line-numbers-mode)

(add-to-list 'default-frame-alist '(font . "Verily Serif Mono:pixelsize=12"))

(setq-default left-fringe-width 2)

;; Emacs 27 feature
(when (= emacs-major-version 27)
  (setq-default display-fill-column-indicator-column 80
		display-fill-column-indicator-char "|")
  (global-display-fill-column-indicator-mode))

;; Disable all bold and italic fonts
(dolist (f (face-list))
  (set-face-attribute f nil
    :weight 'normal :slant 'normal :underline nil))

(setq-default show-paren-delay 0
	      show-paren-when-point-inside-paren t)
(show-paren-mode t)

;; Modeline
(dolist (m '(line-number-mode column-number-mode))
  (funcall m t))

(defun vz/mode-line-file-state ()
  (if (buffer-file-name)
	  (cond (buffer-read-only    " [!]")
			((buffer-modified-p) " [+]")
			(:else               ""))
	""))
  
(defun vz/mode-line-evil-state ()
  (cond
   ((eq evil-state 'visual) "    VISUAL » ")
   ((eq evil-state 'insert) "    INSERT » ")
   (:else                   "    ")))

;; From https://0x0.st/oYX8
(defun vz/mode-line-fill (face except)
  (propertize " "
			  'display `((space :align-to (- (+ right right-fringe right-margin)
											 ,except)))
			  'face face))

(setq-default
 mode-line-format `((:eval (vz/mode-line-evil-state))
					"%b"
				   (:eval (vz/mode-line-file-state))
				   (:eval (vz/mode-line-fill 'mode-line 10))
				   "« %l, %c "))
			  
;; Change faces
(load-file (vz/conf-path "theme.el"))
