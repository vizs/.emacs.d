;; -*- lexical-binding: t; -*-
;; Custom navigation commands and such

(defun vz/beginning-of-line ()
  "Run `back-to-indentation` or `beginning-of-line` depending
on the position of the cursor."
  (interactive)
  (let ((point (point)))
    (back-to-indentation)
    (when (eq point (point))
      (beginning-of-line))))

(defun vz/increase-number-at-point (arg)
  "Increase the number at point by ARG."
  (interactive "p")
  (-when-let (num (thing-at-point 'number))
    (-let* (((beg . end) (bounds-of-thing-at-point 'word))
            (n (- end beg)))
      (delete-region beg end)
      (insert (format (format "%%0%dd" n) (+ arg num))))))

(defun vz/decrease-number-at-point (arg)
  "Decrease the number at point by ARG."
  (interactive "p")
  (vz/increase-number-at-point (- arg)))

(vz/bind
 "C-a" #'vz/beginning-of-line
 "C-c M-+" #'vz/increase-number-at-point
 "C-c M--" #'vz/decrease-number-at-point
 ;; electric-indent-mode is considered
 "C-j" #'newline
 ;; C-x z and C-x M-ESC are hard to press
 "C-." #'repeat
 "C->" #'repeat-complex-command
 ;; I never use Emacs in a terminal
 "C-z" #'zap-up-to-char)

(use-package expand-region
  :defer t
  :functions er/expand-region
  :bind ("C-M-SPC" . er/expand-region)
  :config
  (setq expand-region-contract-fast-key "S-SPC"))

;; (use-package paredit
;;   :defer t)
