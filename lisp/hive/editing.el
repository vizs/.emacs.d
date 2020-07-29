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

(defun vz/backward-kill-word-or-kill-region (arg)
  "Run `kill-region' if region is active or
`backward-kill-word'."
  (interactive "p")
  (if (use-region-p)
      (if paredit-mode
          (paredit-kill-region (region-beginning) (region-end))
        (kill-region (region-beginning) (region-end)))
    (if paredit-mode
        (paredit-backward-kill-word)
      (backward-kill-word arg))))

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
 "C-w" #'vz/backward-kill-word-or-kill-region

 ;; electric-indent-mode is considered
 "C-j" #'newline

 ;; C-x z and C-x M-ESC are hard to press
 "C-." #'repeat
 "C->" #'repeat-complex-command

 ;; I never use Emacs in a terminal
 "C-z" #'zap-up-to-char

 ;; Swap `query-replace` and `query-replace-regexp`
 "M-%" #'query-replace-regexp
 "C-M-%" #'query-replace)

(use-package expand-region
  :defer t
  :functions er/expand-region
  :bind ("C-M-SPC" . er/expand-region)
  :config
  (setq expand-region-contract-fast-key "S-SPC"))

(use-package paredit
  :defer t
  :hook ((emacs-lisp-mode
          lisp-interaction-mode
          racket-mode
          scheme-mode) . paredit-mode))
