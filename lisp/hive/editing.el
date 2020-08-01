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

(defun vz/backward-delete-or-kill-region (arg)
  "Run `kill-region' if region is active or
`backward-delete-char'."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (if paredit-mode
        (paredit-backward-delete arg)
      (backward-delete-char arg))))

(defun vz/backward-kill-word-or-kill-ring-save (arg)
  "Run `kill-ring-save' if region is active or
`backward-kill-word'."
  (interactive "p")
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
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

(defun vz/join-line (arg)
  "Reverse of `delete-indentation'."
  (interactive "P")
  (delete-indentation (not arg)))

(vz/bind
 "C-c M-+" #'vz/increase-number-at-point
 "C-c M--" #'vz/decrease-number-at-point

 "C-a" #'vz/beginning-of-line
 "M-j" #'vz/join-line

 "C-w" #'vz/backward-delete-or-kill-region
 "M-w" #'vz/backward-kill-word-or-kill-ring-save

 ;; electric-indent-mode is considered
 "C-j" #'newline

 ;; C-x z and C-x M-ESC are hard to press
 "C-." #'repeat
 "C->" #'repeat-complex-command

 ;; I never use Emacs in a terminal
 "C-z" #'zap-up-to-char

 ;; Swap `query-replace` and `query-replace-regexp`
 "M-%" #'query-replace-regexp
 "C-M-%" #'query-replace

 ;; X Clipboard
 "C-S-y" #'clipboard-yank
 "C-S-w" #'clipboard-kill-region
 "M-S-w" #'clipboard-kill-ring-save

 ;; Use isearch regexp
 "C-s" #'isearch-forward-regexp
 "C-r" #'isearch-backward-regexp

 ;; Translations
 :map key-translation-map
 "M-r" (kbd "C-x r"))

(use-package expand-region
  :defer t
  :functions er/expand-region
  :bind ("C-M-SPC" . er/expand-region)
  :config
  (setq-ns expand-region
    contract-fast-key "S-SPC"
    smart-cursor t))

(use-package paredit
  :defer t
  :bind (:map paredit-mode-map
              ("M-r" . nil)
              ("M-S-r" . paredit-raise-sexp))
  :hook ((emacs-lisp-mode
          lisp-interaction-mode
          racket-mode
          scheme-mode) . paredit-mode))

(use-package avy
  :demand t
  :config
  (setq avy-all-windows nil)
  (vz/bind
   :prefix "M-g"
   "c" #'avy-goto-char-in-line
   "g" #'avy-goto-line))
