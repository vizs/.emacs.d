;; -*- lexical-binding: t; -*-

(use-package eros
  :after elisp-mode
  :config
  (setq-ns eros
    eval-result-duration nil))

;; From doom-emacs

(defun vz/emacs-lisp-indent-function (indent-point state)
  "A replacement for `lisp-indent-function'.
Indents plists more sensibly. Adapted from
https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned"
  (let ((normal-indent (current-column))
        (orig-point (point))
        ;; TODO Refactor `target' usage (ew!)
        target)
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond ((and (elt state 2)
                (or (not (looking-at-p "\\sw\\|\\s_"))
                    (eq (char-after) ?:)))
           (unless (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp)
             (goto-char calculate-lisp-indent-last-sexp)
             (beginning-of-line)
             (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t))
           (backward-prefix-chars)
           (current-column))
          ((and (save-excursion
                  (goto-char indent-point)
                  (skip-syntax-forward " ")
                  (not (eq (char-after) ?:)))
                (save-excursion
                  (goto-char orig-point)
                  (and (eq (char-after) ?:)
                       (eq (char-before) ?\()
                       (setq target (current-column)))))
           (save-excursion
             (move-to-column target t)
             target))
          ((let* ((function (buffer-substring (point) (progn (forward-sexp 1) (point))))
                  (method (or (function-get (intern-soft function) 'lisp-indent-function)
                              (get (intern-soft function) 'lisp-indent-hook))))
             (cond ((or (eq method 'defun)
                        (and (null method)
                             (> (length function) 3)
                             (string-match-p "\\`def" function)))
                    (lisp-indent-defform state indent-point))
                   ((integerp method)
                    (lisp-indent-specform method state indent-point normal-indent))
                   (method
                    (funcall method indent-point state))))))))

(defun vz/emacs-lisp-mode-init ()
  (eros-mode t)
  (setq lisp-indent-function #'vz/emacs-lisp-indent-function))

(add-hook 'emacs-lisp-mode-hook #'vz/emacs-lisp-mode-init)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
