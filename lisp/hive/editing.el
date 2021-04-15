;; -*- lexical-binding: t; -*-
;; Custom navigation commands and such

;; I never got into the habit of ending sentences with double space
(setq sentence-end-double-space nil)

(add-hook 'server-after-make-frame-hook
          (defun vz/add-to-input-decode-map ()
            (vz/bind
             :map input-decode-map
             (kbd "C-M-m") [C-M-m]
             (kbd "C-M-S-m") [C-M-S-m])
            (remove-hook #'vz/add-to-input-decode-map 'server-after-make-frame-hook)))

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
    (if (bound-and-true-p paredit-mode)
        (paredit-backward-delete arg)
      (backward-delete-char arg))))

(defun vz/backward-kill-word-or-kill-ring-save (arg)
  "Run `kill-ring-save' if region is active or
`backward-kill-word'."
  (interactive "p")
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
    (if (bound-and-true-p paredit-mode)
        (paredit-backward-kill-word)
      (backward-kill-word arg))))

(defun vz/increase-number-at-point (arg)
  "Increase the number at point by ARG."
  (interactive "p")
  (when-let ((num (thing-at-point 'number)))
    (pcase-let* ((`(,beg . ,end) (bounds-of-thing-at-point 'word))
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

(defun vz/goto-beg-and-query-replace-regexp ()
  "Go to beginning of buffer and run `query-replace-regexp'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (call-interactively #'query-replace-regexp)))

(defun vz/kill-ring-save-line (n)
  (interactive "p")
  (kill-ring-save (point)
                  (save-excursion
                    (forward-line (1- n))
                    (line-end-position))))

(defun vz/clipboard-kill-ring-save-line (n)
  (interactive "p")
  (clipboard-kill-ring-save (point)
                            (save-excursion
                             (forward-line (1- n))
                             (line-end-position))))

(vz/bind
 "C-c M-+" #'vz/increase-number-at-point
 "C-c M--" #'vz/decrease-number-at-point

 "C-a" #'vz/beginning-of-line
 "C-j" #'newline
 "M-j" #'vz/join-line

 "C-w" #'vz/backward-delete-or-kill-region
 "M-w" #'vz/backward-kill-word-or-kill-ring-save
 "C-S-k" #'kill-whole-line
 [C-M-m] #'vz/kill-ring-save-line

 ;; `hippie-expand' also takes care of `dabbrev-expand' suggestions
 "M-/" #'hippie-expand

 ;; I never use Emacs in a terminal
 "C-z" #'zap-up-to-char

 "M-%" #'query-replace-regexp
 "C-M-%" #'vz/goto-beg-and-query-replace-regexp

 ;; X Clipboard
 "C-S-y" #'clipboard-yank
 "C-S-w" #'clipboard-kill-region
 "M-W" #'clipboard-kill-ring-save
 [C-M-S-m] #'vz/clipboard-kill-ring-save-line

 ;; dwim
 "M-l" #'downcase-dwim
 "M-u" #'upcase-dwim
 "M-c" #'capitalize-dwim

 [remap just-one-space] #'cycle-spacing

 ;; Translations
 :map key-translation-map
 "M-r" (kbd "C-x r"))

(use-package isearch
  :straight (:type built-in)
  :config
  (vz/bind
   ;; Use isearch regexp
   "C-s" #'isearch-forward-regexp
   "C-r" #'isearch-backward-regexp

   :map isearch-mode-map
   "C-'" #'avy-isearch)
  (setq isearch-allow-scroll t)
  ;; This is from oantolin's config
  (add-hook 'isearch-mode-hook
            (defun vz/isearch-insert-region-if-active ()
              "Insert region as isearch query if it is active."
              (when (use-region-p)
                (isearch-yank-string (buffer-substring-no-properties
                                      (region-beginning) (region-end)))
                (deactivate-mark)))))

(use-package expand-region
  :defer t
  :functions (er/expand-region)
  :bind ("C-M-SPC" . er/expand-region)
  :config
  (setq expand-region-contract-fast-key "S-SPC"
        expand-region-smart-cursor t)
  (when (load-file (expand-file-name "lisp/hive/org-expand-region.el" user-emacs-directory))
    (remove-hook 'org-mode-hook #'er/add-org-mode-expansions)
    (er/enable-mode-expansions 'org-mode #'gb/er/config-org-mode-expansions)))

(use-package paredit
  :defer t
  :bind (:map paredit-mode-map
              ("M-r" . nil)
              ("M-s" . nil)
              ("M-S-r" . paredit-raise-sexp))
  :hook ((emacs-lisp-mode
          lisp-interaction-mode
          racket-mode
          scheme-mode) . paredit-mode))

(use-package siege-mode
  :straight ( :type git :host github
                    :repo "vizs/siege-mode")
  :config
  (vz/bind "M-s M-s" #'siege-explicit-call))

(use-package avy
  :demand t
  :config
  (setq avy-all-windows nil)
  (vz/bind
   "C-S-n" #'avy-goto-line-below
   "C-S-p" #'avy-goto-line-above

   "C-S-f" (defun vz/avy-goto-char-forward (char)
             (interactive (list (read-char "Character: " t)))
             (avy-with vz/avy-goto-char-forward
               (avy-jump (regexp-quote (string char))
                         :beg (point)
                         :end (line-end-position))))

   "C-S-b" (defun vz/avy-goto-char-backward (char)
             (interactive (list (read-char "Character: " t)))
             (avy-with vz/avy-goto-char-backward
               (avy-jump (regexp-quote (string char))
                         :beg (line-beginning-position)
                         :end (point))))

   "M-F" (defun vz/avy-goto-word-forward ()
           (interactive)
           (avy-with vz/avy-goto-word-forward
             (avy-jump avy-goto-word-0-regexp
                       :beg (point)
                       :end (line-end-position))))

   "M-B" (defun vz/avy-goto-word-backward ()
           (interactive)
           (avy-with vz/avy-goto-word-backward
             (avy-jump avy-goto-word-0-regexp
                       :beg (line-beginning-position)
                       :end (point))))

   "C-M-z" #'avy-goto-char-in-line

   :prefix "M-g"
   "f" #'avy-goto-char-2
   "g" #'avy-goto-line))
