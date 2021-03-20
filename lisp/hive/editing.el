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

(vz/bind
 "C-c M-+" #'vz/increase-number-at-point
 "C-c M--" #'vz/decrease-number-at-point

 "C-a" #'vz/beginning-of-line
 "C-j" #'newline
 "M-j" #'vz/join-line

 "C-w" #'vz/backward-delete-or-kill-region
 "M-w" #'vz/backward-kill-word-or-kill-ring-save
 "C-S-k" #'kill-whole-line
 "M-k" #'(lambda (n)
           (interactive "p")
           (kill-ring-save (point)
            (save-excursion
              (next-line (1- n))
              (line-end-position))))

 ;; `hippie-expand' also takes care of `dabbrev-expand' suggestions
 "M-/" #'hippie-expand

 ;; I never use Emacs in a terminal
 "C-z" #'zap-up-to-char

 ;; Swap `query-replace' and `query-replace-regexp'
 "M-%" #'query-replace-regexp
 "C-M-%" #'query-replace

 ;; X Clipboard
 "C-S-y" #'clipboard-yank
 "C-S-w" #'clipboard-kill-region
 "M-W" #'clipboard-kill-ring-save
 "M-K" #'(lambda ()
           (interactive)
           (clipboard-kill-ring-save (point) (line-end-position)))

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
                    :repo "tslilc/siege-mode")
  :config
  (vz/bind "M-s M-s" #'siege-explicit-call))

(use-package avy
  :demand t
  :config
  (setq avy-all-windows nil)
  (vz/bind
   "C-S-n" #'avy-goto-line-above
   "C-S-p" #'avy-goto-line-below
   :prefix "M-g"
   "f" #'avy-goto-char-2
   "g" #'avy-goto-line
   :prefix ""
   "C-M-z" #'avy-goto-char-in-line))

;; Stolen from modal editing experiment

(defun vz/jump-to-char (direction char)
  "Jump to CHAR in DIRECTION. If CHAR is not found after cursor till EOL, then
loop around and look for occurence for CHAR from the start of line."
  (interactive
   (list (if current-prefix-arg 'backward 'forward)
         (string (read-key "Search for: "))))
  (message "Searching for %s" char)
  (let* ((end    (if (eq direction 'forward) #'beginning-of-line #'end-of-line))
         (search (if (eq direction 'forward) #'re-search-forward #'re-search-backward))
         (limit  (if (eq direction 'forward) (line-end-position) (line-beginning-position)))
         (point (save-excursion
                  (condition-case nil
                      (funcall search char limit)
                    (error
                     (funcall end)
                     (funcall search char limit))))))
    (when point
      (goto-char point))))

(defvar vz/jump-to-char-forward-map (make-keymap)
  "Keymap for `vz/jump-to-char' forwards.")

(defvar vz/jump-to-char-backward-map (make-keymap)
  "Keymap for `vz/jump-to-char' backwards.")

(vz/bind
 "M-g F" #'(lambda ()
             (interactive)
             (setq overriding-local-map vz/jump-to-char-forward-map))

 :map vz/jump-to-char-forward-map
 [remap self-insert-command]  #'(lambda () (intearctive) (vz/jump-to-char 'forward (this-command-keys)))
 "C-,"                        #'(lambda () (intearctive) (setq overriding-local-map vz/jump-to-char-backward-map))
 "C-g"                        #'(lambda () (intearctive) (setq overriding-local-map nil))
 "<escape>"                   #'(lambda () (intearctive) (setq overriding-local-map nil))

 :map vz/jump-to-char-backward-map
 [remap self-insert-command]  #'(lambda () (interactive) (vz/jump-to-char 'backward (this-command-keys)))
 "C-,"                        #'(lambda () (interactive) (setq overriding-local-map vz/jump-to-char-forward-map))
 "<escape>"                   #'(lambda () (interactive) (setq overriding-local-map nil))
 "C-g"                        #'(lambda () (interactive) (setq overriding-local-map nil)))
