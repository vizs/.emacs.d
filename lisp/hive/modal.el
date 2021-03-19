;; -*- lexical-binding: t; eval: (outshine-mode t); -*-
;; My dream modal editor -- check modal.org in root

;; * Ryo modal changes

;; Better than modalka and has some more useful extensions
;; like use-package integration and repeat command...
(use-package ryo-modal)

;; ** Suppress ryo-modal-keymap

;; Keys that aren't bound yet don't self-insert
(suppress-keymap ryo-modal-mode-map)

;; ** Ryo modal cursor

;; When in `ryo-modal-mode', you can change the cursor
;; colour and/or shape. There's a bug related to changing
;; the colour (see #24), but cursor shape works just fine

;; Set "insert" mode cursor to hbar
(setq-default cursor-type 'bar
              ryo-modal-cursor-type 'box
              ryo-modal-cursor-color nil)

;; ** Escape out of "insert" mode

(defun vz/m-normal-mode ()
  "Enable `ryo-modal-mode'"
  (interactive)
  (if (minibufferp)
      (minibuffer-keyboard-quit)
    (ryo-modal-mode t)))

(global-set-key (kbd "<escape>") #'vz/m-normal-mode)

;; * Basic navigation
;; ** Block based movement functions

(dolist (d '(forward backward))
  (vz/format-sexp
   (defun vz/m-%1$s-block (&optional arg)
     "Move %1$s-block based on `major-mode'. If `major-mode'
is a lisp, then do sexp movement. If `major-mode' is python,
then do `python-nav-%1$s-block'. Otherwise, do '%1$s-paragraph'"
     (interactive "p")
     (cond
      ((derived-mode-p 'lisp-mode 'emacs-lisp-mode
                       'scheme-mode 'racket-mode)
       (%1$s-sexp arg))
      ((derived-mode-p 'python-mode) (python-nav-%1$s-block arg))
      (t (%1$s-paragraph arg))))
   d))

;; ** d command

;; `delete-forward-char' takes an optional argument to save in killring
(defun vz/m-kill-char (&optional arg)
  (interactive "p")
  (delete-forward-char arg t))

;; ** f and F commands

;; Continuous searching is done by temporarily
;; setting `overriding-terminal-local-map' to
;; `vz/m-search-char-map' which will take over
;; any other keybindings.

;; To prevent ryo-modal-map from acting up,
;; we disable it during the selection time.

;; *** Keymap

;; Keymap to be passed to `set-transient-map'
(defvar vz/m-search-char-map (make-keymap)
  "Keymap that is passed to `set-transient-map' when
using the f and F commands.")

;; *** Implementation

(defun vz/m-search-char--forward  (char)
  "Search for CHAR forwards, wrapping to first match if not
found ahead"
  (let ((point (save-excursion
                 (forward-char)
                 (condition-case nil
                     (re-search-forward char (line-end-position))
                   (error (beginning-of-line)
                          (re-search-forward char (line-end-position)))))))
    (when point
      (goto-char (1- point)))))

(defun vz/m-search-char--backward (char)
  "Like `vz/m-search-char--forward' but search backwards"
  (let ((point (save-excursion
                 (backward-char)
                 (condition-case nil
                     (re-search-backward char (line-beginning-position))
                   (error (end-of-line)
                          (re-search-backward char (line-beginning-position)))))))
    (when point
      (goto-char point))))

(dolist (d '(forward backward))
  (vz/format-sexp
   (defun vz/m-search-char-%1$s ()
     "Search for character %1$s continuously until ESC is pressed"
     (interactive)
     (ryo-modal-mode -1)
     (let ((cursor-type ryo-modal-cursor-type))
       (setq overriding-terminal-local-map vz/m-search-char-map)
       (define-key vz/m-search-char-map [remap self-insert-command]
         (fn! (vz/m-search-char--%1$s (this-command-keys))))
       (define-key vz/m-search-char-map (kbd "<escape>")
         (fn! (setq overriding-terminal-local-map nil)
              (ryo-modal-mode t)))))
   d))

;; ** Kill till EOL

(defun vz/m-kill-sentence ()
  "Like `kill-sentence' but disregards prefix argument"
  (interactive)
  (kill-sentence))

;; ** Binding

(ryo-modal-keys
 ("a"   ryo-modal-mode :then '(forward-char))
 ("A"   ryo-modal-mode :then '(move-end-of-line))
 ("C-a" ryo-modal-mode :then '(move-beginning-of-line))
 ("M-a" ryo-modal-mode)

 ("h" backward-char)
 ("l" forward-char)
 ("j" next-line)
 ("k" previous-line)

 ("H" backward-word)
 ("L" forward-word)

 ("[" forward-paragraph)
 ("]" backward-paragraph)

 ("C-{" vz/m-forward-block)
 ("C-}" vz/m-backward-block)

 ("d" vz/m-kill-char)
 ("D" vz/m-kill-sentence)

 ;; TODO Bind M-s and M-S-s to zzz commands
 ("s" vz/m-kill-char     :exit t)
 ("S" vz/m-kill-sentence :exit t)

 ("f" vz/m-search-char-forward  :norepeat t)
 ("F" vz/m-search-char-backward :norepeat t))

;; Prefix keys

(ryo-modal-keys
 (:norepeat t)
 ("0" "M-0")
 ("1" "M-1")
 ("2" "M-2")
 ("3" "M-3")
 ("4" "M-4")
 ("5" "M-5")
 ("6" "M-6")
 ("7" "M-7")
 ("8" "M-8")
 ("9" "M-9"))

;; * Searching text

;; ctrlf is the best candidate for searching text.

(use-package ctrlf
  :init
  (setq ctrlf-mode-bindings '()
        ctrlf-minibuffer-bindings '())
  :ryo
  ("C-f"   ctrlf-forward-fuzzy)
  ("C-S-f" ctrlf-backward-fuzzy)
  :config
  (defun vz/m-search--prompt (_ &rest args)
    "A function as an advice around `ctrlf--prompt' to
make a simplified prompt"
    (format "Search%s: "
            (if ctrlf--backward-p " Backward" "")))
  (advice-add 'ctrlf--prompt :around #'vz/m-search--prompt)
  (ctrlf-mode t)
  (setq ctrlf-minibuffer-bindings
        ;; TODO Add search type change functions
        `(("C-j"             . ctrlf-next-match)
          (,(kbd "C-k")      . ctrlf-previous-match)
          (,(kbd "C-u")      . ctrlf-previous-page)
          (,(kbd "C-d")      . ctrlf-next-page)
          (,(kbd "<return>") . ctrlf-cancel)
          (,(kbd "<escape>") . ctrlf-cancel))))
