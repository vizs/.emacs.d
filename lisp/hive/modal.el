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
  (interactive)
  (ryo-modal-mode t))

(global-set-key (kbd "<escape>") #'vz/m-normal-mode)

;; * Basic navigation
;; ** Block based movement functions

(dolist (d '(forward backward))
  (vz/format-sexp
   (defun vz/m-%1$s-block ()
     "Move %1$s-block based on `major-mode'. If `major-mode'
is a lisp, then do sexp movement. If `major-mode' is python,
then do `python-nav-%1$s-block'. Otherwise, do '%1$s-paragraph'"
     (interactive)
     (pcase major-mode
       ((or emacs-lisp-mode scheme-mode
            racket-mode lisp-mode)
        (%1$s-sexp))
       (python-mode (python-nav-%1$s-block))
       (_ (%1$s-paragraph))))
   d))

;; ** d command

;; `delete-forward-char' takes an optional argument to save in killring

(defun vz/m-kill-char ()
  (interactive)
  (delete-forward-char 1 t))

;; ** TODO f and F commands

;; The continuously ask for character and do thing
;; can be achieved using `set-transient-map' I think.
;; Although this might be the worst possible approach
;; you could take.

;; *** Keymap

;; Keymap to be passed to `set-transient-map'
(defvar vz/m-find-map (make-keymap)
  "Keymap that is passed to `set-transient-map' when
using the f and F commands.")

;; Make all characters *and* digits undefined
(suppress-keymap vz/m-find-map t)

;; *** Helper function

;; We need to set the letters and digits in `vz/m-find-map'
;; to a function before we run the f and F commands.
;; This function has to be added to :before in `ryo-modal-mode-map'
(defun vz/m--set-f-function (fun)
  )

;; *** Implementation

(defun vz/m-search-char-forward ()
  )

(defun vz/m-search-char-backward ()
  )

;; ** Binding

(ryo-modal-keys
 ("a" ryo-modal-mode :then '(forward-char))
 ("A" ryo-modal-mode :then '(move-end-of-line))

 ("h" backward-char)
 ("l" forward-char)
 ("j" next-line)
 ("k" previous-line)

 ("H" backward-word)
 ("L" forward-word)

 ("{" forward-paragraph)
 ("}" backward-paragraph)

 ("C-{" vz/m-forward-block)
 ("C-}" vz/m-backward-block)

 ("d" vz/m-kill-char)
 ("D" kill-sentence)

 ;; TODO Bind M-c and M-S-c to zzz commands
 ("c" vz/m-kill-char :then ryo-modal-mode)
 ("C" kill-sentence  :then ryo-modal-mode))

;; Prefix keys

(ryo-modal-keys
 (:no-repeat t)
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
  (setq-ns ctrlf
    mode-bindings '()
    minibuffer-bindings '())
  :ryo
  ("C-f"   ctrlf-forward-fuzzy)
  :config
  (defun vz/m-search--prompt (_ &rest args)
    "A function as an advice around `ctrlf--prompt' to
make a simplified prompt"
    (format "Search%s: "
            (if ctrlf--backward-p " Backward" "")))
  (advice-around 'ctrlf--prompt :around #'vz/m-search--prompt)
  (ctrlf-mode t)
  (setq ctrlf-minibuffer-bindings
        `(("C-j"      . ctrlf-next-match)
          (,(kbd "C-k")      . ctrlf-previous-match)
          (,(kbd "C-u")      . ctrlf-previous-page)
          (,(kbd "C-d")      . ctrlf-next-page)
          (,(kbd "<return>") . ctrlf-cancel)
          (,(kbd "<escape>") . ctrlf-cancel))))
