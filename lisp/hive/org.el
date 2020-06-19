;; -*- lexical-binding: t; -*-

;; TODO: Add advice around org-capture-abort and friends to auto-kill frame
;;       if launched via `org-capture' script

;; Enable structure template completion
(require 'org-tempo)

(tempo-define-template
 "org-latex-eq*"
 '("#+begin_export latex" n "\\begin{equation*}" n "\\begin{split}"
   n r n "\\end{split}" n "\\end{equation*}" n "#+end_export" >)
 "<eq")

(add-to-list 'org-tempo-tags '("<eq" . tempo-template-org-latex-eq*))

;; Increase the size of LaTeX previews
(plist-put org-format-latex-options :scale 1.25)

(setq-default cdlatex-auto-help-delay 0.75)

;; TODO: setup org-protocol

(setq-ns org
  add-colon-after-tag-completion t
  default-notes-file (~ "doc/org/notes.org")
  directory (~ "doc/org")
  preview-latex-image-directory (~ ".cache/org-ltximg/")
  ;; file-apps

  ;; indentation
  indent-indentation-per-level 1
  indent-mode-turns-on-hiding-stars t
  src-preserve-indentation t
  ;; indent-mode-turns-off-org-adapt-indentation nil

  ;; style
  highlight-latex-and-related '(latex)
  hidden-keywords '(title author)
  hide-emphasis-markers t
  hide-leading-stars nil ; indent mode also hides stars
  fontify-emphasized-text t
  fontify-done-headline t
  fontify-quote-and-verse-blocks t
  fontify-whole-heading-line t
  src-fontify-natively nil

  ;; I prefer a human readable format over a machine readable one :P
  time-stamp-custom-formats '("<%A, %d %B, %Y>" . "<%A, %d %B, %Y %k:%M>")
  (display-custom-times . default) t ;; Buffer local variable

  preview-latex-default-process 'dvisvgm

  capture-templates
  `(("d" "Dump links and book names or whatever" entry
     (file "dump.org")
     "* TODO %?
:PROPERTIES:
:type: %^{Type|anime|emacs|article|music|manga|book|misc}
:added: %U
:END:
%:link" :prepend t :kill-buffer t)
    ("q" "Quote" entry
     (file "dump.org")
     "* %^{quote|Quote %U|}
:PROPERTIES:
:type: quote
:added: %U
:END:
#+begin_quote
%?

-- %^{Author}
#+end_quote
" :prepend t :kill-buffer t)
    ("e" "Emacs notes" entry
     (file+headline ,(~ "doc/notes.org") "Emacs")
     "* %?")
    ("c" "Chemistry notes" entry
     (file+headline ,(~ "doc/school/g12/notes.org") "Chemistry")
     "* %?")
    ("s" "CS notes" entry
     (file+headline ,(~ "doc/school/g12/notes.org") "CS")
     "* %?")
    ("p" "Physics notes" entry
     (file+headline ,(~ "doc/school/g12/notes.org") "Physics")
     "* %?")
    ("m" "Maths notes" entry
     (file+headline ,(~ "doc/school/g12/notes.org") "Maths")
     "* %?"))

  agenda-files `(,(~ "doc/org/calendar.org")))

(defun vz/org-mode-style ()
  (let ((faces '(org-table org-link org-code org-block org-drawer
                 org-date org-special-keyword org-verbatim
                 org-latex-and-related)))
    (vz/set-monospace-faces faces)
    (-each faces (fn: set-face-attribute <> nil :height 102)))
  (-each org-level-faces
    (fn: set-face-attribute <> nil :weight 'bold))
  (let* ((height1 (+ 120 40))
         (height0 (+ height1 20))
         (height2 (- height1 20))
         (height3 (- height1 60)))
    (set-face-attribute 'org-level-1 nil :height height1)
    (set-face-attribute 'org-level-2 nil :height height2)
    (set-face-attribute 'org-document-title nil :height height0 :weight 'bold)
    (set-face-attribute 'org-quote nil
                        :family vz/variable-font
                        :slant 'italic)
    (set-face-attribute 'org-block-begin-line nil
                        :height height3 :weight 'bold)
    (set-face-attribute 'org-block-end-line nil
                        :inherit 'org-block-begin-line)))

(defun vz/org-mode-init ()
  (org-indent-mode t)
  (org-num-mode t)
  (electric-pair-local-mode t)
  (org-toggle-pretty-entities)
  (setq line-spacing 0.01
        buffer-face-mode-face `(:family ,vz/variable-font :height 120))
  (setq-local
   electric-pair-inhibit-predicate
   `(lambda (c) (if (char-equal c ?<) t
                  (,electric-pair-inhibit-predicate c))))
  (buffer-face-mode)
  (vz/org-mode-style))

(add-hook 'org-mode-hook #'vz/org-mode-init)

(general-nmap
  :keymaps 'org-mode-map
  :prefix "SPC"
  "oel" #'org-latex-export-to-latex
  "oil" #'org-insert-link
  "olp" #'org-latex-preview
  "olc" #'org-cdlatex-mode
  "of"  #'org-sparse-tree
  "ot"  #'org-todo
  "ost" #'org-babel-tangle
  "j"   (fn! (counsel-org-goto)
             (vz/beacon-highlight)))
