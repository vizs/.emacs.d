;; -*- lexical-binding: t; -*-

;; TODO: Add advice around org-capture-abort and friends to auto-kill frame
;;       if launched via `org-capture' script

(use-package org-bullets
  :defer t)

(use-package valign
  :straight (:type git :host github :repo "casouri/valign")
  :config (valign-mode))

;; Enable structure template completion
(require 'org-tempo)

(tempo-define-template
 "org-latex-eq*"
 '("\\begin{equation*}" n "\\begin{split}"
   n r n "\\end{split}" n "\\end{equation*}" >)
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
  hide-emphasis-markers t
  hide-leading-stars nil ; indent mode also hides stars
  fontify-emphasized-text t
  fontify-quote-and-verse-blocks t
  fontify-whole-heading-line t
  src-fontify-natively nil

  ;; org-bullets
  bullets-bullet-list '(" ")

  ;; I prefer a human readable format over a machine readable one :P
  time-stamp-custom-formats '("<%A, %d %B, %Y>" . "<%A, %d %B, %Y %k:%M>")
  (display-custom-times . default) t

  preview-latex-default-process 'dvisvgm

  capture-templates
  `(("d" "Dump links, book names, whatever")
    ("da" "Anime/Manga to read" entry
     (file "dump.org")
     "* TODO %^{Name|}
:PROPERTIES:
:type: %^{Type|anime|manga}
:added: %U
:END:
%?
%:link" :prepend t :kill-buffer t)
    ("de" "Emacs link to go through" entry
     (file "dump.org")
     "* TODO %^{What is it?|}
:PROPERTIES:
:type: emacs
:added: %U
:END:
%?
%:link" :prepend t :kill-buffer t)
    ("dq" "Add a quote" entry
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
    ("dm" "Music Link" entry
     (file "dump.org")
     "* %^{Name|}
:PROPERTIES:
:type: music
:added: %U
:END:
%?
" :prepend t :kill-buffer t)
    ("do" "Others" entry
     (file "dump.org")
     "* TODO %^{Title|}
:PROPERTIES:
:type: %^{Type|article|book|misc}
:added: %U
:END:
%?
" :prepend t :kill-buffer t)

    ("n" "Notes")
    ("ne" "Emacs notes" entry
     (file+headline ,(~ "doc/notes.org") "Emacs")
     "* %?")
    ("nc" "Chemistry notes" entry
     (file+headline ,(~ "doc/school/g12/notes.org") "Chemistry")
     "* %?")
    ("ns" "CS notes" entry
     (file+headline ,(~ "doc/school/g12/notes.org") "CS")
     "* %?")
    ("np" "Physics notes" entry
     (file+headline ,(~ "doc/school/g12/notes.org") "Physics")
     "* %?")
    ("nm" "Maths notes" entry
     (file+headline ,(~ "doc/school/g12/notes.org") "Maths")
     "* %?")

    ("c" "Calendar")
    ("cs" "Schedule" entry
     (file+headline "calendar.org" "Schedules")
     "* TODO %?
SCHEDULED: %(call-interactively #'org-time-stamp)" :prepend t)
    ("cd" "Deadline" entry
     (file+headline "calendar.org" "Deadlines")
     "* TODO %?
DEADLINE: %(call-interactively #'org-time-stamp)" :prepend t)
    ("ca" "Appointment" entry
     (file+headline "calendar.org" "Appointments")
     "* TODO %?\n%(call-interactively #'org-time-stamp)" :prepend t))

  agenda-files `(,(~ "doc/org/calendar.org")))

(defun vz/org-mode-style ()
  (let ((faces '(org-table org-link org-code org-block org-drawer
                 org-date org-special-keyword org-verbatim org-tag
                 org-latex-and-related)))
    (vz/set-monospace-faces faces)
    (-each faces (fn: set-face-attribute <> nil :height 102)))
  (-each org-level-faces
    (fn: set-face-attribute <> nil :weight 'bold :slant 'normal))
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
  (org-bullets-mode t)
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

(defun vz/counsel-org-goto ()
  "Run `counsel-org-goto`, then beacon blink"
  (interactive)
  (counsel-org-goto)
  (vz/beacon-highlight))

(bind-keys
 :map org-mode-map
 ("C-c j" . vz/counsel-org-goto))
