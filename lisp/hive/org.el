;; -*- lexical-binding: t; -*-

;; Enable structure template completion
(require 'org-tempo)

;; TODO: setup org-protocol

(setq-ns org
 add-colon-after-tag-completion t
 default-notes-file (expand-file-name "~/doc/org/notes.org")
 directory (expand-file-name "~/doc/org")
 ; file-apps
         
 ;; indentation
 indent-indentation-per-level 1
 indent-mode-turns-on-hiding-stars t
 src-preserve-indentation t
 ; indent-mode-turns-off-org-adapt-indentation nil

 ;; style
 hide-emphasis-markers nil
 ; hide-leading-stars t ; indent mode also hides stars
 fontify-emphasized-text t
 fontify-done-headline t
 fontify-quote-and-verse-blocks t
 fontify-whole-heading-line t
 src-fontify-natively nil

 capture-templates
 `(("d" "Dump links and book names or whatever" entry
    (file "dump.org")
    "* TODO %?
:PROPERTIES:
:type: %^{Type|anime|emacs|article|music|manga|book|misc}
:added: %T
:END:
%:link" :prepend t :kill-buffer t)
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
    "* %?")))

(defun vz/org-mode-style ()
  (vz/set-monospace-faces '(org-table org-link org-code org-block org-drawer
                            org-date org-special-keyword org-verbatim))
  (-each org-level-faces
    (fn: set-face-attribute <> nil :weight 'bold))

  (let* ((height1 (+ 100 40))
         (height2 (- height1 20))
         (height3 (- height1 60)))
    (set-face-attribute 'org-level-1 nil :height height1)
    (set-face-attribute 'org-level-2 nil :height height2)
    (set-face-attribute 'org-quote nil
                        :family "IBM Plex Serif" :slant 'italic)
    (set-face-attribute 'org-block-begin-line nil
                        :height height3 :weight 'bold)
    (set-face-attribute 'org-block-end-line nil
                        :inherit 'org-block-begin-line)))

(defun vz/org-mode-init ()
  (org-indent-mode t)
  (org-num-mode t)
  (setq line-spacing 0.01
        buffer-face-mode-face `(:family ,vz/variable-font :height 100))
  (setq-local vz/jump-func #'counsel-org-goto)
  (buffer-face-mode)
  (vz/org-mode-style))

(add-hook 'org-mode-hook #'vz/org-mode-init)

(general-nmap
  :keymaps 'org-mode-map
  :prefix "SPC"
  "oel" #'org-latex-export-to-latex
  "of"  #'org-sparse-tree
  "t"   #'org-todo
  "ost" #'org-babel-tangle)
