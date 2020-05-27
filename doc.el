(defun vz/make-pdf ()
  "Compile org/tex document to pdf"
  (interactive)
  (pcase major-mode
   ('org-mode   (org-latex-export-to-pdf))
   ('latex-mode (call-process "pdflatex" nil 0 nil "buf"))))

(defun vz/preview-doc ()
  "Preview org or latex document. If it is an org-mode document, then compiles
it to pdf. If file i unsaved, it will not save it."
  (interactive)
  (let ((pdf (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))
	(unless (file-exists-p pdf) (vz/make-pdf))
	;; TODO: Consider using an emacs' plugin instead of zathura
	(call-process "zathura" nil 0 nil pdf)))

;; enable completion for blocks
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

%:link
:PROPERTIES:
:type: %^{Type|anime|emacs|article|music|manga|book|misc}
:added: %T
:END:" :prepend t :kill-buffer t)
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

(use-package org-bullets
  :config
  (setq org-bullets-bullet-list '(" "))
  (add-hook 'org-mode-hook #'org-bullets-mode))

(defun vz/org-mode-style ()
  (dolist (f '(org-table org-link org-code org-block org-drawer
               org-date org-special-keyword org-verbatim))
    (set-face-attribute f nil :inherit 'fixed-pitch))

  (dolist (f org-level-faces)
    (custom-set-faces
     `(,f ((t :weight bold)))))

  (let* ((height1 (+ 100 40))
         (height2 (- height1 20))
         (height3 (- height1 60)))
    (custom-set-faces
     `(org-level-1 ((t :height ,height1 :weight bold)))
     `(org-level-2 ((t :height ,height2 :weight bold)))
     `(org-quote   ((t :family "IBM Plex Serif" :slant italic)))
     `(org-block-begin-line ((t :height ,height3 :weight bold)))
     '(org-block-end-line ((t :inherit org-block-begin-line))))))

(defun vz/org-mode-init ()
  (org-indent-mode t)
  (org-num-mode t)
  (setq line-spacing 0.01)
  (setq buffer-face-mode-face '(:family "Charter" :height 100))
  (buffer-face-mode)
  (vz/org-mode-style))

(add-hook 'org-mode-hook #'vz/org-mode-init)

(general-nmap
  "SPC Oc" #'org-capture)

(general-nmap
  :keymaps 'org-mode-map
  :prefix "SPC"
  "j" #'counsel-org-goto-all
  "et" #'org-latex-export-to-latex
  "t" #'org-todo
  "sT" #'org-babel-tangle)

(general-nmap
  :prefix "SPC"
  :keymaps '(org-mode-map latex-mode-map)
  "pp" #'vz/preview-doc
  "ep" #'vz/make-pdf)
