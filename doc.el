(defun vz/make-pdf ()
  "Compile org/tex document to pdf"
  (interactive)
  (pcase major-mode
   ('org-mode (org-latex-export-to-pdf))
   ('latex-mode (call-process "pdflatex" nil 0 nil "buf"))))

(defun vz/preview-doc ()
  "Preview org or latex document. If it is an org-mode document, then compiles
it to pdf. If file i unsaved, it will not save it."
  (interactive)
  (let ((pdf (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))
	(unless (file-exists-p pdf) vz/make-pdf)
	;; TODO: Consider using an emacs' plugin instead of zathura
	(call-process "zathura" nil 0 nil pdf)))

(setq-ns org
 hide-emphasis-markers t
 fontify-emphasized-text t
 fontify-done-headline t
 fontify-quote-and-verse-blocks t
 fontify-whole-heading-line t
 src-fontify-natively t)

(use-package org-bullets
  :config
  (setq org-bullets-bullet-list '(" ")))

(dolist (f '(org-table org-link org-code org-block
             org-date org-special-keyword org-verbatim))
  (set-face-attribute f nil :inherit 'fixed-pitch))

(dolist (f org-level-faces)
  (custom-set-faces
   `(,f ((t :weight bold)))))

(let* ((height1 (+ (face-attribute 'variable-pitch :height) 40))
       (height2 (- height1 20)))
  (custom-set-faces
   `(org-level-1 ((t :height ,height1 :weight bold)))
   `(org-level-2 ((t :height ,height2 :weight bold)))
   '(org-quote ((t :slant italic)))
   '(org-block-begin-line ((t :weight bold)))
   '(org-block-end-line ((t :inherit org-block-begin-line)))))

(defun vz/org-mode-init ()
  (variable-pitch-mode t)
  (org-bullets-mode t)
  (org-indent-mode t)
  (setq line-spacing 0.01))

(add-hook 'org-mode-hook 'vz/org-mode-init)

(general-nmap
  :keymaps 'org-mode-map
  :prefix "SPC"
  "et" 'org-latex-export-to-latex
  "t" 'org-todo
  "sT" 'org-babel-tangle)
(general-nmap
  :prefix "SPC"
  :keymaps '(org-mode-map latex-mode-map)
  "pp" 'vz/preview-doc
  "ep" 'vz/make-pdf)
