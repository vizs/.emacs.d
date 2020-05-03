(defun vz/make-pdf ()
  "Compile org/tex document to pdf"
  (interactive)
  (cond
   ((equal major-mode 'org-mode) (org-latex-export-to-pdf))
   ((equal major-mode 'latex-mode) (call-process "pdflatex" nil 0 nil
												 "buf"))))

(defun vz/preview-doc ()
  "Preview org or latex document. If it is an org-mode document, then compiles
it to pdf. If file is unsaved, it will not save it."
  (interactive)
  (let ((pdf (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))
	(unless (file-exists-p pdf) vz/make-pdf)
	;; TODO: Consider using an emacs' plugin instead of zathura
	(call-process "zathura" nil 0 nil pdf)))

;; Set it if you want to literate python
;; (setq org-src-preserve-indentation t)
(general-nmap
 :keymaps 'org-mode-map
 :prefix "SPC"
 "et" 'org-latex-export-to-latex
 "t" 'org-todo
 "sT" 'org-babel-tangle
 :keymaps '(org-mode-map latex-mode-map)
 "pp" 'vz/preview-doc
 "ep" 'vz/make-pdf)
