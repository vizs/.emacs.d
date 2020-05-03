(defun vz/preview-doc ()
  "Preview org or latex document. If it is an org-mode document, then compiles
it to pdf. If file is unsaved, it will not save it."
  (interactive)
  (let ((pdf (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))
	(unless (file-exists-p pdf)
	  (cond
	   ((equal major-mode 'org-mode) (org-latex-export-to-pdf))
	   ((equal major-mode 'latex-mode) (call-process "pdflatex" nil 0 nil
													 "buf"))))
	(call-process zathura nil 0 nil pdf)))

;; Set it if you want to literate python
;; (setq org-src-preserve-indentation t)
(general-define-key
 :states 'normal :keymaps 'org-mode-map
 :prefix "SPC"
 "et" 'org-latex-export-to-latex
 "t" 'org-todo
 "sT" 'org-babel-tangle)

(general-define-key
 :states 'normal :keymaps '(org-mode-mapp latex-mode-map)
 "SPC pp" 'vz/preview-doc)
