;; document related stuff

(defun vz/preview-pdf ()
  (interactive)
  (let ((buf (buffer-file-name))
        (replace (lambda (suffix str)
                   (replace-regexp-in-string suffix "\.pdf" str))))
    (call-process "zathura" nil 0 nil
                  (if (s-suffix? ".tex" buf)
                      (funcall replace "\.tex" buf)
                    (funcall replace "\.org" buf)))))

(defun vz/export-to-pdf ()
  (interactive)
  (let ((buf (buffer-file-name)))
    (cond
     ((s-suffix? ".tex" buf) (call-process "pdflatex" nil 0 nil buf))
     ((s-suffix? ".org" buf) (org-latex-export-to-pdf)))))

(setq org-src-preserve-indentation t)

(vz/bind
 'normal
 'org-mode-map
 "SPC e t"        'org-latex-export-to-latex
 "SPC t"          'org-todo
 "SPC s T"        'org-babel-tangle)

(when vz/show-numbers?
  (add-hook 'org-mode-hook 'display-line-numbers-mode))

(vz/bind
 'normal
 '(latex-mode-map org-mode-map)
 "SPC e p"        'vz/export-to-pdf
 "SPC p p"        'vz/preview-pdf)
