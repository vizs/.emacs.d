;; document related stuff

;; org-mode
(defun vz/org-preview-pdf ()
  (interactive)
  (call-process "zathura" nil 0 nil
                (replace-regexp-in-string "\.org" "\.pdf" (buffer-file-name))))

(setq org-src-preserve-indentation t)

(vz/bind
 'normal
 'org-mode-map
 "SPC e p"        'org-latex-export-to-pdf
 "SPC e t"        'org-latex-export-to-latex
 "SPC t"          'org-todo
 "SPC s T"        'org-babel-tangle
 "SPC p p"        'vz/org-preview-pdf)

(when vz/show-numbers?
  (add-hook 'org-mode-hook 'display-line-numbers-mode))
