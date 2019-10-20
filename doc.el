;; document related stuff

;; org-mode
(defun vz/org-preview-pdf ()
  (interactive)
  (call-process "zathura" nil 0 nil
                (replace-regexp-in-string "\.org" "\.pdf" (buffer-file-name))))

(setq org-src-preserve-indentation t)

(general-define-key
 :states      'normal
 :keymaps     'org-mode-map
 :prefix      "SPC"
 "e p"        'org-latex-export-to-pdf
 "e t"        'org-latex-export-to-latex
 "t"          'org-todo
 "s T"        'org-babel-tangle
 "p p"        'vz/org-preview-pdf)

(add-hook 'org-mode-hook 'display-line-numbers-mode)
