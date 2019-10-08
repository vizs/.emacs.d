;; document related stuff

;; org-mode
(setq org-src-preserve-indentation t)

(general-define-key
 :states      'normal
 :keymaps     'org-mode-map
 :prefix      "SPC"
 "e p"        'org-latex-export-to-pdf
 "e t"        'org-latex-export-to-latex
 "t a"        'org-babel-tangle
 "t"          'org-todo)
