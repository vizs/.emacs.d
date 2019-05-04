;; document related stuff
;; TODO: add latex stuff

;; view pdfs
(use-package pdf-tools
  :ensure t
  :config
  (defun vz:pdf-hook ()
    (display-line-numbers-mode)
    (evil-normal-state))
  (add-hook 'pdf-view-mode-hook 'vz:pdf-hook)
  (add-hook 'special-mode-hook 'vz:pdf-hook))
