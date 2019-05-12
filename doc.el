;; document related stuff
;; TODO: add latex stuff

;; view pdfs
(use-package pdf-tools
  :ensure t
  :config
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode))))
