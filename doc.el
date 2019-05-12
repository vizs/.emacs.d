;; document related stuff
;; TODO: add latex stuff

;; view pdfs
(use-package pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode))))
