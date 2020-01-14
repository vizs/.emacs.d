(use-package edwina
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  (edwina-mode 1)
  (vz/bind-nois
   "C-w O"     'delete-other-windows
   "C-w o"     'ace-window
   "C-w r"     'edwina-arrange
   "C-w j"     'edwina-select-next-window
   "C-w k"     'edwina-select-next-window
   "C-w RET"   'edwina-zoom
   "C-w S-RET" 'edwina-clone-window
   "C-w q"     'edwina-delete-window))

(use-package avy
  :config
  (setq avy-background t)
  (vz/bind-novs
   "J" 'avy-goto-line-below
   "K" 'avy-goto-line-above))

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
