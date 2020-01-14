(use-package edwina
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected)
        edwina-mfact                0.60)
  (defun vz/edwina-default-mfact ()
    (interactive)
    (setq edwina-mfact 0.60)
    (edwina-arrange))
  (edwina-mode 1)
  (vz/bind-nois
   "C-w o"   'ace-window
   "C-w x"   'ace-delete-window
   "C-w r"   'edwina-arrange
   "C-w R"   'vz/edwina-default-mfact
   "C-w j"   'edwina-select-next-window
   "C-w k"   'edwina-select-previous-window
   "C-w h"   'evil-window-left
   "C-w l"   'evil-window-right
   "C-w H"   'edwina-dec-mfact
   "C-w L"   'edwina-inc-mfact
   "C-w i"   'edwina-inc-nmaster
   "C-w d"   'edwina-dec-nmaster
   "C-w RET" 'edwina-zoom
   "C-w s"   'edwina-clone-window
   "C-w q"   'edwina-delete-window))

(use-package avy
  :config
  (setq avy-background t)
  (vz/bind-novs
   "J" 'avy-goto-line-below
   "K" 'avy-goto-line-above))

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; so i dont get lost
(use-package beacon
  :config
  (beacon-mode 1)
  (setq beacon-blink-when-window-scrolls           nil
        beacon-blink-when-point-moves-horizontally nil
        beacon-blink-when-point-moves-vertically   nil))

(use-package ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-point))
        ivy-posframe-border-width 0)
  (ivy-posframe-mode 1))
