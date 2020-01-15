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
   :prefix "C-w"
   "o"   'ace-window
   "x"   'ace-delete-window
   "r"   'edwina-arrange
   "R"   'vz/edwina-default-mfact
   "j"   'edwina-select-next-window
   "k"   'edwina-select-previous-window
   "h"   'evil-window-left
   "l"   'evil-window-right
   "H"   'edwina-dec-mfact
   "L"   'edwina-inc-mfact
   "i"   'edwina-inc-nmaster
   "d"   'edwina-dec-nmaster
   "RET" 'edwina-zoom
   "s"   'edwina-clone-window
   "q"   'edwina-delete-window))

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
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
        ivy-posframe-border-width 0)
  (ivy-posframe-mode 1))
