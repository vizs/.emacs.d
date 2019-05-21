;; im a dirty vim user
(use-package evil
  :config
  (setq evil-want-integration t
        evil-want-keybinding nil)
  (evil-mode t)
  (vz:style-evil-cursor))

(use-package general
  :after evil
  :init
  (setq general-override-states '(insert emacs hybrid normal
                                  visual motion operator replace)))
