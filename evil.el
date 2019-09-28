;; im a dirty vim user
(use-package evil
  :config
  (setq evil-want-integration t
        evil-want-keybinding  t)
  (evil-mode t)
  (vz:style-evil-cursor))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :after evil
  :init
  (setq general-override-states '(insert emacs hybrid normal
                                  visual motion operator replace)))
(general-define-key
 :states    'normal
 :keymaps   'override
 "C-w o"     nil
 "g c"      'comment-line
 "C-w o"    'ace-window
 "C-w O"    'delete-other-windows)

(general-define-key
 :states    'visual
 :keymaps   'override
 "g c"      'comment-line)
