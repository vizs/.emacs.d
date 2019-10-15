;; im a dirty vim user
;; TODO: change ex mode keybindings

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (setq evil-want-integration t)
  (evil-mode t)
  (vz/style-evil-cursor))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  (setq evil-want-keybinding t))

(use-package general
  :after evil
  :init
  (setq general-override-states '(insert emacs hybrid normal
                                  visual motion operator replace)))
(general-define-key
 :states    'normal
 :keymaps   'override
 "g c"      'comment-line
 "C-w O"    'delete-other-windows
 "SPC r c"  'vz/reload-config
 "SPC s t"  'vz/spawn-st)

(general-define-key
 :states    '(normal insert)
 :keymaps   'override
 "C-w o"     nil
 "C-w o"     'ace-window)

(general-define-key
 :states    'visual
 :keymaps   'override
 "g c"      'comment-line)
