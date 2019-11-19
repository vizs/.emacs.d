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

(vz/bind-norm
 "g c"      'comment-line
 "C-w O"    'delete-other-windows
 "C-w q"    'evil-window-delete
 "SPC r c"  'vz/reload-config
 "SPC s t"  'vz/spawn-st
 "SPC c d"  'counsel-find-file)

(vz/bind-nois
 "C-w o"      nil
 "C-w c"      nil
 "C-w C-c"    nil
 "C-w o"     'ace-window)

(vz/bind-vis
 "g c" 'comment-line)
