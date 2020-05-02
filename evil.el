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
                                  visual motion operator replace))

  (defmacro vz/bind (state keymap &rest body)
    `(general-define-key
      :states  ,state
      :keymaps ,keymap
      ,@body))

  (defmacro vz/bind-norm (&rest body)
    `(general-define-key
      :states  'normal
      :keymaps 'override
      ,@body))

  (defmacro vz/bind-novs (&rest body)
    `(general-define-key
      :states  '(normal visual)
      :keymaps 'override
      ,@body))

  (defmacro vz/bind-nois (&rest body)
    `(general-define-key
      :states  '(normal insert)
      :keymaps 'override
      ,@body))

  (defmacro vz/bind-vis (&rest body)
    `(general-define-key
      :states  'visual
      :keymaps 'override
      ,@body)))

;;(vz/bind-nois
;; "C-V" 'evil-paste-after)

(vz/bind-norm
 :prefix "SPC"
 "rc"  'vz/reload-config
 "st"  'vz/spawn-st
 "cd"  'counsel-find-file)

(vz/bind-novs
 "gc" 'comment-line)

(vz/bind-vis
 "ga" 'align-regexp)
