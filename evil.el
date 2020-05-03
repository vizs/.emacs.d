;; I'm a sinner
;; This evil configuration acts more like vis

(use-package evil
  :init
  (setq-default
   evil-want-C-d-scroll t
   evil-want-C-u-scroll t
   evil-want-Y-yank-to-eol t)
  :config
  (evil-mode t))

(use-package evil-collection
  :after evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-collection-init)
  (setq evil-want-keybinding t))

(use-package general
  :after evil
  :config
  (setq general-override-states '(insert emacs hybrid normal
										 visual motion operator replace))
  (general-define-key
   :states 'normal :keymaps 'override
   "gc" 'comment-line
   "C-e" 'eval-last-sexp)
  (general-define-key
   :states 'normal :keymaps 'override
   :prefix "SPC"
   "rc" 'vz/reload-config))

(use-package evil-numbers
  :after evil
  :config
  (general-define-key
   :states 'normal :keymaps 'override
   "C-a" 'evil-numbers/inc-at-pt
   "C-x" 'evil-numbers/dec-at-pt))

;; TODO: package sam.el (https://github.com/realwhz/sam.el)

;; Multiple cursor implementation akin to vis'
;; TODO: * C-p in visual and normal mode
;;       * C-u,C-d in visual mode

(use-package evil-mc
  :after evil)

(define-minor-mode vz/evil-mc-mode
  "Toggle evil-mc-mode in a single buffer without any keybinds"
  :group 'evil-mc
  :init-value nil
  :lighter evil-mc-mode-line
  (cond (evil-mc-mode
         (evil-mc-define-vars)
         (evil-mc-initialize-vars)
         (evil-mc-initialize-hooks))
        (:else
         (evil-mc-teardown-hooks))))
(put 'vz/evil-mc-mode 'permanent-local t)

(define-globalized-minor-mode vz/global-evil-mc-mode
  vz/evil-mc-mode evil-mc-initialize)

(vz/global-evil-mc-mode)

(defun vz/mc--C-n ()
  (interactive)
  (evil-mc-make-and-goto-next-match)
  (evil-visual-state))

(defun vz/mc--remove ()
  (interactive)
  (evil-force-normal-state)
  (when (evil-mc-has-cursors-p)
	(evil-mc-undo-all-cursors)))

(defun vz/mc--C-p ()
  (interactive)
  (when (evil-mc-has-cursors-p)
	(evil-mc-undo-cursor-at-pos (point))
	(evil-mc-goto-cursor (evil-mc-find-next-cursor) nil)))

(defun vz/mc--visual-C-x ()
  (interactive)
  (evil-mc-skip-and-goto-next-match)
  (evil-visual-state))

(general-define-key
 :states 'normal :keymaps 'override
 "C-n" 'vz/mc--C-n
 "C-p" 'vz/mc--C-p
 "C-l" 'evil-mc-make-all-cursors
 "C-j" 'evil-mc-make-cursor-move-next-line
 "C-k" 'evil-mc-make-cursor-move-prev-line
 "<escape>" 'vz/mc--remove)

(general-define-key
 :states 'visual :keymaps 'override
 "C-n" 'vz/mc--C-n
 "C-x" 'vz/mc--visual-C-x
 "C-p" 'vz/mc--C-p)
