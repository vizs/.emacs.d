;; This configuration tries to act more like vis

(use-package evil
  :init
  (setq-default
   ;; evil-want-minibuffer t
   evil-want-keybinding nil
   evil-want-C-d-scroll t
   evil-want-C-u-scroll t
   evil-want-Y-yank-to-eol t)
  :config
  (evil-mode t))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  (setq evil-want-keybinding t))

(use-package general
  :after evil
  :config
  (setq general-override-states '(insert emacs hybrid normal
										 visual motion operator replace))
  ;; NOTE: Add t if you want to shorten
  (general-evil-setup)
  (general-nmap
   "gc" 'comment-line
   "C-e" 'eval-last-sexp
   :prefix "SPC"
   "rc" 'vz/reload-config))

(use-package evil-numbers
  :after evil
  :config
  (general-nmap
   "C-a" 'evil-numbers/inc-at-pt
   "C-x" 'evil-numbers/dec-at-pt))

;; The bitmaps don't look good
(use-package evil-fringe-mark
  :config
  (setq-default
   evil-fringe-mark-side 'left-fringe
   evil-fringe-mark-show-special nil))

;; Doesn't leave visual mode
(defun vz/evil-shift-left ()
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun vz/evil-shift-right ()
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(general-vmap
  ">" 'vz/evil-shift-right
  "<" 'vz/evil-shift-left)

(use-package avy
  :config
  (general-nmap
	"C-f" 'avy-goto-char
	"C-S-f" 'avy-goto-char-timer))

;; TODO: * package sam.el (https://github.com/realwhz/sam.el)
;;       * make 0x0 an ex command

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

;; BROKEN
(defun vz/mc--C-p ()
  (interactive)
  (when (evil-mc-has-cursors-p)
	(evil-mc-undo-cursor-at-pos (point))
	(evil-mc-goto-cursor (evil-mc-find-next-cursor) nil)))

(defun vz/mc--visual-C-x ()
  (interactive)
  (evil-mc-skip-and-goto-next-match)
  (evil-visual-state))

(general-nmap
 :keymaps 'override
 "C-n" 'vz/mc--C-n
 "C-p" 'vz/mc--C-p
 "C-l" 'evil-mc-make-all-cursors
 "C-j" 'evil-mc-make-cursor-move-next-line
 "C-k" 'evil-mc-make-cursor-move-prev-line
 "<escape>" 'vz/mc--remove)

(general-vmap
 :keymaps 'override
 "C-n" 'vz/mc--C-n
 "C-x" 'vz/mc--visual-C-x
 "C-p" 'vz/mc--C-p)
