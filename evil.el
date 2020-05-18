;; This configuration tries to act more like vis

(use-package evil
  :init
  (setq-ns evil-want
   ;; minibuffer t
   keybinding nil
   C-d-scroll t
   C-u-scroll t
   Y-yank-to-eol t)
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
  ;; NOTE: Pass t if you want to shorten
  (general-evil-setup)
  (general-nmap
   "gc" #'comment-line
   "C-e" #'eval-last-sexp)
  (general-nmap
	  :prefix "SPC" "rc" #'vz/reload-config))

(use-package evil-numbers
  :after evil
  :config
  (general-nmap
   "C-a" #'evil-numbers/inc-at-pt
   "C-S-x" #'evil-numbers/dec-at-pt))

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
  ">" #'vz/evil-shift-right
  "<" #'vz/evil-shift-left)

(use-package avy
  :config
  (general-nmap
	"gf" #'avy-goto-char
	"gF" #'avy-goto-char-timer))

(use-package ace-window
  :after avy
  :init
  (setq aw-keys '(?a ?s ?d ?f ?h ?j ?k ?l)))

(defun vz/split-window-below-ask ()
  (interactive)
  (let ((buf (vz/get-file-or-buffer))
        (win (split-window-below)))
    (set-window-buffer win buf)
    (select-window win)))

(defun vz/split-window-right-ask ()
  (interactive)
  (let ((buf (vz/get-file-or-buffer))
        (win (split-window-right)))
    (set-window-buffer win buf)
    (select-window win)))

;; Behaves sort of like acme(1)
(defun vz/shrink-other-windows ()
  "Shrink all other windows except the selected window
in the same vertical column"
  (interactive)
  (window-resize (selected-window) (window-max-delta)))

(general-nmap
  :prefix "C-w"
  "o" #'ace-window
  "O" #'delete-other-windows
  "x" #'ace-delete-window
  "S" #'vz/split-window-below-ask
  "V" #'vz/split-window-right-ask
  "C-h" #'vz/shrink-other-windows
  "s" #'split-window-below
  "v" #'split-window-right)

(when (vz/load-pkg "sam")
  (defvar vz/sam-minor-mode-map (make-keymap) "vz/sam-minor-mode keymap")

  (define-minor-mode vz/sam-minor-mode
    "Minor mode for *sam-cmd* buffer"
    :init-value nil
    :keymap vz/sam-minor-mode-map)

  ;; Yoinked from sam-eval-command
  (defun vz/sam--eval-command ()
    (when (bufferp (get-buffer "*sam-cmd*"))
      (dolist (str (split-string (with-current-buffer "*sam-cmd*"
                                   (buffer-string)) "\n"))
        (let ((cmd (condition-case nil
                       (let ((case-fold-search nil))
                         (setq sam-command-in-progress
                               (concat sam-command-in-progress str))
                         (sam-compile-command sam-command-in-progress))
                         (error (setq sam-command-in-progress nil)
                                nil))))
          (if cmd
              (progn (sam-edit-mode)
                     (setq sam-command-in-progress nil)
                     (sam-eval-command cmd)
                     (and sam-please-go-away (progn (sam-leave-edit-mode))))
            (setq sam-command-in-progress
                  (concat sam-command-in-progress "\n")))))))
  
  (defun vz/sam-eval-command ()
    (interactive)
    (when (region-active-p)
        (sam-set-dot (region-beginning) (region-end)))
    (when (bufferp (get-buffer "*sam-cmd*"))
      (with-current-buffer "*sam-cmd*" (erase-buffer)))
    (let ((cmd-buf (get-buffer-create "*sam-cmd*"))
          (win (split-window-below)))
      (set-window-buffer win cmd-buf)
      (select-window win)
      (vz/sam-minor-mode 1)))

  (defun vz/sam-quit-win ()
    (interactive)
    (when (string= (buffer-name (current-buffer)) "*sam-cmd*")
      (window--delete (selected-window))
      (vz/sam--eval-command)))

  (general-nmap
    "C-S-e" #'vz/sam-eval-command)

  (general-nmap
    :keymaps 'vz/sam-minor-mode-map
    "Q" #'vz/sam-quit-win))

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
  (if (evil-mc-mode
         (evil-mc-define-vars)
         (evil-mc-initialize-vars)
         (evil-mc-initialize-hooks))
      (evil-mc-teardown-hooks)))
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

(defun vz/mc--visual-C-x ()
  (interactive)
  (evil-mc-skip-and-goto-next-match)
  (evil-visual-state))

(general-nmap
 :keymaps 'override
 "C-n" #'vz/mc--C-n
 "C-l" #'evil-mc-make-all-cursors
 "C-j" #'evil-mc-make-cursor-move-next-line
 "C-k" #'evil-mc-make-cursor-move-prev-line
 "<escape>" #'vz/mc--remove)

(general-vmap
 :keymaps 'override
 "C-n" #'vz/mc--C-n
 "C-x" #'vz/mc--visual-C-x)
