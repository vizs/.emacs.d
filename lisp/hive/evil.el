;; -*- lexical-binding: t; -*-

;; Don't leave visual mode
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

;; Avy functions
;; from u/loskutak-the-ptak
(defun vz/avy-search (&optional arg)
  (interactive "P")
  (let ((avy-timeout-seconds 120))
    (call-interactively #'avy-goto-char-timer arg)))

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

;; TODO: Look into removing evil-collection eventually
;;       Removing evil-collection makes general-override-map impossible to use?
(use-package evil-collection
 :after evil
 :config
 (setq evil-want-keybinding t)
 (evil-collection-init))

;; Setup general and keybinds
(general-evil-setup)
(general-imap
  "C-S-v" (fn:! evil-paste-after 1 ?+))
(general-nmap
  :keymaps 'override
  "gc"      #'comment-line
  "C-e"     #'eval-last-sexp
  "/"       #'swiper
  "?"       #'swiper-backward
  "SPC rc"  #'vz/reload-config
  "SPC pb"  #'previous-buffer
  "SPC nb"  #'next-buffer
  "SPC SPC" #'counsel-M-x
  "SPC df"  (fn:! command-execute vz/describe-function-func)
  "SPC d."  (fn:! command-execute vz/goto-definition-func)
  "SPC dv"  #'counsel-describe-variable
  "SPC dk"  #'counsel-descbinds
  "SPC dF"  #'counsel-describe-face
  "SPC ff"  #'counsel-find-file
  "SPC j"   #'counsel-imenu
  "SPC b"   #'ivy-switch-buffer
  "C-w C-h" #'vz/shrink-other-windows
  "C-w S"   #'vz/split-window-below-ask
  "C-w V"   #'vz/split-window-right-ask
  "C-w s"   #'split-window-below
  "C-w v"   #'split-window-right
  "C-w o"   #'ace-window
  "C-w O"   #'delete-other-windows
  "C-w x"   #'ace-delete-window
  "gj"      #'avy-goto-line-below
  "gk"      #'avy-goto-line-above
  "gf"      #'avy-goto-char
  "gF"      #'avy-goto-timer
  "g/"      #'vz/avy-search)
(general-vmap
  :keymaps 'override
  "TAB" #'indent-for-tab-command
  ">"   #'vz/evil-shift-right
  "<"   #'vz/evil-shift-left
  "gj"  #'avy-goto-line-below
  "gk"  #'avy-goto-line-above)

(use-package evil-numbers
  :after evil
  :config
  (general-imap
    "C-a" #'evil-numbers/inc-at-pt
    "C-x" #'evil-numbers/dec-at-pt))

(use-package link-hint
  :after avy
  :config
  (general-nmap
    :keymaps 'override
    :prefix "SPC"
    "lo" #'link-hint-open-link
    "lc" #'link-hint-copy-link))

(straight-use-package
 '(sam :type git :host github
       :repo "realwhz/sam.el"
       :fork (:host github :repo "vizs/sam.el")))
(use-package sam
  :after evil
  :config
  (defvar vz/sam-minor-mode-map (make-keymap))
  (define-minor-mode vz/sam-minor-mode
    "Minor mode for *sam-cmd* buffers"
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

;; Very ugly right now
(use-package evil-mc
  :after evil
  :config
  (define-minor-mode vz/evil-mc-mode
    "Toggle evil-mc-mode in a single buffer without any keybinds"
    :group 'evil-mc
    :init-value nil
    :lighter evil-mc-mode-line
    (if evil-mc-mode
         (evil-mc-define-vars)
         (evil-mc-initialize-vars)
         (evil-mc-initialize-hooks)
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
    "C-n"      #'vz/mc--C-n
    "C-l"      #'evil-mc-make-all-cursors
    "C-j"      #'evil-mc-make-cursor-move-next-line
    "C-k"      #'evil-mc-make-cursor-move-prev-line
    "<escape>" #'vz/mc--remove)

  (general-vmap
    :keymaps 'override
    "C-n" #'vz/mc--C-n
    "C-x" #'vz/mc--visual-C-x))

(evil-mode t)
