;; -*- lexical-binding: t; -*-

(defun vz/reload-config ()
  "Reload init.el"
  (interactive)
  (load user-init-file nil 'nomessage)
  (redraw-display)
  (force-mode-line-update t))

(defun vz/fread (path)
  "Read file and return the contents"
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-string)))

(defun vz/eval-file (path stdin args)
  "Evaluate elisp file in path"
  (eval (ignore-errors (read-from-whole-string (vz/fread path)))))

(defun vz/disable-bold-italic ()
  "Disable bold and italic for everything except bold and italic face"
  (-each (face-list)
    (fn:
     set-face-attribute <> nil
                         :weight 'normal
                         :slant 'normal))
  ;;(set-face-attribute 'bold   nil :weight 'bold)
  (set-face-attribute 'italic nil :slant  'italic))

(defun vz/windows-in-direction (direction &optional windows)
  "Get all windows in direction relative to selected window"
  (let ((win (window-in-direction direction
                                  (or (car windows) (selected-window)))))
    (if win
        (vz/windows-in-direction direction (cons win windows))
      windows)))

(use-package prescient
  :config
  (setq-ns prescient
    history-length 150
    ;filter-method '(literal regexp initialism fuzzy)
    save-file (~ ".cache/emacs-prescient.el"))
  (prescient-persist-mode))

(use-package ivy-prescient
  :after prescient
  :config
  (setq-ns ivy-prescient-enable
    sorting nil)
  (ivy-prescient-mode t))

(vz/use-package evil nil
  :init
  (setq-ns evil-want
    keybinding nil
    C-d-scroll t
    C-u-scroll t
    Y-yank-to-eol t))

(use-package flyspell
  :straight (:type built-in)
  :hook (org-mode . flyspell-mode)
  :defer t
  :init
  (setq-ns flyspell
    persistent-highlight t
    issue-message-flag nil)
  (setq-ns ispell
    program-name "hunspell"))

(vz/use-package org nil
  :straight (:type built-in)
  :defer t
  :general (:keymaps 'override :states 'normal
    "SPC oc" #'org-capture)
  :init
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode)
    :defer t
    :config
    (setq org-bullets-bullet-list '(" "))))

(vz/use-package circe "irc"
  :defer t
  :functions (vz/circe-jump-irc vz/circe-jump-discord)
  :general (:keymaps 'override :states 'normal :prefix "SPC i"
    "i" #'vz/circe-jump-irc
    "d" #'vz/circe-jump-discord)
  :init
  (defun pass (passwd)
    "Get password"
    (replace-regexp-in-string
     "\n$" ""
	   (shell-command-to-string (format "pass get %s" passwd))))

  (defun pass-irc (serv)
    `(lambda (_) (pass (format "irc/%s" ,serv))))

  (defun pass-discord (serv)
    `(lambda (_)
       (unless (or vz/ircdiscord-process (process-live-p vz/ircdiscord-process))
         (setq-default vz/ircdiscord-process
                       (start-process "ircdiscord" nil "ircdiscord")))
       (format "%s:%d" (pass "misc/discord") ,serv))))

(use-package comint
  :defer t
  :straight (:type built-in)
  :config
  (defun vz/comint-send-input (&optional start end)
    "Send region if present, otherwise current line to current buffer's process"
    (interactive "r")
    (if (region-active-p)
        (let ((cmd (buffer-substring (or start (region-beginning))
                                     (or end (region-end)))))
          (comint-send-string (get-buffer-process (current-buffer))
                              (concat cmd "\n"))
          (comint-add-to-input-history cmd))
      (comint-send-input))
    (when (evil-visual-state-p)
      (evil-exit-visual-state)))

  (general-nmap
    :keymaps 'comint-mode-map
    "<RET>" #'vz/comint-send-input
    "[w"    #'comint-write-output
    "[d"    #'comint-delete-output
    "[j"    #'comint-next-prompt
    "[k"    #'comint-previous-prompt
    "[c"    #'comint-clear-buffer)

  (general-imap
    :keymaps 'comint-mode-map
    "<S-return>" #'comint-accumulate)

  (general-vmap
    :keymaps 'comint-mode-map
    "<RET>" #'vz/comint-send-input))

(vz/use-package shell nil
  :defer t
  :straight (:type built-in)
  :init
  (defun vz/cd-selbuf (path)
    "Change working directory of selected buffer"
    (with-current-buffer
        (window-buffer (selected-window))
      (setq default-directory path))))

;; (vz/use-package wand "plumb"
;;   :straight (:type git :host github
;;              :repo "cmpitg/wand"))

(use-package show-paren
  :defer t
  :hook (prog-mode . show-paren-mode)
  :straight (:type built-in)
  :config
  (setq-ns show-paren
    delay 0
    when-point-inside-paren t))

;; It updates only when you balance the parenthesis
(use-package aggressive-indent
  :defer t)

(use-package company
  :defer t
  :hook (prog-mode . company-mode)
  :config
  (setq-ns company
   require-match nil
   idle-delay 0.2
   tooltip-limit 10
   minimum-prefix-length 2)
  (general-define-key
   :keymaps 'company-active-map
   "M-n" nil
   "M-p" nil
   "C-j" #'company-select-next
   "C-k" #'comapny-select-previous))

(use-package company-prescient
  :defer t
  :after company
  :hook (prog-mode . company-prescient-mode))

(use-package hl-todo
  :defer t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":")
  (general-nmap
    :prefix "["
    "j" #'hl-todo-next
    "k" #'hl-todo-previous))

(use-package ws-butler
  :config (ws-butler-global-mode))

(use-package edit-indirect
  :defer t
  :functions (vz/edit-indirect-paragraph)
  :general
  (:keymaps 'override :prefix "SPC" :states '(normal visual)
    "nr" #'edit-indirect-region
    "np" #'vz/edit-indirect-paragraph)
  (:keymaps 'edit-indirect-mode-map :states 'normal
    "q" #'edit-indirect-commit
    "Q" #'edit-indirect-abort)
  :config
  (setq edit-indirect-guess-mode-function
        (fn: funcall (with-current-buffer <> major-mode)))
  (defun vz/edit-indirect-paragraph ()
    (interactive)
    (mark-paragraph)
    (command-execute #'edit-indirect-region)))

;; TODO: Is there a cleaner way to do this other than adding a hook?
;; NOTE: They are buffer-local variables already.
(use-package go-mode
  :defer t
  :hook (before-save . gofmt-before-save)
  :config
  (add-hook 'go-mode-hook
            (fn: setq-local
                 vz/describe-function-func #'godef-describe
                 vz/goto-definition-func #'godef-jump)))

(use-package racket-mode
  :defer t
  :hook (racket-mode . racket-unicode-input-method-enable)
  :hook (racket-mode . aggressive-indent-mode)
  :config
  (add-hook 'racket-mode-hook
            (fn: setq-local
                 vz/describe-function-func #'racket-repl-describe
                 vz/goto-definition-func #'racket-repl-visit-definition))
  :general (:states 'normal :keymaps 'racket-mode-map
    "C-e" #'racket-send-last-sexp)
  :general (:states 'normal :prefix "SPC" :keymaps 'racket-mode-map
    "rsr" #'racket-send-region
    "rsd" #'racket-send-definition))

(use-package nix-mode
  :defer t
  :mode "\\.nix\\'")

(use-package python
  :defer t
  :straight (:type built-in)
  ;; :hook (python-mode . run-python)
  :general (:states 'normal :prefix "SPC" :keymaps 'python-mode-map
    "rsr" #'python-shell-send-region
    "rsd" #'python-shell-send-defun
    "rsf" #'python-shell-send-buffer
    "rsF" #'python-shell-send-file)
  :config
  (add-hook 'python-mode-hook
            (fn: setq-local
                 vz/describe-function-func #'python-describe-at-point))
  (setq-ns python-shell
    interpreter "python3"
    interpreter-args "-i"))

(use-package scheme
  :defer t
  :straight (:type built-in)
  :hook (scheme-mode . aggressive-indent-mode)
  :config
  (setq scheme-program-name "csi"))

;;(use-package emacs-lisp
;;  :defer t
;;  :straight (:type built-in)
;;  :hook (emacs-lisp-mode . aggressive-indent-mode))
