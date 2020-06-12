;; -*- lexical-binding: t; -*-

(defun vz/reload-config ()
  "Reload init.el."
  (interactive)
  (load user-init-file nil 'nomessage)
  (redraw-display)
  (force-mode-line-update t))

(defun vz/disable-bold-italic ()
  "Disable bold and italic for everything except bold and italic face."
  (-each (face-list)
    (fn:
     set-face-attribute <> nil :weight 'normal :slant 'normal))
                               ;;:underline nil))
  (set-face-attribute 'bold   nil :weight 'bold)
  (set-face-attribute 'italic nil :slant 'italic :underline nil))

(defun vz/windows-in-direction (direction &optional windows)
  "Get all WINDOWS in DIRECTION relative to selected window."
  (-->
   (or (car windows) (selected-window))
   (window-in-direction direction it)
   (if it
       (vz/windows-in-direction direction (cons it windows))
     windows)))

(load-file (expand-file-name "lisp/hive/modeline.el" user-emacs-directory))
(load-file (expand-file-name "lisp/hive/scripting.el" user-emacs-directory))

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
    sorting t)
  (ivy-prescient-mode t))

(vz/use-package evil nil
  :init
  (setq-ns evil-want
    keybinding nil
    C-d-scroll t
    C-u-scroll t
    Y-yank-to-eol t))

(use-package fringe
  :straight (:type built-in)
  :config
  ;; Remove the ugly left and right curly arrow for continued lines
  (setq
   fringe-indicator-alist (asoc-put! fringe-indicator-alist
                                     'continuation
                                     '(nil nil)))
  (fringe-mode '(5 . 0)))

(use-package flyspell
  :straight (:type built-in)
  :hook (org-mode . flyspell-mode)
  :defer t
  :init
  (setq-ns flyspell
    persistent-highlight t
    issue-message-flag nil)
;;  mark-duplication-flag nil)
  (setq-ns ispell
    program-name "hunspell"))

(use-package flymake
  :straight (:type built-in)
  :defer t
  :hook (sh-mode . flymake-mode)
  :config
  (define-fringe-bitmap 'vz/fringe-left-arrow
    [#b11000000 #b01100000 #b00110000
     #b00011000 #b00110000 #b01100000
     #b11000000]
    nil nil 'center)
  (setq-ns flymake
   error-bitmap   '(vz/fringe-left-arrow error)
   warning-bitmap '(vz/fringe-left-arrow warning)
   note-bitmap    '(vz/fringe-left-arrow compilation-info)))

(use-package flymake-shellcheck
  :after flymake
  :defer t
  :commands flymake-shellcheck-load
  :hook (sh-mode . flymake-shellcheck-load))

;; I actually prefer to center /everything/
;; Does not work with vertical splits
(use-package perfect-margin
  :defer t
  :custom
  (perfect-margin-ignore-regexps '("^\\*term-[0-9]+\\*"))
  (perfect-margin-ignore-filters '())
  (perfect-margin-visible-width   90)
  :general (:states 'normal :keymaps 'override
    "SPC C" #'perfect-margin-mode))

(straight-use-package 'auctex)
(straight-use-package 'cdlatex)

(vz/use-package org nil
  :straight (:type built-in)
  :defer t
  :general (:keymaps 'override :states 'normal
    "SPC oc" #'org-capture
    "SPC oj" #'counsel-org-goto-all)
  :init
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode)
    :defer t
    :config
    (setq org-bullets-bullet-list '(" ")))
  (use-package valign
    :straight (:type git :host github :repo "casouri/valign")
    :config (valign-mode)))

(vz/use-package circe "irc"
  :defer t
  :functions (vz/circe-jump-irc vz/circe-jump-discord)
  :general (:keymaps 'override :states 'normal :prefix "SPC i"
    "i" #'vz/circe-jump-irc
    "d" #'vz/circe-jump-discord)
  :init
  (defun pass (passwd)
    "Get password"
    (s-replace-regexp "\n$" "" (shell-command-to-string
                                (format "pass get %s" passwd))))
  (defun pass-irc (serv)
    (fn: pass (format "irc/%s" serv)))
  (defun pass-discord (serv)
    (fn (unless (or vz/ircdiscord-process
                    (process-live-p vz/ircdiscord-process))
          (setq-default vz/ircdiscord-process
                        (start-process "ircdiscord" nil "ircdiscord")))
        (format "%s:%d" (pass "misc/discord") serv))))

(use-package comint
  :defer t
  :straight (:type built-in)
  :functions (vz/comint-send-input)
  :general
   (:keymaps 'comint-mode-map :states 'normal
    "<RET>" #'vz/comint-send-input
    "[w"    #'comint-write-output
    "[d"    #'comint-delete-output
    "[j"    #'comint-next-prompt
    "[k"    #'comint-previous-prompt
    "[c"    #'comint-clear-buffer)
  (:keymaps 'comint-mode-map :states 'insert
    "<S-return>" #'comint-accumulate)
  (:states 'visual :keymaps 'comint-mode-map
    "<RET>" #'vz/comint-send-input)
  :config
  (defun vz/comint-send-input (&optional start end)
    "Send region if present, otherwise current line to current buffer's process"
    (interactive "r")
    (if (use-region-p)
        (let ((cmd (buffer-substring (or start (region-beginning))
                                     (or end (region-end)))))
          (comint-send-string (get-buffer-process (current-buffer))
                              (concat cmd "\n"))
          (comint-add-to-input-history cmd))
      (comint-send-input))
    (when (evil-visual-state-p)
      (evil-exit-visual-state))))

(vz/use-package shell nil
  :defer t
  :straight (:type built-in))

(vz/use-package wand "plumb"
  :straight (:type git :host github
             :repo "cmpitg/wand"
             :fork (:repo "vizs/wand")))

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

;; TODO: Look into using company-quickhelp
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
   "C-j" (fn! (company-complete-common-or-cycle 1))
   "C-k" (fn! (company-complete-common-or-cycle -1))))

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

(use-package go-mode
  :defer t
  :hook
  (before-save . gofmt-before-save)
  (go-mode . flymake-mode)
  :general (:states 'normal :prefix "SPC" :keymaps 'go-mode-map
    "df" #'godef-describe
    "j" #'godef-jump)
  :config
  ;; Cleaned up flymake-go
  (defun vz/flymake-go ()
    (list "go" (list "fmt"
                     (flymake-proc-init-create-temp-buffer-copy
                      'flymake-proc-create-temp-inplace))))
  (add-hook 'go-mode-hook
            (fn (add-to-list 'flymake-proc-allowed-file-name-masks
                             '("\\.go\\'" vz/flymake-go)))
            nil t))

(use-package racket-mode
  :defer t
  :hook
  (racket-mode . racket-unicode-input-method-enable)
  (racket-mode . aggressive-indent-mode)
  (racket-mode . racket-xp-mode)
  :general (:states '(normal visual) :keymaps 'racket-mode-map
    "C-e"     #'racket-eval-last-sexp
    "SPC rsr" #'racket-send-region
    "SPC rsd" #'racket-send-definition
    "SPC rse" #'racket-eval-last-sexp
    "SPC df"  #'racket-xp-describe
    "SPC d."  #'racket-xp-visit-definition)
  :config
  (defun vz/racket--symbol-at-point-or-prompt (ofun &rest args)
    (-let (((force-prompt-p prompt completions) args))
      (message (prin1-to-string force-prompt-p))
      (let ((sap (thing-at-point 'symbol t)))
        (if (or force-prompt-p (null sap))
            (let ((s (if completions
                         (ivy-read prompt completions
                                   :initial-input sap
                                   :sort t)
                       (read-from-minibuffer prompt sap))))
              (if (s-blank? (racket--trim (substring-no-properties s)))
                  nil
                s))
          sap))))
  (advice-add 'racket--symbol-at-point-or-prompt
              :around #'vz/racket--symbol-at-point-or-prompt))

(use-package nix-mode
  :defer t
  :mode "\\.nix\\'")

;; TODO: Use nix-sandbox for flymake commands
(use-package nix-sandbox)

(use-package python
  :defer t
  :straight (:type built-in)
  ;; :hook (python-mode . run-python)
  :general (:states 'normal :prefix "SPC" :keymaps 'python-mode-map
    "rsr" #'python-shell-send-region
    "rsd" #'python-shell-send-defun
    "rsf" #'python-shell-send-buffer
    "rsF" #'python-shell-send-file
    "df"  #'python-describe-at-point)
  :config
  (setq-ns python-shell
    interpreter "python3"
    interpreter-args "-i"))

;; To Lua or not to Lua
;; TODO
;; (use-package fennel-mode)

(use-package scheme
  :defer t
  :straight (:type built-in)
  :hook (scheme-mode . aggressive-indent-mode)
  :config
  (setq scheme-program-name "csi"))
