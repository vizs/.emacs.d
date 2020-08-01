;; -*- lexical-binding: t; eval: (outshine-mode t); -*-

;; * Functions

(defun pass (passwd)
  "Get password using `pass'"
  (s-replace-regexp "\n$" "" (shell-command-to-string
                              (format "pass get %s" passwd))))

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
     set-face-attribute <> nil :weight 'normal))
  ;; :slant 'normal
  ;; :underline nil))
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

(defmacro vz/bind (&rest args)
  "This sort of works like `general` and `bind-keys`.
The arguments to function is given like in `general` and :map <>
behaviour is similar to that of in `bind-keys`."
  (let ((map 'global-map)
        (prefix ""))
    `(progn
       ,@(-map
          (fn (-let (((key fun) <>))
               (pcase key
                (:map    (setq map fun)    '())
                (:prefix (setq prefix fun) '())
                (_ (list 'define-key
                          map
                          (if (stringp key) (kbd (concat prefix " " key)) key)
                          fun)))))
           (-partition 2 args)))))

;; Functions used to communicate with emacsclient.
;; It's in a separate file because it depends on dynamic scoping
(load-file (expand-file-name "lisp/hive/scripting.el" user-emacs-directory))

;; * Builtin stuff that doesn't depend on anything else
;; ** Fringe

(use-package fringe
  :straight (:type built-in)
  :config
  ;; Remove the ugly left and right curly arrow for continued lines
  (setf (cdr (assoc 'continuation fringe-indicator-alist)) '(nil nil))
  (setf (cdr (assoc 'truncation   fringe-indicator-alist)) '(nil nil))
  (fringe-mode '(5 . 0)))

;; ** Highlighting parenthesis

(use-package show-paren
  :defer t
  :hook (prog-mode . show-paren-mode)
  :straight (:type built-in)
  :config
  (setq-ns show-paren
    delay 0
    when-point-inside-paren t))

;; ** Spell check

;; Enabled on major-mode basis
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

;; ** On the fly syntax checker

(use-package flymake
  :straight (:type built-in)
  :defer t
  :hook (sh-mode . flymake-mode)
  :config
  (define-fringe-bitmap 'vz/fringe-left-arrow
    [#b11000000
     #b01100000
     #b00110000
     #b00011000
     #b00110000
     #b01100000
     #b11000000]
    nil nil 'center)
  (setq-ns flymake
    error-bitmap   '(vz/fringe-left-arrow error)
    warning-bitmap '(vz/fringe-left-arrow warning)
    note-bitmap    '(vz/fringe-left-arrow compilation-info)))

;; * Modeline

(load-file (expand-file-name "lisp/hive/modeline.el"
			     user-emacs-directory))
;; * Shell

(use-package comint
  :defer t
  :straight (:type built-in)
  :functions (vz/comint-send-input)
  :bind
  (:map comint-mode-map
        ("<S-return>" . comint-accumulate)
	      ("<return>"   . vz/comint-send-input))
  :config
  (defun vz/comint-send-input ()
    "Send region if present, otherwise current line to current buffer's process"
    (interactive)
    (if (use-region-p)
        (let ((cmd (buffer-substring (region-beginning)
                                     (region-end))))
          (comint-send-string (get-buffer-process (current-buffer))
                              (concat cmd "\n"))
          (comint-add-to-input-history cmd))
      (let ((after (comint-after-pmark-p)))
        (save-excursion (comint-send-input))
        (when after
          (comint-next-prompt 1))))))

(vz/use-package shell nil
  :demand t
  :straight (:type built-in))

;; * External packages
;; ** pos-tip

;; Used by quite a lot of packages
;; Might as well install it explicitly
(use-package pos-tip
  :defer t)

;; ** Whitespace

(use-package ws-butler
  :config (ws-butler-global-mode))

;; ** Highlight keywords and jump between them

(use-package hl-todo
  :defer t
  :hook (prog-mode . hl-todo-mode)
  :bind
  (:map prog-mode-map
   ("C-c tn" . hl-todo-next)
   ("C-c tp" . hl-todo-previous))
  :config
  (setq hl-todo-highlight-punctuation ":"))

;; ** Center buffer

(use-package olivetti
  :defer t
  :hook ((Man-mode org-mode) . olivetti-mode)
  :bind ("C-c C" . olivetti-mode)
  :config
  (setq-ns olivetti
    body-width 80
    enable-visual-line-mode nil))

;; ** Folding text

;; outshine improves outline-minor-mode by providing
;; org-mode like features
;; I only really use very few features,
;; I still need to see what outshine offers
(use-package outshine
  :defer t
  :functions vz/outshine-jump
  :bind
  (:map outshine-mode-map
        ("C-c J" . vz/outshine-jump))
  :config
  (defun vz/outshine-jump ()
    (interactive)
    (setq-local counsel-outline-settings
                `(,major-mode
                  (:outline-regexp ,(outshine-calc-outline-regexp)
                   :outline-level ,(outshine-calc-outline-level))))
    (counsel-outline))
  (setq-ns outshine
    oldschool-elisp-outline-regexp-base "[*]\\{1,8\\}"
    startup-folded-p t
    imenu-show-headlines-p nil))

;; ** Edit a part of a buffer in separate window

(use-package edit-indirect
  :defer t
  :functions vz/edit-indirect-paragraph
  :config
  (setq edit-indirect-guess-mode-function
        (fn: funcall (with-current-buffer <> major-mode)))
  (defun vz/edit-indirect-paragraph ()
    (interactive)
    (mark-paragraph)
    (command-execute #'edit-indirect-region)))

;; ** Execute actions based on text

;; Desperately needs a rewrite
(vz/use-package wand "plumb"
  :straight (:type git :host github
                   :repo "cmpitg/wand"
                   :fork (:repo "vizs/wand")))

;; * Org-mode

;; You don't need any explanation
(vz/use-package org nil
  :straight (:type built-in)
  :defer t
  :bind
  (("C-c oc" . org-capture)
   ("C-c oa" . org-agenda)
   ("C-c oj" . counsel-org-goto-all))
  :init
  (straight-use-package 'auctex)
  (straight-use-package 'cdlatex))

;; * Communication

;; IRC and Discord using ircdiscord
(vz/use-package circe "irc"
  :defer t
  :functions (vz/circe-jump-irc vz/circe-jump-discord)
  :init
  (defun pass-irc (serv)
    (fn: pass (format "irc/%s" serv)))
  (defun pass-discord (serv)
    (fn (unless (or vz/ircdiscord-process
                    (process-live-p vz/ircdiscord-process))
          (setq-default vz/ircdiscord-process
                        (start-process "ircdiscord" nil "ircdiscord")))
        (format "%s:%d" (pass "misc/discord") serv))))

;; * Programming Languages

(use-package company
  :demand t
	:functions (company-complete-common-or-cycle
			        vz/company-previous-candidate
			        vz/company-next-candidate)
	:bind
  (:map company-active-map
        ("C-n" . vz/company-next-candidate)
        ("C-p" . vz/company-previous-candidate))
  :config
  (defun vz/company-previous-candidate ()
    (interactive)
    (company-complete-common-or-cycle -1))
  (defun vz/company-next-candidate ()
    (interactive)
    (company-complete-common-or-cycle 1))
  (setq-ns company
    require-match nil
    idle-delay 0.2
    tooltip-limit 10
    show-numbers 'left
    global-modes '(not shell-mode org-mode)
    minimum-prefix-length 2)
  (setq completion-in-region-function
        (fn (if company-mode
                (company-complete-common)
              (ivy-completion-in-region <1> <2> <3> <4>))))
  (add-to-list 'company-backends #'company-capf)
  (global-company-mode))

;; Useful for lisps
(use-package aggressive-indent
  :defer t)

;; ** Shellcheck

(use-package flymake-shellcheck
  :after flymake
  :defer t
  :commands flymake-shellcheck-load
  :hook (sh-mode . flymake-shellcheck-load))

;; ** Python

(use-package python
  :defer t
  :straight (:type built-in)
  :bind
  ("C-c df" . python-describe-at-point)
  :config
  (setq-ns python-shell
    interpreter "python3"
    interpreter-args "-i"))

;; ** Scheme

;; Depends on chicken
(use-package scheme
  :defer t
  :straight (:type built-in)
  ;; :hook (scheme-mode . aggressive-indent-mode)
  :config
  (setq scheme-program-name "csi"))

;; ** Emacs Lisp

;; Has a separate file
(vz/use-package elisp-mode "elisp"
  :straight (:type built-in)
  :functions (vz/emacs-lisp-indent-function))

;; ** Go

(use-package go-mode
  :defer t
  :hook
  (before-save . gofmt-before-save)
  :config
  ;; Cleaned up flymake-go
  (defun vz/flymake-go ()
    (list "go" (list "fmt"
                     (flymake-proc-init-create-temp-buffer-copy
                      'flymake-proc-create-temp-inplace)))))
  ;;(add-hook 'go-mode-hook
  ;;          (defun vz/go-mode-init ()
  ;;            (flymake-mode)
  ;;            (add-to-list 'flymake-proc-allowed-file-name-masks
  ;;                         '("\\.go\\'" vz/flymake-go)))
  ;;          nil t))

;; ** Racket

(use-package racket-mode
  :defer t
  :hook
  (racket-mode . racket-unicode-input-method-enable)
  ;; (racket-mode . aggressive-indent-mode)
  (racket-mode . racket-xp-mode)
  :config
  (setq racket-show-functions '(racket-show-pos-tip))
  (add-hook 'racket-xp-mode-hook
            (defun vz/racket-xp-mode-init ()
              (remove-hook 'pre-display-functions
                           #'racket-xp-pre-redisplay t)
              (setq-local eldoc-documentation-function #'racket-xp-eldoc-function)))
  ;; For that sweet ivy-prescient sorting
  (defun vz/racket--symbol-at-point-or-prompt (_ &rest args)
    (let* ((s (ivy-read (cadr args) (caddr args)
                        :preselect (thing-at-point 'symbol)
                        :sort t)))
      (if (s-blank? (racket--trim (substring-no-properties s)))
          nil
        s)))
  (advice-add 'racket--symbol-at-point-or-prompt
              :around #'vz/racket--symbol-at-point-or-prompt))

;; ** Nix

(use-package nix-mode
  :defer t
  :mode "\\.nix\\'")

;; TODO: Use nix-sandbox for flymake commands
;; (use-package nix-sandbox)

;; ** Fennel
;; To Lua or not to Lua
;; TODO
(use-package fennel-mode)

;; * Sorting entries

;; Save and sort entries in ivy and company
(use-package prescient
  :config
  (setq-ns prescient
    history-length 150
    ;;  filter-method '(literal regexp initialism fuzzy)
    save-file (~ ".cache/emacs-prescient.el"))
  (prescient-persist-mode))

(use-package ivy-prescient
  :after prescient
  :config
  (setq-ns ivy-prescient-enable
    sorting t)
  (ivy-prescient-mode t))

(use-package company-prescient
  :after company
  :hook (prog-mode . company-prescient-mode))

;; * Fun
;; ** Dad Jokes

(use-package dad-joke)

;; * Improve(?) editing experience

(load-file (expand-file-name "lisp/hive/editing.el" user-emacs-directory))

;; * Interface
;; ** Transmission

(use-package transmission
  :demand t
  :bind ("C-c T" . transmission)
  :config
  (setq-ns transmission
    time-format "%A, %d %B, %Y %k:%M"
    units 'iec))
