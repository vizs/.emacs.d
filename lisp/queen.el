;; -*- lexical-binding: t; -*-

;; * Package manager

;; Quelpa scares me so I'm using straight, use `use-package' to manage
;; configuration of packages.

;; ** Bootstrap straight

;; Straight from the readme
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name
                       "straight/repos/straight.el/bootstrap.el"
                       user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-vc-git-default-clone-depth 1 ; Be ASAP
      straight-cache-autoloads t)

;; ** Install use-package

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

;; ** Macro to load in a file when using `use-package'

(defmacro vz/use-package (name file &rest body)
  "Like `use-package' but also load file located lisp/hive/FILE.el."
  (declare (indent 2))
  `(use-package ,name
     ,@body
     :config
     (load-file (format "%s/lisp/hive/%s.el" user-emacs-directory (or ,file ',name)))))

;; * Helper functions


(require 'seq)
(require 'pcase)

;; I only really write racket so HAHA

;; ** filter-map

(defun vz/filter-map (pred list)
  "Like (seq-filter #'(lambda (x) x) (seq-map #'pred list)."
  (letrec ((helper #'(lambda (list res)
                       (if (null list)
                           res
                         (funcall helper (butlast list)
                          (if-let ((elt (funcall pred (car (last list)))))
                              (cons elt res)
                            res))))))
    (funcall helper list '())))

;; ** find-index

(defun vz/find-index (pred list)
  "Find first index for which PRED returned non-nil in
LIST. Return nil if otherwise."
  (letrec ((helper #'(lambda (n list)
                       (cond
                        ((null list) nil)
                        ((funcall pred (car list)) n)
                        (t (funcall helper (1+ n) (cdr list)))))))
    (funcall helper 0 list)))

;; ** Full path to files in $HOME

(defun ~ (file)
  "Path to FILE respective to $HOME."
  (expand-file-name file (getenv "HOME")))

;; ** Set face's font-family

(defun vz/set-monospace-faces (faces)
  (seq-each #'(lambda (x) (set-face-attribute x nil :family vz/monospace-font))
            faces))

(defun vz/set-variable-faces (faces)
  (seq-each #'(lambda (x) (set-face-attribute x nil :family vz/variable-font))
            faces))

;; ** External Elisp Enhancers

(use-package s)                         ; String
(use-package f)                         ; File
(use-package asoc                       ; Alist
  :straight ( :type git :host github :repo "troyp/asoc.el"))

;; * Customise core built-in packages
;; ** Disable custom files

(use-package cus-edit
  :straight ( :type built-in)
  :config
  (setq custom-file "/dev/null"))

;; ** Auto saves and backups

;; It is really annoying when Emacs scatters auto saves and backups in
;; random directories. But you can save them under a single directory.

(use-package files
  :straight ( :type built-in)
  :config
  (setq
   ;; Save auto save files under a single directory
   auto-save-file-name-transforms `((".*" ,(~ ".cache/emacs-autosave/") t))
   auto-save-list-file-prefix (~ ".cache/emacs-autosave/")

   ;; Pointless to send a message IMO, only clutters the *Messages*
   ;; buffer.
   auto-save-no-message t

   ;; Don't ask to delete old backup-files
   delete-old-versions t

   ;; I /don't/ really care about the ownership of backup files
   backup-by-copying t

   ;; Save backup files under a single directory
   backup-directory-alist `((".*" . ,(~ ".cache/emacs-bkups/")))))

;; ** Auto-revert mode

(use-package autorevert
  :straight ( :type built-in)
  :config
  (global-auto-revert-mode t)
  (setq
   ;; Kind of pointless to have this when I can just turn on `auto-revert-mode'
   create-lockfiles nil

   ;; Should hopefully be more efficient this way
   auto-revert-use-notify t
   auto-revert-avoid-polling t))

;; ** Auto ``chmod +x'' files

(use-package executable
  :straight ( :type built-in)
  :config
  (add-hook 'after-save-hook
            #'executable-make-buffer-file-executable-if-script-p))

;; ** Cursor

(setq-default
 cursor-in-non-selected-windows nil
 cursor-type '(bar . 2))

;; ** End files w/ newline

(use-package files
  :straight ( :type built-in)
  :config
  (setq require-final-newline t))

;; ** X clipboard things

(use-package select
  :straight ( :type built-in)
  :config
  (setq
   ;; Separate kill-ring and X clipboard
   x-select-enable-clipboard nil))

;; ** Indentation

(setq-default
 tab-always-indent 'complete
 indent-tabs-mode t
 tab-width 4

 c-basic-offset 4
 cperl-basic-offset 4
 python-indent 4)

(add-hook 'prog-mode-hook
          (defun vz/indent-with-spaces-hook ()
            (when (apply #'derived-mode-p
                         '(nix-mode emacs-lisp-mode racket-mode scheme-mode))
              (setq-local indent-tabs-mode nil))))

;; ** Horizontal scrolling

(use-package mwheel
  :straight ( :type built-in)
  :config
  (setq mouse-wheel-tilt-scroll t
        mouse-wheel-flip-direction t))

;; ** Use `view-mode' for read only files

(use-package files
  :straight ( :type built-in)
  :config
  (setq view-read-only t))

;; ** Uniquify buffer names

(use-package uniquify
  :straight ( :type built-in)
  :config
  (setq uniquify-buffer-name-style 'forward))

;; ** I don't want to shoot myself

(defalias 'yes-or-no-p 'y-or-n-p)

(setq
 use-dialog-box nil
 ;; Not resizing pixelwise irks me to no end.
 frame-resize-pixelwise t)


(setq
 ;; C L E A N
 initial-scratch-message nil

 inhibit-startup-screen t
 initial-buffer-choice t)

(use-package vc-hooks
  :straight ( :type built-in)
  :config
  (setq vc-follow-symlinks t))

(use-package files
  :straight ( :type built-in)
  :config
  ;; Unsafe IK /shrug
  (setq enable-local-eval t))

(use-package novice
  :straight ( :type built-in)
  :config
  (setq disabled-command-function nil))

;; ** Fonts

(setq
 vz/monospace-font (replace-regexp-in-string
                    "\n$" ""
                    (shell-command-to-string "fc-match -f %{family} monospace"))
 vz/variable-font (replace-regexp-in-string
                   "\n$" ""
                   (shell-command-to-string "fc-match -f %{family} serif")))

(add-to-list 'default-frame-alist
             `(font . ,(format "%s:pixelsize=12" vz/monospace-font)))

(set-face-attribute 'variable-pitch nil :family vz/variable-font)

(add-hook 'server-after-make-frame-hook
          (defun vz/set-emoji-range ()
            (set-fontset-font t 'symbol
                              "Noto Color Emoji" nil 'prepend)
            (remove-hook 'server-after-make-frame-hook
                         #'vz/set-emoji-range)))

;; ** Theme and minor styling

(add-to-list 'custom-theme-load-path
             (expand-file-name "lisp/themes" user-emacs-directory))

(load-theme 'vz-nh t)

;; Display … instead of $ at the visual end of truncated line
(set-display-table-slot standard-display-table 'truncation ?…)

;; Inner border
(add-to-list 'default-frame-alist '(internal-border-width . 4))

;; ** Highlight on long cursor movements

(require 'pulse)

(use-package pulse
  :straight ( :type built-in)
  :config
  (setq pulse-flag t))

;; Too lazy to change the function call elsewhere :P
(defun vz/beacon-highlight (&rest _)
  "Pulse the current line momentarily."
  (interactive)
  (unless (minibufferp)
    (let ((pulse-iterations 25)
          (pulse-delay 0.1))
      (pulse-momentary-highlight-one-line (point)))))

;; (dolist (hook '(window-state-change-hook ; window selection
;;                 window-configuration-change-hook ; window deletion
;;                 window-scroll-functions))
;;   (add-hook hook #'vz/beacon-highlight))

;; * Selection engine

(use-package ivy
  :functions vz/ivy-minibuffer-insert-at-point
  :bind ( :map ivy-minibuffer-map
               ("<C-up>" . ivy-minibuffer-grow)
               ("<C-down>" . ivy-minibuffer-shrink))
  :config
  (setq ivy-count-format " [%d/%d] "
        ivy-use-virtual-buffers t
        ivy-do-completion-in-region nil
        ivy-wrap t
        ivy-height 15)
  ;; This was moved to ivy-hydra.el
  (defun ivy-minibuffer-grow ()
    "Grow the minibuffer window by 1 line."
    (interactive)
    (setq-local max-mini-window-height
                (setq ivy-height (1+ ivy-height))))
  (defun ivy-minibuffer-shrink ()
    "Shrink the minibuffer window by 1 line."
    (interactive)
    (when (> ivy-height 2)
      (setq-local max-mini-window-height
                  (setq ivy-height (1- ivy-height)))
      (window-resize nil -1)))
  (ivy-mode 1))

(use-package ivy-avy
  :after ivy)

(use-package counsel
  :after ivy
  :demand t
  :bind ("C-c j" . counsel-imenu)
  :config
  (setq counsel-find-file-at-point t
        counsel-org-headline-display-todo t
        counsel-org-headline-display-tags t)
  (defun vz/counsel-M-x-prompt ()
    "pls ")
  (advice-add 'counsel--M-x-prompt :override #'vz/counsel-M-x-prompt)
  (seq-each #'(lambda (x) (define-key counsel-mode-map (vector 'remap x) nil))
            '(yank-pop describe-bindings))
  (counsel-mode t))

;; * -*-*-*-
;; Local Variables:
;; eval: (outline-minor-mode)
;; outline-regexp: ";; [*]+"
;; End:
