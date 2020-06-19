;; -*- lexical-binding: t; -*-

;; * Set variables and sane defaults
;; ** Sanity

(setq-default
 ;; I especially like how much IBM Plex Mono stands out in org mode buffers
 vz/monospace-font "monospace"
 vz/variable-font "Charter"

 vz/ircdiscord-process nil
 vz/functional-major-modes '(nix-mode emacs-lisp-mode racket-mode scheme-mode)

 use-dialog-box nil

 backup-by-copying t
 backup-directory-alist '((".*" . "~/.cache/emacs-bkups/"))
 delete-old-versions t
 keep-new-versions 5
 keep-old-versions 2
 version-control t
 auto-save-file-name-transforms '((".*" "~/.cache/emacs-autosave/" t))
 auto-save-list-file-prefix "~/.cache/emacs-autosave/"
 create-lockfiles nil

 cursor-in-non-selected-windows nil
 custom-file "/dev/null"

 ;; Startup stuff
 initial-scratch-message nil
 inhibit-startup-screen t
 initial-buffer-choice t

 ;; Insert newline at EOF
 require-final-newline t

 ;; I prefer to separate default kill register and clipboard
 x-select-enable-clipboard nil

 ;; Follow links in version controlled
 vc-follow-symlinks t

 ;; Try to complete as well
 tab-always-indent 'complete

 ;; Indentation
 indent-tabs-mode t
 tab-width 4)

(defalias 'yes-or-no-p 'y-or-n-p)

;; ** Indentation

(defun vz/prog-functional-indent-style ()
  (when (member major-mode vz/functional-major-modes)
    (setq indent-tabs-mode nil
          tab-width 2)))

(defvar c-basic-offset 4)
(defvar cperl-basic-offset 4)
(defvar python-indent 4)

(add-hook 'prog-mode-hook #'vz/prog-functional-indent-style)

;; * Niceties
;; ** Automatically chmod +x file if it has a shebang

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; ** Auto-revert buffer if file is modified

(global-auto-revert-mode)

;; ** Fonts

(add-to-list 'default-frame-alist `(font . ,(format "%s:pixelsize=12"
                                             vz/monospace-font)))

;; ** Display > instead of $ at the visual end of truncated line

(set-display-table-slot standard-display-table 'truncation ?>)

;; ** Helper macros
;; *** setq but with namespace!

;; inspo: https://github.com/neeasade/emacs.d
(defmacro setq-ns (ns &rest args)
  "`setq' but with the ``namespace'' as NS.
If variable is a cons cell, then cdr is attached to setq.
For example, to set a buffer local variable, you pass the variable name
as (name-without-ns . local)."
  (declare (indent 1) (debug 0))
  (dolist (x (seq-partition args 2))
    (let ((set 'setq)
          (var (car x)))
      (when (listp (car x))
        (setq set (intern (format "setq-%s" (cdr (car x))))
              var (car (car x))))
      (eval `(,set ,(intern (format "%s-%s" ns var))
              ,(cadr x))))))

;; *** Formating s-expressions?

;; from u/b3n
(defmacro vz/format-sexp (sexp &rest format-args)
  "Format SEXP and eval it."
  `(eval (read (format ,(format "%S" sexp) ,@format-args))))

;; ** Custom theme

(add-to-list 'custom-theme-load-path
             (expand-file-name "lisp/themes" user-emacs-directory))

(load-theme 'vz t)

;; * Straight
;; ** Bootstrap straight

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

;; ** Setup straight variables

(straight-use-package 'use-package)
(setq-ns straight
  use-package-by-default t
  cache-autoloads t
  vc-git-default-clone-depth 1)

;; * Major helper functions
;; ** Emacs Lisp enhancers

;; dash | list
;; s    | string
;; f    | file
;; fn   | lambda
;; asoc | alist
(use-package dash)
(use-package s)
(use-package f)
(use-package fn
  :straight (:type git :host github
             :repo "troyp/fn.el"
             :fork (:host github :repo "vizs/fn.el"))
  :config
  ;; TODO: Check if first element is string,
  ;;       if so add it to interactive
  (defmacro fn! (&rest body)
    "Like `fn' but interactive"
    `(lambda ()
       (interactive)
       ,@body))
  (defmacro fn:! (&rest body)
    "Like `fn:' but interactive"
    `(lambda ()
       (interactive)
       (,@body))))
(use-package asoc
  :straight (:type git :host github :repo "troyp/asoc.el"))

;; ** use-packge but also load a file

(defmacro vz/use-package (name file &rest body)
  "Like `use-package' but also load file located lisp/hive/FILE.el."
  (declare (indent 2))
  `(use-package ,name
     ,@body
     :config
     (load-file (format "%s/lisp/hive/%s.el"
                 user-emacs-directory
                 (or ,file ',name)))))

;; ** Random helper functions

(defun ~ (file)
  "Path to FILE respective to $HOME"
  (expand-file-name file (getenv "HOME")))

(defun vz/random-choice (list)
  "Choose a random element from list."
  (nth (random (1- (length list))) list))

;; ** Font related functions

(defun vz/set-monospace-faces (faces)
  (-each faces
    (fn: set-face-attribute <> nil :family vz/monospace-font)))

(defun vz/set-variable-faces (faces)
  (-each faces
    (fn: set-face-attribute <> nil :family vz/variable-font)))

;; ** Quality of life packages
;; *** Easier binds

(use-package general
  :init
  (setq general-override-states '(insert emacs hybrid normal
                                  visual motion operator replace)))

;; *** Saner switch window and goto-char motion

(use-package avy)
(use-package ace-window
  :after avy
  :config
  ;; More noticable this way
  (set-face-attribute 'aw-leading-char-face nil :height 150)
  (setq-ns aw
    keys '(?a ?s ?d ?f ?h ?j ?k ?l)
    ;; Only consider the windows in the active frame
    scope 'frame))

;; *** Selection engine

(use-package ivy
  :after general
  :general
  (:keymaps 'ivy-minibuffer-map
            "C-p" nil
            "C-n" nil
            "<escape>" #'minibuffer-keyboard-quit
            "<C-up>"   #'ivy-minibuffer-grow
            "<C-down>" #'ivy-minibuffer-shrink
            "C-s"      #'ivy-avy
            "C-j"      #'ivy-next-line
            "C-k"      #'ivy-previous-line
            "C-u"      #'ivy-scroll-down-command
            "C-d"      #'ivy-scroll-up-command)
  (:keymaps 'ivy-switch-buffer-map
            "C-k" nil
            "C-k"   #'ivy-previous-line
            "C-M-K" #'ivy-switch-buffer-kill)
  :config
  (setq-ns ivy
    count-format "[%d/%d] "
    use-virtual-buffers t
    do-completion-in-region nil
    wrap t
    height 15)
  (require 'ivy-avy)

  ;; This was moved to ivy-hydra.el
  (defun ivy-minibuffer-grow ()
    "Grow the minibuffer window by 1 line."
    (interactive)
    (setq-local max-mini-window-height
                (cl-incf ivy-height)))

  (defun ivy-minibuffer-shrink ()
    "Shrink the minibuffer window by 1 line."
    (interactive)
    (when (> ivy-height 2)
      (setq-local max-mini-window-height
                  (cl-decf ivy-height))
      (window-resize nil -1)))

  (defun vz/get-file-or-buffer ()
    "Select a list of opened buffers, files in current directory and entries in
recentf and return the corresponding buffer. Create one if it doesn't exist"
    (unless (boundp 'recentf-list)
      (recentf-mode)
      (recentf-load-list))
    (-->
     (->>
      (append (-map #'buffer-name (buffer-list))
              (-filter #'f-file? (directory-files default-directory))
              recentf-list)
      (-uniq)
      (ivy-read "> "))
     (or (get-buffer it) (find-file-noselect it))))
  (ivy-mode 1))

(use-package counsel
  :after ivy
  :config
  (setq-ns counsel
    find-file-at-point t
    org-headline-display-todo t)
  (defun vz/counsel-M-x-prompt (ofun &rest args)
    "pls ")
  (advice-add 'counsel--M-x-prompt :around #'vz/counsel-M-x-prompt))

;; *** Blink cursor on certain actions

(use-package beacon
  :config
  (beacon-mode 1)
  (setq-ns beacon-blink-when
    window-scrolls t
    point-moves-horizontally nil
    point-moves-vertically nil)
  (defun vz/beacon-highlight ()
    "`beacon-blink' for one second to capture attention"
    (let ((beacon-blink-duration 1))
      (beacon-blink))))
