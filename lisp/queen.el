;; -*- lexical-binding: t; -*-

(setq vz/monospace-font "Verily Serif Mono"
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
      auto-save-file-transforms '((".*" "~/.cache/emacs-autosave/" t))
      auto-save-list-file-prefix "~/.cache/emacs-autosave/"
      create-lockfiles nil
      cursor-in-non-selected-windows nil
      custom-file "/dev/null"
      gc-cons-threshold 16777216 ;; 16M
      x-select-enable-clipboard nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; inspo: https://github.com/neeasade/emacs.d
(defmacro setq-ns (ns &rest args)
  "Set variables with pre as their `namespace'"
  (dolist (x (seq-partition args 2))
    (eval `(setq ,(intern (format "%s-%s" ns (car x)))
                 ,(cadr x)))))

(defun ~ (file)
  (expand-file-name file (getenv "HOME")))

(defun vz/random-choice (list)
  (nth (random (1- (length list))) list))

(defun vz/set-monospace-faces (faces)
  (dolist (face faces)
    (set-face-attribute face nil :family vz/monospace-font)))

(defun vz/set-variable-faces (faces)
  (dolist (face faces)
    (set-face-attribute face nil :family vz/variable-font)))

(defun vz/prog-functional-indent-style ()
  (when (member major-mode vz/functional-major-modes)
    (setq indent-tabs-mode nil
          tab-width 2)))

;; Bootstrap straight
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

(straight-use-package 'use-package)
(setq-ns straight
  use-package-by-default t
  cache-autoloads t)

(defmacro vz/use-package (name file &rest body)
  `(use-package ,name
     ,@body
     :config
     (load-file (format "%s/lisp/hive/%s.el"
                        user-emacs-directory
                        (or ,file ',name)))))

;; Indentation
(setq-default indent-tabs-mode t
              tab-width 4)
(defvar c-basic-offset 4)
(defvar cperl-basic-offset 4)
(defvar python-indent 4)

(add-hook 'prog-mode-hook #'vz/prog-functional-indent-style)

;; Line numbers and column indicator
(setq-ns display-line
  numbers-type 'relative
  numbers-width 0
  current-absolute t)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'view-mode-hook #'(lambda () (display-line-numbers-mode 0)))

(when (>= emacs-major-version 27)
  (setq-ns display-fill-column-indicator
    column 80
    char "|")
  (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'view-mode-hook #'(lambda () (display-fill-column-indicator-mode 0))))

;; Modeline
(load-file (expand-file-name "lisp/hive/modeline.el" user-emacs-directory))

;; Fonts
(add-to-list 'default-frame-alist `(font . ,(format "%s:pixelsize=12"
                                                    vz/monospace-font)))

;; Fringe width
(fringe-mode '(2 . 2))

;; Quality of life improvements
(use-package general)
(use-package avy)
(use-package ace-window
  :after avy
  :config
  (setq aw-keys '(?a ?s ?d ?f ?h ?j ?k ?l)))
(use-package ivy
  :after general
  :config
  ;; This was moved to ivy-hydra.el
  (unless (fboundp 'ivy-minibuffer-grow)
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
        (window-resize nil -1))))

  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1)

  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "C-p" nil
   "C-n" nil
   "<C-up>"   #'ivy-minibuffer-grow
   "<C-down>" #'ivy-minibuffer-shrink
   "C-s"      #'ivy-avy
   "C-j"      #'ivy-next-line
   "C-k"      #'ivy-previous-line
   "C-p"      #'ivy-minibuffer-grow
   "C-n"      #'ivy-minibuffer-shrink
   "C-u"      #'ivy-scroll-down-command
   "C-d"      #'ivy-scroll-up-command)

  (general-define-key
   :keymaps 'ivy-switch-buffer-map
   "C-k" #'ivy-previous-line
   ;; Shift cannot be bound???
   "C-M-K" #'ivy-switch-buffer-kill)

  (defun vz/get-file-or-buffer ()
    "Return buffer corresponding to buffer-name or file-name
Create file-buffer if it such no buffer/file exists"
    (let ((buf-name (ivy-read "> "
                              (append
                               (seq-filter #'file-regular-p
                                           (directory-files default-directory))
                               (mapcar #'buffer-name (buffer-list))))))
      (or (get-buffer buf-name) (find-file-noselect buf-name)))))
(use-package counsel
  :after ivy
  :config
  (setq counsel-find-file-at-point t))
(use-package beacon
  :config
  (beacon-mode 1)
  (setq-ns beacon-blink-when
    window-scrolls t
    point-moves-horizontally nil
    point-moves-vertically nil))
