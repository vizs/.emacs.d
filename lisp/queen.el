;; -*- lexical-binding: t; -*-

;; * Set variables and sane defaults
;; ** Sanity
(setq-default
 vz/monospace-font (replace-regexp-in-string
                    "\n$" ""
                    (shell-command-to-string "fc-match -f %{family} monospace"))
 vz/variable-font (replace-regexp-in-string
                   "\n$" ""
                   (shell-command-to-string "fc-match -f %{family} serif"))

 vz/ircdiscord-process nil
 vz/functional-major-modes '(nix-mode emacs-lisp-mode racket-mode scheme-mode)

 use-dialog-box nil

 ;; Initialise `Info-directory-list' with Emacs info files
 Info-directory-list Info-default-directory-list

 backup-by-copying t
 backup-directory-alist '((".*" . "~/.cache/emacs-bkups/"))
 delete-old-versions t
 keep-new-versions 5
 keep-old-versions 2
 version-control t
 auto-save-file-name-transforms '((".*" "~/.cache/emacs-autosave/" t))
 auto-save-list-file-prefix "~/.cache/emacs-autosave/"
 create-lockfiles nil

 ;; Cursor styling
 cursor-in-non-selected-windows nil
 cursor-type '(bar . 2)

 custom-file "/dev/null"

 ;; Startup stuff
 initial-scratch-message nil
 inhibit-startup-screen t
 initial-buffer-choice t

 ;; Insert newline at EOF
 require-final-newline t

 ;; I prefer to separate default kill register and clipboard
 x-select-enable-clipboard nil

 ;; Follow links to version controlled files
 vc-follow-symlinks t

 tab-always-indent 'complete

 enable-local-eval t

 ;; Horizontal scrolling
 mouse-wheel-tilt-scroll t
 mouse-wheel-flip-direction t

 ;; Indentation
 indent-tabs-mode t
 tab-width 4

 frame-resize-pixelwise t

 ;; Move to bottom/top of buffer
 scroll-error-top-bottom t

 ;; Avoid duplicate items in kill-ring
 kill-do-not-save-duplicates t

 disabled-command-function nil

 auto-save-no-message t

 ;; I can't write anything anw
 view-read-only t

 ;; This is much easier on the eyes
 uniquify-buffer-name-style 'forward)

(defalias 'yes-or-no-p 'y-or-n-p)

;; *** Delete region when inserting text
;; (delete-selection-mode t)

;; ** Indentation
(defvar c-basic-offset 4)
(defvar cperl-basic-offset 4)
(defvar python-indent 4)

(add-hook 'prog-mode-hook
          (defun vz/prog-functional-indent-style ()
            (when (apply #'derived-mode-p vz/functional-major-modes)
              (setq-local indent-tabs-mode nil
                          tab-width 2))))

;; * Niceties
;; ** Helper macros
(require 'seq)

;; *** setq but with namespace
;; inspo: https://github.com/neeasade/emacs.d
;; (defmacro setq-ns (ns &rest args)
;;   "`setq' but with the namespace as NS.
;; If variable is a cons cell, then cdr is attached to setq.
;; For example, to set a buffer local variable, you pass the variable name
;; as (name-without-ns . local)."
;;   (declare (indent 1))
;;   `(progn
;;      ,@(mapcar (lambda (x)
;;                  (let ((set 'setq)
;;                        (var (car x)))
;;                   (when (and (listp var) (not (listp (cdr var))))
;;                    (setq set (intern (format "setq-%s" (cdr var)))
;;                     var (car var)))
;;                   (list set (intern (format "%s-%s" ns var)) (cadr x))))
;;         (seq-partition args 2))))

;; *** setq but for a hook
;; inspo: doom-emacs

;; (defmacro setq-hook--create-fun (mode &rest body)
;;   "Helper macro to create function for `setq-hook'."
;;   `(defun ,(intern (format "vz/setq-for-%s" mode)) ()
;;     (setq-local ,@body)))

;; (defmacro setq-hook (mode &rest body)
;;   "Set buffer local variable for a major mode."
;;   (declare (indent 1))
;;   (let ((f `(setq-hook--create-fun ,mode ,@body)))
;;     `(add-hook ',mode ,f nil t)))

;; *** Formating s-expressions?
;; from u/b3n
;; (defmacro vz/format-sexp (sexp &rest format-args)
;;   "Format SEXP and eval it."
;;   `(eval (read (format ,(format "%S" sexp) ,@format-args))))

;; ** Automatically chmod +x file if it has a shebang
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; ** Auto-revert buffer if file is modified
;; From u/redblobgames
(setq auto-revert-use-notify t
      auto-revert-avoid-polling t)

(global-auto-revert-mode)

;; ** Fonts
(add-to-list 'default-frame-alist `(font . ,(format "%s:pixelsize=12"
                                             vz/monospace-font)))

(set-face-attribute 'variable-pitch nil :family vz/variable-font)

;; Color Emoji Time

(add-hook 'server-after-make-frame-hook
          (defun vz/set-emoji-range ()
            (set-fontset-font t 'symbol
                              "Noto Color Emoji" nil 'prepend)
            (remove-hook 'server-after-make-frame-hook
                         #'vz/set-emoji-range)))

;; ** Display … instead of $ at the visual end of truncated line
(set-display-table-slot standard-display-table 'truncation ?…)

;; ** Inner border
(add-to-list 'default-frame-alist '(internal-border-width . 4))

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
(setq straight-use-package-by-default t
      straight-cache-autoloads t
      straight-vc-git-default-clone-depth 1)

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
     (load-file (format "%s/lisp/hive/%s.el" user-emacs-directory (or ,file ',name)))))

;; ** Random helper functions
(defun ~ (file)
  "Path to FILE respective to $HOME"
  (expand-file-name file (getenv "HOME")))

;; ** Font related functions
(defun vz/set-monospace-faces (faces)
  (-each faces
    (fn: set-face-attribute <> nil :family vz/monospace-font)))

(defun vz/set-variable-faces (faces)
  (-each faces
    (fn: set-face-attribute <> nil :family vz/variable-font)))

;; ** Quality of life packages
;; *** Saner switch window and goto-char motion
(use-package avy
  :bind
  (("C-S-n" . avy-goto-line-below)
   ("C-S-p" . avy-goto-line-above)))
(use-package ace-window
  :after avy
  :functions vz/ace-delete-window
  :bind (("C-x o" . ace-window)
	       ("C-x 0" . vz/ace-delete-window))
  :config
  (defun vz/ace-delete-window ()
    "If window count is 2, run `delete-window`; `ace-delete-window` otherwise."
    (interactive)
    (if (eq 2 (length (window-list)))
        (delete-window)
      (ace-delete-window)))
  ;; More noticable this way
  (set-face-attribute 'aw-leading-char-face nil :height 150)
  (setq aw-keys '(?a ?s ?d ?f ?h ?j ?k ?l)
        ;; Only consider the windows in the active frame
        aw-scope 'frame))

;; *** Selection engine
(use-package ivy
  :functions vz/ivy-minibuffer-insert-at-point
  :bind (:map ivy-minibuffer-map
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
  (defun vz/get-file-or-buffer ()
    "Select a list of opened buffers, files in current directory and entries in
recentf and return the corresponding buffer. Create one if it doesn't exist"
    (--> (append (ivy--virtual-buffers)
                 (-filter #'f-file? (directory-files default-directory))
                 (-map #'buffer-name (buffer-list)))
         (ivy-read "> " it)
         (or (get-buffer it) (find-file-noselect it))))
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
  (-each '(yank-pop describe-bindings)
    (fn (define-key counsel-mode-map (vector 'remap <>) nil)))
  (counsel-mode t))

;; *** Blink cursor on certain actions
(use-package beacon
  :config
  (beacon-mode 1)
  (setq beacon-blink-when-window-scrolls t
        beacon-blink-when-point-moves-horizontally nil
        beacon-blink-when-point-moves-vertically nil)
  (defun vz/beacon-highlight ()
    "`beacon-blink' for one second to capture attention"
    (let ((beacon-blink-duration 1))
      (beacon-blink))))

;; * Custom theme
(add-to-list 'custom-theme-load-path
             (expand-file-name "lisp/themes" user-emacs-directory))

(load-theme 'vz-nh t)

;; Local Variables:
;; eval: (outline-minor-mode)
;; outline-regexp: ";; [*]+"
;; End:
