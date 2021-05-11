;; -*- lexical-binding: t; -*-

(when (< emacs-major-version 27)
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(load-file (expand-file-name "lisp/queen.el" user-emacs-directory))
(load-file (expand-file-name "lisp/workers.el" user-emacs-directory))

(defvar vz/initial-loaded t)


(add-hook 'server-after-make-frame-hook
          (defun vz/set-DISPLAY ()
            (setenv "DISPLAY" ":0")
            (remove-hook 'server-after-make-frame-hook #'vz/set-DISPLAY)))
