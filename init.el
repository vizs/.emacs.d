;; -*- lexical-binding: t; -*-

(when (< emacs-major-version 27)
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(load-file (expand-file-name "lisp/queen.el" user-emacs-directory))
(load-file (expand-file-name "lisp/workers.el" user-emacs-directory))

(defvar vz/initial-loaded t)
