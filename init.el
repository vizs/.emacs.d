;; -*- lexical-binding: t; -*-

(when (< emacs-major-version 27)
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(load-file (expand-file-name "lisp/queen.el" user-emacs-directory))
(load-file (expand-file-name "lisp/workers.el" user-emacs-directory))

(unless (boundp 'vz/initial-loaded)
  (vz/disable-bold-italic))

(defvar vz/initial-loaded t)
