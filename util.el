(defun vz:init-package ()
  ;; if emacs version < 27
  (when (version< emacs-version "27.0")
    (package-initialize))
  (require 'package)

  ;; add melpa
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  ;; package refresh content
  (when (not package-archive-contents)
    (package-refresh-contents)))

(defun vz:load-elfiles (files)
  (when files
    (load-file (concat user-emacs-directory (car files)))
    (vz:load-elfiles (cdr files))))

(defun vz:reload-config ()
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))
