(defun vz:load-elfiles (files)
  (when files
    (load-file (concat user-emacs-directory (car files)))
    (vz:load-elfiles (cdr files))))

(defun vz:reload-config ()
  (load-file (concat user-emacs-directory "init.el")))
