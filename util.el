(defun vz:load-elfiles (files)
  (when files
    (load-file (concat user-emacs-directory (car files)))
    (vz:load-elfiles (cdr files))))

(defun vz:reload-config ()
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

(defun vz:minimal-ui ()
  "minimal ui - hides mode-line and fringe"
  (interactive)
  (setq mode-line-format nil)
  (fringe-mode '(0 . 0))
  (display-line-numbers-mode 0))

(defun vz:no-bloat ()
  "no bloat - disable company, ivy, etc."
  (interactive)
  (company-mode -1)
  (ivy-mode -1))

(defun vz:no-bloat-too-much ()
  (vz:no-bloat)
  (undo-tree-mode -1)
  (show-paren-mode -1))

(defun vz:irc-mode ()
  (interactive)
  (load-file (concat user-emacs-directory "irc.el"))
  (vz:no-bloat-too-much)
  (vz:minimal-ui)
  (circe "disc:r/up"))

(defun vz:norm-mode ()
  (interactive)
  (vz:load-elfiles '("evil.el" "prog.el" "irc.el"
                     "doc.el" "bind.el")))

(defun vz:prog-mode ()
  (interactive)
  (vz:load-elfiles '("evil.el" "prog.el" "bind.el")))

(defun vz:do-client-mode ()
  (if (eq (boundp 'vz:preferred-mode) nil)
      (setq vz:preferred-mode 'vz:norm-mode))
  (funcall vz:preferred-mode))
