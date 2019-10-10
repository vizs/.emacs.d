(defun vz/load-elfiles (files)
  (when files
    (load-file (concat user-emacs-directory (car files)))
    (vz/load-elfiles (cdr files))))

(defun vz/reload-config ()
  (interactive)
  (load-file (concat user-emacs-directory "init.el"))
  (redraw-display)
  (force-mode-line-update t))

(defun vz/minimal-ui ()
  "minimal ui - hides mode-line and fringe"
  (interactive)
  (setq header-line-format nil)
  (setq mode-line-format nil)
  (fringe-mode '(0 . 0))
  (display-line-numbers-mode 0))

(defun vz/no-bloat ()
  "no bloat - disable company, ivy, etc."
  (interactive)
  (company-mode -1)
  (ivy-mode -1))

(defun vz/no-bloat-too-much ()
  (vz/no-bloat)
  (undo-tree-mode -1)
  (show-paren-mode -1))

(defun vz/irc-mode ()
  (interactive)
  (load-file (concat user-emacs-directory "irc.el"))
  (vz/no-bloat-too-much)
  (vz/minimal-ui)
  (circe "disc:r/up"))

(defun vz/norm-mode ()
  (interactive)
  (vz/load-elfiles '("evil.el" "prog.el" "irc.el"
                     "doc.el")))

(defun vz/prog-mode ()
  (interactive)
  (vz/load-elfiles '("evil.el" "prog.el")))

(defun vz/do-client-mode ()
  (if (eq (boundp 'vz/preferred-mode) nil)
      (setq vz/preferred-mode 'vz/norm-mode))
  (funcall vz/preferred-mode))

(defun vz/fread (path)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-string)))

(defun vz/load-pkg (pkg-name)
  (let ((pkg-path (concat user-emacs-directory "/pkgs/" pkg-name)))
    (if (file-directory-p pkg-path)
        (progn
          (add-to-list 'load-path pkg-path)
          t)
      nil)))
