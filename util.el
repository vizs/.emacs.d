(defun vz/load-elfiles (files)
  (when files
    (load-file (concat user-emacs-directory (car files)))
    (vz/load-elfiles (cdr files))))

(defun vz/reload-config ()
  (interactive)
  (load-file (concat user-emacs-directory "init.el"))
  (redraw-display)
  (force-mode-line-update t))

(defun vz/fread (path)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-string)))

(defun vz/spawn-st ()
  (interactive)
  (call-process "tab" nil 0 nil "-w" "st"))

(defun vz/getenv-or (env fallback)
  (let ((evar (getenv env)))
    (if evar
        evar
      fallback)))

(defun vz/bind (state keymap &rest binds)
  (when binds
    (general-define-key
     :states state :keymaps keymap
     (car binds) (cadr binds))
    (apply 'vz/bind state keymap (cddr binds))))

(defun vz/bind-norm (&rest binds)
  (apply 'vz/bind 'normal 'override binds))

(defun vz/bind-nois (&rest binds)
  (apply 'vz/bind '(normal insert) 'override binds))

(defun vz/bind-vis (&rest binds)
  (apply 'vz/bind 'visual 'override binds))
