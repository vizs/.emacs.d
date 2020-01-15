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

(defun vz/between? (n a b)
  (and (>= n a)
       (<= n b)))

;; from https://github.com/neeasade/dotfiles/blob/master/bin/bin/elisp
(defun vz/eval-file (path)
  (eval
   (ignore-errors
     (read-from-whole-string (vz/fread path)))))
