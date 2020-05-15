(defun vz/conf-path (file)
  "Get path to file in user-emacs-directory"
  (expand-file-name (concat user-emacs-directory file)))

(defun vz/load-files (files)
  "Load files. Path is relative to user-emacs-directory"
  (when files
    (load-file (vz/conf-path (car files)))
    (vz/load-files (cdr files))))

(defun vz/reload-config ()
  "Reload init.el"
  (interactive)
  (load-file (vz/conf-path "init.el"))
  (redraw-display)
  (force-mode-line-update t))

(defun vz/fread (path)
  "Read file and return the contents"
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-string)))

;; From https://github.com/neeasade
(defun vz/eval-file (path stdin args)
  "Evaluate elisp file in path"
  (eval (ignore-errors (read-from-whole-string (vz/fread path)))))

(defun vz/cd-selbuf (path)
  "Change working directory of selected buffer"
  (with-current-buffer (window-buffer (selected-window))
	(setq default-directory path)))

(defun pass (passwd)
  "Get password"
  (replace-regexp-in-string "\n$" ""
	(shell-command-to-string (format "pass get %s" passwd))))

(defun pass-irc (serv)
  `(lambda (_) (pass (format "irc/%s" ,serv))))

(defun pass-discord (serv)
  `(lambda (_) (format "%s:%d" (pass "misc/discord") ,serv)))

;; inspo: https://github.com/neeasade/emacs.d
(defmacro setq-ns (ns &rest args)
  "Set variables with pre as their `namespace'"
  (let ((ns (prin1-to-string ns)))
    (dolist (x (seq-partition args 2))
      (set (intern (format "%s-%s" ns (car x))) (cadr x)))))

(defun vz/random-choice (list)
  (nth (random 0 (1- (length list))) list))
