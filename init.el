;; -*- lexical-binding: t; -*-

(when (< emacs-major-version 27)
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(load-file (expand-file-name "lisp/queen.el" user-emacs-directory))
(load-file (expand-file-name "lisp/workers.el" user-emacs-directory))

(unless (boundp 'vz/initial-loaded)
  (vz/disable-bold-italic))

(defvar vz/initial-loaded t)

;; Add `Info-default-directory-list' to `Info-directory-list'

(setq Info-directory-list (append Info-directory-list Info-default-directory-list))
(put 'list-threads 'disabled nil)

;; Make an org-scratch buffer.
;; To achive this, we will create a file in
;; `~/lib/org-scratch/<%Y%m%d%H%M>.org` and add this pattern
;; to `recentf-exclude' so as to remove previous scratch buffers from
;; `ivy--buffer-list'.
(make-thread                            ; Don't care how long it takes
 (fn
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude
                 (eval `(rx
                         ,(expand-file-name "~/lib/org-scratch/")
                         (one-or-more num)
                         ".org"))))
  (with-current-buffer
      (find-file-noselect (expand-file-name
                           (concat (format-time-string "%Y%m%d%H%M") ".org")
                           "~/lib/org-scratch"))
    (rename-buffer "*org-scratch*")
    (with-eval-after-load (expand-file-name "lisp/hive/modeline.el" user-emacs-directory)
      (setq-local vz/mode-line-file-include-file-status? nil
                  vz/mode-line-file-include-file-short-path? nil)))))
(put 'list-timers 'disabled nil)
