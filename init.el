;; -*- lexical-binding: t; -*-

(when (< emacs-major-version 27)
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(load-file (expand-file-name "lisp/queen.el" user-emacs-directory))
(load-file (expand-file-name "lisp/workers.el" user-emacs-directory))

(defvar vz/initial-loaded t)

;; TODO: Consider using a single file but make *org-scratch* to be a
;; narrowed view of date subtree

;; Make an org-scratch buffer.
;; To achive this, we will create a file in
;; `~/lib/org-scratch/<%Y%m%d%H%M>.org` and add this pattern
;; to `recentf-exclude' so as to remove previous scratch buffers from
;; `ivy--buffer-list'.
(make-thread ; Don't care how long it takes (can still block Emacs though)
 #'(lambda ()
     (with-eval-after-load 'recentf
      (add-to-list 'recentf-exclude
       (rx
        (eval (expand-file-name "~/lib/org-scratch/"))
        (one-or-more num)
        ".org")))
     (with-current-buffer
         (find-file-noselect (expand-file-name
                              (concat (format-time-string "%Y%m%d%H%M") ".org")
                              "~/lib/org-scratch"))
       (org-mode)
       (rename-buffer "*org-scratch*")
       (with-eval-after-load (expand-file-name "lisp/hive/modeline.el" user-emacs-directory)
        (setq-local vz/mode-line-file-include-file-status? nil
         vz/mode-line-file-include-file-short-path? nil)))))
(put 'list-timers 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
