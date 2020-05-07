(setq
 backup-by-copying t
 backup-directory-alist '((".*" . "~/.cache/emacs-bkups/"))
 delete-old-versions t
 keep-new-versions 5
 keep-old-versions 2
 version-control t
 auto-save-file-transforms '((".*" "~/.cache/emacs-autosave/" t))
 auto-save-list-file-prefix "~/.cache/emacs-autosave/"
 create-lockfiles nil
 cursor-in-non-selected-windows nil
 custom-file "/dev/null"
 ;; frame-title-format '("emacs: " (:eval ...))
 gc-cons-threshold 50000000
 x-select-enable-clipboard nil)
(defalias 'yes-or-no-p 'y-or-n-p)

(load-file (concat user-emacs-directory "util.el"))
(vz/load-files '("pkg.el" "evil.el" "prog.el" "shell.el" "ui.el" "doc.el"))
