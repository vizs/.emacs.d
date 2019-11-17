(setq user-emacs-directory "~/etc/emacs.d/")
(load-file (concat user-emacs-directory "util.el"))
(vz/load-elfiles '("pkg.el" "aesthetics.el" "mline.el"
                   "doc.el" "evil.el" "irc.el"
                   "prog.el" "sane.el" ".secret.el" "term.el"))
(vz/disable-bold-italic-underline)
(vz/set-mode-line-face)
