(load-file (concat user-emacs-directory "util.el"))
(vz:load-elfiles '("pkg.el" "aesthetics.el" "sane.el"
                   ".secret.el" "prog.el" "irc.el"
                   "doc.el" "bind.el"))
(vz:disable-bold-italic)
