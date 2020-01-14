(setq user-emacs-directory (expand-file-name "~/etc/emacs.d/"))
(load-file (concat user-emacs-directory "util.el"))

;; variables
(setq vz/show-numbers?          nil
      vz/show-paren-highlight?  nil
      vz/tray                   -1
      vz/pad                    2
      vz/theme                 "mughal")

;; load rest of the files
(vz/load-elfiles '("pkg.el" "aesthetics.el" "mline.el"
                   "evil.el" "doc.el" "irc.el" "win.el"
                   "prog.el" "sane.el" ".secret.el"))

;; run functions
(vz/disable-bold-italic-underline)
