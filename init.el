(load-file (concat user-emacs-directory "util.el"))
(vz:load-elfiles '("pkg.el" "aesthetics.el" "mline.el"
                   "sane.el" ".secret.el"))
;; TODO: find a nice way to integrate these modes
(vz:norm-mode)
(vz:disable-bold-italic-underline)
(vz:set-mode-line-face)
