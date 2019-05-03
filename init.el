(load-file (concat user-emacs-directory "util.el"))

(vz:init-package)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq vc-follow-symlinks t)

(vz:load-elfiles '("sane.el"
                   "aesthetics.el"
                   "prog.el"
                   "bind.el"))
