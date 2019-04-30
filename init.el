(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("GNU" . "https://elpa.gnu.org/packages/"))

;;(when (not package-archive-contents)
;;  (package-refresh-contents))
(when (version< emacs-version "27.0")
  (unless package--initialized (package-initialize t)))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq vc-follow-symlinks t)

(load-file (concat user-emacs-directory "util.el"))

(vz:load-elfiles '("sane.el"
                   "aesthetics.el"
                   "prog.el"))
