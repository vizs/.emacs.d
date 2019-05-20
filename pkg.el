;; use-package and friends
(setq package-user-dir (concat user-emacs-directory "pkgs"))

(when (version< emacs-version "27.0")
  (package-initialize))
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t
      vc-follow-symlinks t)
