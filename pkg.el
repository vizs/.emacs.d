;; use-package and friends
(setq package-user-dir "~/var/cache/emacs-pkgs")

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
(setq use-package-always-ensure  t
      vc-follow-symlinks         t)

(defun vz/load-pkg (pkg-name)
  (let ((pkg-path (concat user-emacs-directory "/pkgs/" pkg-name)))
    (if (file-directory-p pkg-path)
	(progn
	  (add-to-list 'load-path pkg-path)
	  t)
      nil)))
