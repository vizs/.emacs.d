(setq package-user-dir (expand-file-name "~/.cache/emacs-pkgs"))

;; Need it in emacs 27 if you change package-user-dir
(package-initialize)

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package) (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t
      vc-follow-symlinks t)

(defun vz/load-pkg (name)
  "Load local package named name"
  (let ((path (expand-file-name (concat user-emacs-directory "pkgs/" name))))
    (if (file-directory-p path)
        (progn (add-to-list 'load-path path)
          (load name)
          t)
      nil)))
