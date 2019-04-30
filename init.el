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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ace-window use-package twilight-bright-theme general evil eglot counsel company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#ffffffffffff"))))
 '(company-scrollbar-fg ((t (:background "#ffffffffffff"))))
 '(company-tooltip ((t (:inherit default :background "#ffffffffffff"))))
 '(company-tooltip-annotation ((t (:inherit font-lock-constant-face :background "#ffffffffffff"))))
 '(company-tooltip-annotation-selection ((t (:inherit font-lock-constant-face :background "#ffffffffffff"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face :background "#ffffffffffff"))))
 '(company-tooltip-selection ((t (:inherit font-lock-constant-face :background "#ffffffffffff")))))
