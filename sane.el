;; sane defaults

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(mouse-wheel-mode -1)

(setq-default cursor-in-non-selected-windows nil)

(setq backup-by-copying t
      backup-directory-alist '(("." . "~/var/cache/emacs-bkups"))
      delete-old-versions t
      keep-new-versions 5
      keep-old-versions 2
      version-control t)

(use-package ace-window)

(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq-default indent-tabs-mode nil)

;; "disable" custom.el
(defconst custom-file "/dev/zero")

(add-hook 'before-save-hook 'whitespace-cleanup)
(setq frame-title-format '("emacs: "
                           (vz:mode-line-file-name)))
(setq gc-cons-threshold 50000000)
(defalias 'yes-or-no-p 'y-or-n-p)
