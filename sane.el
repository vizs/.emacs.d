;; sane defaults
;; who even needs these?
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(mouse-wheel-mode -1)

;; it gets really confusing
(setq-default cursor-in-non-selected-windows nil)

;; default backup system is so retarded. sorry
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/var/cache/emacs-bkups"))
      delete-old-versions t
      keep-new-versions 5
      keep-old-versions 2
      version-control t)

;; the perfect way to switch windows
(use-package ace-window
  :ensure t)

(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq-default indent-tabs-mode nil)

;; dont clutter init.el
;;(setq custom-file (concat user-emacs-directory ".custom.el"))
;;(when (file-exists-p custom-file)
;;  (load custom-file))

;; disable custom.el
(defconst custom-file "/dev/zero")


(add-hook 'before-save-hook 'whitespace-cleanup)
(setq frame-title-format '("emacs: %f"))
(setq gc-cons-threshold 50000000)
