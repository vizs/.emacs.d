;; sane defaults

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(mouse-wheel-mode -1)
(blink-cursor-mode 0)

(setq-default cursor-type      '(hbar . 3)
              tab-width         4
              indent-tabs-mode  nil)

(setq backup-by-copying               t
      backup-directory-alist         '(("." . "~/var/cache/emacs-bkups"))
      delete-old-versions             t
      keep-new-versions               5
      keep-old-versions               2
      version-control                 t
      auto-save-file-transforms      '((".*" "~/var/cache/emacs-bkups" t))
      auto-save-list-file-prefix     "~/var/cache/emacs-bkups"
      auto-save-default               nil
      create-lockfiles                nil
      cursor-in-non-selected-windows  nil
      custom-file                     (concat user-emacs-directory ".custom.el")
      frame-title-format             '("emacs: "
                                       (:eval (vz/mode-line-file-name)))
      gc-cons-threshold               50000000)

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package ace-window)
(use-package disable-mouse
  :config
  (global-disable-mouse-mode))

(defvaralias 'c-basic-offset     'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq-default indent-tabs-mode nil)

(when (file-exists-p custom-file)
  (load custom-file))

(add-hook 'before-save-hook 'whitespace-cleanup)
