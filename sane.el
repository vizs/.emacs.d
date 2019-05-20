;; sane defaults
;; im a dirty vim user
(use-package evil
  :config
  (setq evil-want-integration t
        evil-want-keybinding nil)
  (evil-mode t)
  (vz:style-evil-cursor))

(use-package general
  :after evil
  :init
  (setq general-override-states '(insert emacs hybrid normal
                                  visual motion operator replace)))

;; reload config when a new frame is launched
;;(add-hook 'after-make-frame-functions (lambda (arg) (vz:reload-config)))

;; who even needs these?
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(mouse-wheel-mode -1)

;; it gets really confusing
(setq-default cursor-in-non-selected-windows nil)

;; initial buffer
;; not using right now because i end up cumulating a lot of unused buffers
(defun vz:new-empty-buffer ()
  (interactive)
  (let (($buf (generate-new-buffer "new")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf))

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

;; much easier than highlight
(use-package rainbow-delimiters
  :config
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode))

(add-hook 'before-save-hook 'whitespace-cleanup)

(setq frame-title-format '("emacs: %f"))

(setq gc-cons-threshold 50000000)
