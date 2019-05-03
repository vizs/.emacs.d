;; autocompletion and other stuff
(use-package ivy
  :after general
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t))

(use-package counsel
  :after ivy
  :ensure t)

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (vz:theme-company)
  (setq company-require-match 'never
        company-idle-delay 1
        company-tooltip-limit 10
        company-minimum-prefix-length 0))

(use-package rust-mode
  :ensure t
  :config
  (autoload 'rust-mode "rust-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

;; this just fucking works
(use-package eglot
  :ensure t
  :config
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure))

;; TODO: racket
