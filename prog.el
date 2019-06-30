;; autocompletion and other stuff
(use-package ivy
  :after general
  :config
  (add-hook 'prog-mode-hook 'ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (vz:theme-ivy))

(use-package counsel
  :after ivy)

(use-package company
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (vz:theme-company)
  (setq company-require-match 'never
        company-idle-delay 1
        company-tooltip-limit 10
        company-minimum-prefix-length 0))

(use-package rust-mode
  :config
  (autoload 'rust-mode "rust-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

;; this just fucking works
(use-package eglot
  :config
  (add-hook 'python-mode-hook 'eglot-ensure))
  ;;(add-hook 'rust-mode-hook 'eglot-ensure))

(use-package racket-mode)

;; much easier than highlight
(use-package rainbow-delimiters
  :init
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'racket-mode-hook 'rainbow-delimiters-mode)
  :config
  (vz:theme-rainbow-parens))

;; TODO: racket
