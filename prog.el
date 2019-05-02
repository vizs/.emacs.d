;; autocompletion and other stuff
(use-package ivy
  :after general
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (general-define-key
   :states 'normal
   :keymaps 'override
   (kbd "/") 'swiper
   (kbd "C-s") 'swiper))

(use-package counsel
  :after ivy
  :ensure t
  :config
  (general-define-key
   :states 'normal
   :keymaps 'override
   (kbd ";") 'counsel-M-x)
  (general-define-key
   :states '(normal insert)
   :keymaps 'override
   (kbd "M-x") 'counsel-M-x
   (kbd "C-x C-f") 'counsel-find-file
   (kbd "C-x C-h") 'counsel-describe-function
   (kbd "C-x C-v") 'counsel-describe-variable))

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (vz:theme-company)
  (setq company-require-match 'never
        company-idle-delay 1
        company-tooltip-limit 10
        company-minimum-prefix-length 0)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

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
