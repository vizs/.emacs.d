;; autocompletion and other stuff

(electric-pair-mode)

(use-package ivy
  :after general
  :config
  (add-hook 'prog-mode-hook 'ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (vz/theme-ivy)
  (general-define-key
   :states    'normal
   :keymaps   'override
   "/"        'swiper
   "SPC b"    'ivy-switch-buffer)
  (general-define-key
   :keymaps  'ivy-minibuffer-map
   "C-p"      nil
   "C-n"      nil
   "C-j"     'ivy-next-line
   "C-k"     'ivy-previous-line))

(use-package counsel
  :after ivy
  :config
  (general-define-key
   :states    'normal
   :keymaps   'override
   ","        'counsel-M-x
   "SPC d f"  'counsel-describe-function
   "SPC d v"  'counsel-describe-variable
   "SPC d F"  'counsel-describe-face
   "SPC ,"    'counsel-imenu))

(use-package company
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (vz/theme-company)
  (setq company-require-match 'never
        company-idle-delay 1
        company-tooltip-limit 10
        company-minimum-prefix-length 0)
  (general-define-key
   :keymaps   'company-active-map
   "M-n"       nil
   "M-p"       nil
   "C-j"      'company-select-next
   "C-k"      'company-select-previous))

(use-package rust-mode
  :config
  (autoload 'rust-mode "rust-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

;; this just fucking works
(use-package eglot
 :config
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure))

(use-package racket-mode
  :config
  (add-hook 'racket-mode-hook 'racket-unicode-input-method-enable))

;; much easier than highlight
(use-package rainbow-delimiters
  :init
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'racket-mode-hook 'rainbow-delimiters-mode)
  :config
  (vz/theme-rainbow-parens))

(when (vz/load-pkg "company-tabnine")
  (add-to-list 'company-backends #'company-tabnine))
