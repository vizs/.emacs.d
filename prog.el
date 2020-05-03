(setq-default
 indent-tabs-mode t
 tab-width 4)

(dolist (v '(c-basic-offset cperl-basic-offset python-indent))
  (defvaralias v 'tab-width))

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1)
  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "C-p" nil
   "C-n" nil
   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line)
  (general-nmap
   :keymaps 'override
   "/" 'swiper
   "?" 'swiper-backward))

(use-package counsel
  :after ivy
  :config
  (general-nmap
   :prefix "SPC"
   "df" 'counsel-describe-function
   "dv" 'counsel-describe-variable
   "dF" 'counsel-describe-face
   "ff" 'counsel-find-file
   "j" 'counsel-imenu
   "b" 'ivy-switch-buffer))

(use-package company
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (setq company-require-match nil
        company-idle-delay 1
        company-tooltip-limit 10
        company-minimum-prefix-length 0)
  (general-define-key
   :keymaps 'company-active-map
   "M-n" nil
   "M-p" nil
   "C-n" 'company-select-next
   "C-p" 'company-select-previous))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package go-mode
  :config
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package racket-mode
  :config
  (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable))
