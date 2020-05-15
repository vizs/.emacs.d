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
   "dk" 'counsel-descbinds
   "dF" 'counsel-describe-face
   "ff" 'counsel-find-file
   "j" 'counsel-imenu
   "b" 'ivy-switch-buffer))

(use-package company
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (setq-ns company
   require-match nil
   idle-delay 0.2
   tooltip-limit 10
   minimum-prefix-length 2)
  (general-define-key
   :keymaps 'company-active-map
   "M-n" nil
   "M-p" nil))

(use-package go-mode
  :config
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package racket-mode
  :config
  (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable))

(use-package nix-mode :mode "\\.nix\\'")

(dolist (h '(racket-mode-hook emacs-lisp-mode-hook
             scheme-mode-hook nix-mode-hook))
  (add-hook h #'(lambda () (setq indent-tabs-mode nil
                                 tab-width 2))))
