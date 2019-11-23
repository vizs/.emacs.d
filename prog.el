;; autocompletion and other stuff

(use-package ivy
  :after general
  :config
  (add-hook 'prog-mode-hook 'ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (vz/bind-norm
   "/"        'swiper
   "SPC b"    'ivy-switch-buffer)
  (vz/bind
   nil
   'ivy-minibuffer-map
   "C-p"      nil
   "C-n"      nil
   "C-j"     'ivy-next-line
   "C-k"     'ivy-previous-line))

(use-package counsel
  :after ivy
  :config
  (vz/bind-norm
   ","        'counsel-M-x
   "SPC d f"  'counsel-describe-function
   "SPC d v"  'counsel-describe-variable
   "SPC d F"  'counsel-describe-face
   "SPC j"    'counsel-imenu))

(use-package company
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-require-match nil
        company-idle-delay 1
        company-tooltip-limit 10
        company-minimum-prefix-length 0)
  (vz/bind
   nil
   'company-active-map
   "M-n"       nil
   "M-p"       nil
   "C-j"      'company-select-next
   "C-k"      'company-select-previous))

;; (use-package rust-mode
;;   :config
;;   (autoload 'rust-mode "rust-mode" nil t)
;;   (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

(use-package racket-mode
  :config
  (add-hook 'racket-mode-hook 'racket-unicode-input-method-enable))

;; much easier than highlight
(use-package rainbow-delimiters
  :init
  (mapc (lambda (mode)
          (add-hook mode 'rainbow-delimiters-mode))
        (list 'lisp-mode-hook 'emacs-lisp-mode-hook 'racket-mode-hook)))

(when (vz/load-pkg "company-tabnine")
  (add-to-list 'company-backends #'company-tabnine))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '(racket-mode . ("racket-language-server")))
  (mapc (lambda (mode)
          (add-hook mode 'eglot-ensure))
        (list 'python-mode-hook 'racket-mode-hook)))
