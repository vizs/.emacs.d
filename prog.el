(setq-default
 indent-tabs-mode t
 tab-width 4)

(defvar c-basic-offset 4)
(defvar cperl-basic-offset 4)
(defvar python-indent 4)

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1)
  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "C-p" nil
   "C-n" nil
   "<C-up>" #'ivy-minibuffer-grow
   "<C-down>" #'ivy-minibuffer-shrink
   "C-s" #'ivy-avy
   "C-j" #'ivy-next-line
   "C-k" #'ivy-previous-line
   "C-p" #'ivy-minibuffer-grow
   "C-n" #'ivy-minibuffer-shrink
   "C-u" #'ivy-scroll-down-command
   "C-d" #'ivy-scroll-up-command)
  (general-define-key
   :keymaps 'ivy-switch-buffer-map
   "C-k" #'ivy-previous-line
   ;; Shift cannot be bound???
   "C-M-K" #'ivy-switch-buffer-kill)
  (general-nmap
   :keymaps 'override
   "/" #'swiper
   "?" #'swiper-backward))

(use-package counsel
  :after ivy
  :config
  (setq-ns counsel
   find-file-at-point t)
  (general-nmap
    :prefix "SPC"
    ;; potentially useful counsel commands:
    ;; #'counsel-search
    ;; #'counsel-switch-to-shell-buffer
    "df" #'counsel-describe-function
    "dv" #'counsel-describe-variable
    "dk" #'counsel-descbinds
    "dF" #'counsel-describe-face
    "ff" #'counsel-find-file
    "j" #'counsel-imenu
    "b" #'ivy-switch-buffer))

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
   "M-p" nil
   "C-j" #'company-select-next
   "C-k" #'comapny-select-previous))

(use-package hl-todo
  :config
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  (setq hl-todo-highlight-punctuation ":")
  (general-nmap
    :prefix "["
    "j" #'hl-todo-next
    "k" #'hl-todo-previous))

(use-package go-mode
  :config
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package racket-mode
  :config
  (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable))

(use-package nix-mode :mode "\\.nix\\'")

(use-package python
  :config
  (setq-ns python-shell
    interpreter "python3"
    interpreter-args "-i"))

(use-package scheme
  :config
  (setq scheme-program-name "csi"))

(dolist (h '(racket-mode-hook emacs-lisp-mode-hook
             scheme-mode-hook nix-mode-hook))
  (add-hook h #'(lambda () (setq indent-tabs-mode nil tab-width 2))))
