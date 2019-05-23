;; TODO: remove this and remap C-n C-p to C-j C-k in ex

;; evil
(general-define-key
 :states 'normal
 :keymaps 'override
 (kbd "C-w o") nil
 (kbd "g c") 'comment-line
 (kbd "C-w o") 'ace-window
 (kbd "C-w O") 'delete-other-windows)
(general-define-key
 :states 'visual
 :keymaps 'override
 (kbd "g c") 'comment-line)

;; ivy and counsel
(general-define-key
  :states 'normal
  :keymaps 'override
  (kbd "/") 'swiper
  (kbd "C-s") 'swiper
  (kbd ",") 'counsel-M-x)

(general-define-key
  :states '(normal insert)
  :keymaps 'override
  (kbd "M-x") 'counsel-M-x
  (kbd "C-x C-f") 'counsel-find-file
  (kbd "C-h f") 'counsel-describe-function
  (kbd "C-h v") 'counsel-describe-variable)

(define-key ivy-minibuffer-map (kbd "C-p") nil)
(define-key ivy-minibuffer-map (kbd "C-n") nil)
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)

;; company
(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-j") 'company-select-next)
(define-key company-active-map (kbd "C-k") 'company-select-previous)

;; ace-window
(general-define-key
 :states '(normal insert)
 :keymaps 'override
 (kbd "C-x o") 'ace-window)
