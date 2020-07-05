;; -*- lexical-binding: t; -*-
;; Extracted from doom-emacs' early-init.el and various other
;; Emacs configurations

(setq gc-cons-threshold most-positive-fixnum
      default-file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil
      menu-bar-mode nil
      tool-bar-mode nil
      frame-inhibit-implied-resize t
      package-enable-at-startup nil)

(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(advice-add #'x-apply-session-resources :override #'ignore)
(advice-add #'package--ensure-init-file :override #'ignore)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (garbage-collect)
              (setq gc-cons-threshold 16777216 ;; 16M
               file-name-handler-alist default-file-name-handler-alist
               frame-inhibit-implied-resize '(tab-bar-lines))))
