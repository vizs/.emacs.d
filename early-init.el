;; -*- lexical-binding: t; -*-
;; Extracted from doom-emacs' early-init.el

(setq gc-cons-threshold most-positive-fixnum
      menu-bar-mode nil
      tool-bar-mode nil
      frame-inhibit-implied-resize t
      package-enable-at-startup nil)

(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(advice-add #'x-apply-session-resources :override #'ignore)
(advice-add #'package--ensure-init-file :override #'ignore)
