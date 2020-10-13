;; -*- lexical-binding: t; -*-

(use-package moody
  :demand t)

(-each '(display-time-mode display-battery-mode)
  (fn: funcall <> t))

(defun vz/mode-line-file-state ()
  (if (buffer-file-name)
      (cond
       (buffer-read-only    " [!]")
       ((buffer-modified-p) " [+]")
       (t                   ""))
    ""))

(defun vz/mode-line-file-short-dir ()
  (if-let ((path (if (derived-mode-p 'comint-mode)
                   (concat default-directory "/a")
                 (buffer-file-name))))
      (let ((dir (->>
                   (f-dirname path)
                   (f-short)
                   (f-split))))
        (concat
         (f-short (apply #'f-join (-map (fn (substring <> 0 1)) (-drop-last 1 dir))))
         "/" (-last-item dir) "|"))
    ""))

(defun vz/mode-line-git-branch ()
  (if-let ((branch (when (or (buffer-file-name)
                             (derived-mode-p 'comint-mode))
                     (car (vc-git-branches)))))
      (format "(%s)" branch)
    ""))

;; From https://0x0.st/oYX8
(defun vz/mode-line-fill (face except)
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin)
                                           ,except)))
              'face face))

(defun vz/mode-line-roundise-text (text &optional foreground background)
  "Return an image object with TEXT surrounded by arcs on either side."
  (require 'svg)
  (if (s-blank? text)
      ""
    (let* ((f (font-at 0 nil "l"))
           (fw (window-font-width nil 'mode-line))
           (h (window-font-height nil 'mode-line))
           (w (+ h (* fw (string-width text))))
           (svg (svg-create w h)))
      (svg-rectangle svg 0 0 w h
                     :rx (/ h 2)
                     :ry (/ h 2)
                     :fill (or background
                               (if (moody-window-active-p) vz/mode-line-bg vz/mode-line-bgi)))
      (svg-text svg (format " %s " text)
                :font-family (font-get f :family)
                :font-size   (font-get f :size)
                :font-weight (font-get f :weight)
                :fill (or foreground
                          (if (moody-window-active-p) vz/mode-line-fg vz/mode-line-fgi))
                :x (/ h 2)
                :y (1+ (font-get f :size)))
      (propertize " " 'display (svg-image svg :ascent 'center)))))

;; Update battery and time at intervals
(setq-default vz/mode-line-battery nil
              vz/mode-line-time nil)

(make-thread
 (fn (while t
       (setq-default
        vz/mode-line-battery (vz/mode-line-roundise-text
                              (battery-format "%p%%%b" (funcall battery-status-function))))
       (sleep-for battery-update-interval)))
 "vz/mode-line-battery setter")

(make-thread
 (fn (while t
       (setq-default vz/mode-line-time (vz/mode-line-roundise-text (format-time-string "%H:%M")))
       (sleep-for display-time-interval)))
 "vz/mode-line-time setter")

(setq-default
 battery-update-interval 360
 vz/mode-line-extra-info '(:eval (and
                                  (window-at-side-p nil 'right)
                                  (window-at-side-p nil 'bottom)
                                  (concat
                                   vz/mode-line-time
                                   " "
                                   vz/mode-line-battery)))
 vz/mode-line-format `("  "
                       (:eval (vz/mode-line-roundise-text
                               (concat (vz/mode-line-file-short-dir)
                                (buffer-name)
                                (vz/mode-line-file-state))))
                       " "
                       (:eval (vz/mode-line-roundise-text (vz/mode-line-git-branch)))
                       (:eval (vz/mode-line-fill 'mode-line 18))
                       ,vz/mode-line-extra-info)
 mode-line-format vz/mode-line-format)
