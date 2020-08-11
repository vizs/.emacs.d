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
  (--if-let (if (derived-mode-p 'comint-mode)
                (concat default-directory "/a")
              (buffer-file-name))
      (let* ((dir (->>
                   (f-dirname it)
                   (f-short)
                   (f-split)))
             (length (1- (length dir))))
        (concat
         (->>
          dir
          (-map-indexed (fn (if (eq <1> length)
                                <2>
                              (substring <2> 0 1))))
          (apply #'f-join)
          (f-short))
         "|"))
    ""))

(defun vz/mode-line-git-branch ()
  (-if-let (branch (when (or (buffer-file-name) (derived-mode-p 'comint-mode))
                     (car (vc-git-branches))))
      (format "(%s)" branch)
    ""))

(defun vz/mode-line-roundize-text (text)
  "Return an image object with text surrounded by arcs on either side."
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
                     :fill (if (moody-window-active-p) vz/mode-line-bg vz/mode-line-bgi))
      (svg-text svg (format " %s " text)
                :font-family (font-get f :family)
                :font-size (font-get f :size)
                :font-weight (font-get f :weight)
                :fill (if (moody-window-active-p) vz/mode-line-fg vz/mode-line-fgi)
                :x (/ h 2)
                :y (1+ (font-get f :size)))
      (propertize " " 'display (svg-image svg :ascent 'center)))))

;; From https://0x0.st/oYX8
(defun vz/mode-line-fill (face except)
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin)
                                           ,except)))
              'face face))

(setq-default
 battery-update-interval 360
 vz/mode-line-format `("    "
                       (:eval (vz/mode-line-roundize-text
                               (concat
                                (vz/mode-line-file-short-dir)
                                (buffer-name)
                                (vz/mode-line-file-state))))
                       " "
                       (:eval (vz/mode-line-roundize-text (vz/mode-line-git-branch)))
                       (:eval (vz/mode-line-fill 'mode-line 20))
                       (:eval (vz/mode-line-roundize-text (format-time-string "%H:%M")))
                       " "
                       (:eval (vz/mode-line-roundize-text (battery-format "%b%p%%" (funcall battery-status-function)))))
 mode-line-format vz/mode-line-format)

(-each '(:background :foreground)
  (fn (set-face-attribute 'mode-line nil <> (face-attribute 'default <>))
      (set-face-attribute 'mode-line-inactive nil <> (face-attribute 'default <>))))
