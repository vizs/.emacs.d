;; -*- lexical-binding: t; -*-

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
  (--if-let (if (derived-mode-p 'shell-mode)
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

;; From https://0x0.st/oYX8
(defun vz/mode-line-fill (face except)
  (propertize " "
        'display `((space :align-to (- (+ right right-fringe right-margin)
                                       ,except)))
        'face face))

(setq-default
 battery-update-interval 360
 vz/mode-line-format `("    "
                       (:eval (vz/mode-line-file-short-dir))
                       "%b"
                       (:eval (vz/mode-line-file-state))
                       (:eval (vz/mode-line-fill 'mode-line 19))
                       "« "
                       (:eval (format-time-string "[%H:%M] "))
                       "["
                       (:eval (battery-format
                               "%b%p"
                               (funcall battery-status-function)))
                       "%%]")
 mode-line-format vz/mode-line-format)
