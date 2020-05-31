;; -*- lexical-binding: t; -*-

(-each '(display-time-mode display-battery-mode)
  (fn: funcall <> t))

(defun vz/mode-line-file-state ()
  (if (buffer-file-name)
      (cond
       (buffer-read-only    " [!]")
       ((buffer-modified-p) " [+]")
       (:else               ""))
  ""))
  
(defun vz/mode-line-evil-state ()
  (cond
   ((eq evil-state 'visual) "    VISUAL » ")
   ((eq evil-state 'insert) "    INSERT » ")
   (:else                   "    ")))

;; From https://0x0.st/oYX8
(defun vz/mode-line-fill (face except)
  (propertize " "
        'display `((space :align-to (- (+ right right-fringe right-margin)
                                       ,except)))
        'face face))

(setq-default
 battery-update-interval 240
 mode-line-format `((:eval (vz/mode-line-evil-state))
                    "%b"
                    (:eval (vz/mode-line-file-state))
                    (:eval (vz/mode-line-fill 'mode-line 19))
                    "« "
                    (:eval (format-time-string "[%H:%M] "))
                    "["
                    (:eval (battery-format
                            "%b%p"
                            (funcall battery-status-function)))
                    "%%]"))
