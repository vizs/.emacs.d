(line-number-mode t)
(column-number-mode t)

(defun vz/mode-line-file-name ()
  (let ((file-name (buffer-file-name)))
    (cond ((not file-name) (buffer-name))
          (t (replace-regexp-in-string "/home/viz" "~" file-name)))))

(defun vz/mode-line-file-state ()
  (when (buffer-file-name)
    (cond (buffer-read-only "!")
          ((buffer-modified-p) "*"))))

(setq vz/mode-line-format
      '("  "
        (:eval (vz/mode-line-file-name))
        (:eval (vz/mode-line-file-state))
        "     "
        (line-number-mode   "L%l")
        (column-number-mode " C%c" t)
        "     "
        mode-name))

(defun vz/set-mode-line-face ()
  (set-face-attribute 'header-line nil
                      :background vz/color0
                      :foreground vz/color7)
  (set-face-attribute 'header-line-highlight nil
                      :background vz/color0
                      :foreground vz/color15)
  (set-face-attribute 'mode-line  nil
                      :background vz/color0
                      :foreground vz/color7)
  (set-face-attribute 'mode-line-highlight nil
                      :background vz/color0
                      :foreground vz/color15))

(setq-default mode-line-format   nil
              header-line-format vz/mode-line-format)

(defun vz/toggle-header-line ()
  (interactive)
  (if (not header-line-format)
      (setq header-line-format vz/mode-line-format)
    (setq header-line-format nil)))

(defun vz/toggle-mode-line ()
  (interactive)
  (if (not mode-line-format)
      (setq mode-line-format vz/mode-line-format)
    (setq mode-line-format nil)))
