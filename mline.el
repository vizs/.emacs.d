(line-number-mode t)
(column-number-mode t)

(defun vz/mode-line-file-name ()
  (let ((file-name (buffer-file-name)))
    (cond ((not file-name) (buffer-name))
          (t (replace-regexp-in-string "/home/viz" "~" file-name)))))

(defun vz/mode-line-file-state ()
  (when (buffer-file-name)
    (cond (buffer-read-only "!")
          ((buffer-modified-p) "*")
          (t "")))
  "")

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
  (custom-set-faces
   `(header-line           ((t :background ,vz/color0 :foreground ,vz/color7)))
   `(header-line-highlight ((t :background ,vz/color0 :foreground ,vz/color15)))
   `(mode-line             ((t :background ,vz/color0 :foreground ,vz/color7)))
   `(mode-line-highlight   ((t :background ,vz/color0 :foreground ,vz/color15)))))

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

(when (vz/load-pkg "awesome-tray")
  (require 'awesome-tray)
  (defun vz/tray-line-column ()
    (format "L%s C%s"
            (format-mode-line "%l")
            (format-mode-line "%c")))

  (add-to-list 'awesome-tray-module-alist
               '("vz/loc" .
                 (vz/tray-line-column default)))

  (defun vz/tray-file-name ()
    (format "%s%s"
            (vz/mode-line-file-name)
            (vz/mode-line-file-state)))

  (add-to-list 'awesome-tray-module-alist
               '("vz/file" .
                 (vz/tray-file-name default)))

  (setq awesome-tray-active-modules '("vz/file" "vz/loc"))
  (setq-default header-line-format nil)
  (awesome-tray-mode 1))
