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

;; (setq vz/mode-line-format
;;       '("  "
;;         (:eval (vz/mode-line-file-name))
;;         (:eval (vz/mode-line-file-state))
;;         "     "
;;         (line-number-mode   "L%l")
;;         (column-number-mode " C%c" t)
;;         "     "
;;         mode-name))

(setq vz/mode-line-format '((:eval (vz/mode-line-file-name))))

(defun vz/set-mode-line-face ()
  (custom-set-faces
   `(header-line           ((t :background ,vz/color0 :foreground ,vz/color7)))
   `(header-line-highlight ((t :background ,vz/color8 :foreground ,vz/color15)))
   `(mode-line             ((t :background ,vz/color0 :foreground ,vz/color7)))
   `(mode-line-highlight   ((t :background ,vz/color0 :foreground ,vz/color15)))))

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


(cond
 ;; awesome-tray
 ((= vz/tray 1)
  (when (vz/load-pkg "awesome-tray")
    (require 'awesome-tray)

    (defun vz/tray-line-column ()
      (format-mode-line "   %p   L%l   C%c"))

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
    (setq-default header-line-format nil
                  mode-line-format   nil)
    (awesome-tray-mode)))

 ;; header-line
 ((= vz/tray 2)
  (setq-default header-line-format vz/mode-line-format
                mode-line-format   nil))

 ;; if anything else, don't display {mode,head}-line or awesome-tray
 (t
  (setq-default header-line-format nil
                mode-line-format   nil)))
