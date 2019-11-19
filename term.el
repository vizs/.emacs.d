(use-package vterm
  :config
  (custom-set-faces
   `(vterm-color-default ((t :foreground ,vz/color7 :background ,vz/color0)))
   `(vterm-color-black   ((t :foreground ,vz/color7 :background ,vz/color0)))
   `(vterm-color-red     ((t :foreground ,vz/color7 :background ,vz/color7)))
   `(vterm-color-green   ((t :foreground ,vz/color7 :background ,vz/color7)))
   `(vterm-color-yellow  ((t :foreground ,vz/color7 :background ,vz/color7)))
   `(vterm-color-blue    ((t :foreground ,vz/color7 :background ,vz/color7)))
   `(vterm-color-magenta ((t :foreground ,vz/color7 :background ,vz/color7)))
   `(vterm-color-cyan    ((t :foreground ,vz/color7 :background ,vz/color7)))
   `(vterm-color-white   ((t :foreground ,vz/color7 :background ,vz/color7))))

  (defun vz/term-switch ()
    (interactive)
    (let ((vterm-buf (get-buffer "vterm")))
      (if (member vterm-buf (buffer-list))
          (switch-to-buffer-other-window vterm-buf)
        (vterm-other-window))))

  (vz/bind-norm
   "SPC T"      'vz/term-switch))
