;; File with dynamic binding for vz/eval-file

(defun vz/eval-file (path stdin args)
  "Evaluate elisp file in path"
  (eval (read-from-whole-string (f-read-text path))))
