;; TODO: * Find out how to add functions to executable list thing
;;       * Find out how to expand directory aliases

(setq explicit-shell-file-name (or (getenv "SHELL") "/bin/sh"))

;; Disable colours in shell-mode
(setf ansi-color-for-comint-mode 'filter)
(setq-default
 shell-font-lock-keywords nil
 comint-buffer-maximum-size 2000)

;; from http://0x0.st/Hroa
(defun shell-sync-dir-with-prompt (string)
  "Set prompt to `|Pr0mPT|${PWD}|...'"
  (if (string-match "|Pr0mPT|\\([^|]*\\)|" string)
      (let ((cwd (match-string 1 string)))
        (setq default-directory
              (if (string-equal "/" (substring cwd -1))
                  cwd
                (setq cwd (concat cwd "/"))))
        (replace-match "" t t string 0))
    string))

(defun vz/shell-mode-init ()
  (dolist (h '(comint-truncate-buffer comint-watch-for-password-prompt))
  `(add-hook 'comint-output-filter-functions ,h))
  (shell-dirtrack-mode nil)
  (setq-local
   inhibit-field-text-motion t
   comint-process-echoes t) ;; Disables duplicates
  (setq Man-notify-method 'quiet)
  (add-hook 'comint-preoutput-filter-functions #'shell-sync-dir-with-prompt))

(defun vz/comint-send-input (&optional start end)
  "Send region if present, otherwise current line to current buffer's process"
  (interactive "r")
  (if (region-active-p)
      (comint-send-string
       (get-buffer-process (current-buffer))
       (concat (buffer-substring (or start (region-beginning))
                                 (or end (region-end))) "\n"))
    (comint-send-input))
  (when (evil-visual-state-p)
    (evil-exit-visual-state)))

(defun vz/shell-history ()
  "Returns current shell's history as a list"
  (call-process "mksh" nil nil nil "-ic" "fc -r -l -n 1 >/tmp/shhist")
  (split-string (vz/fread "/tmp/shhist") "\n" nil "\t"))

(defun vz/shell-insert-from-hist ()
  "Search for command in history and run it"
  (interactive)
  (let* ((input (concat (comint-get-old-input-default) " "))
         (cmd (ivy-read "> " (vz/shell-history) :initial-input input)))
    (unless (string= input " ")
      (comint-delete-input))
    (comint-send-string (get-buffer-process (current-buffer)) cmd)))

(defun vz/shell-get-dir-alias ()
  (call-process "mksh" nil nil nil "-ic" "alias -d >/tmp/diralias")
  (split-string (vz/fread "/tmp/diralias") "\n"))

(defun vz/shell-jump-to-dir ()
  "Jump to directory alias"
  (interactive)
  (comint-delete-input)
  (let ((dir (ivy-read "> " (vz/shell-get-dir-alias))))
    (comint-send-string
     (get-buffer-process (current-buffer))
     (format "cd ~%s\n" (car (split-string dir "="))))))

(defun vz/popup-shell ()
  "Open M-x shell in project's PWD"
  (interactive)
  (let ((shell-process (get-buffer-process "*shell*")))
    (if (and (get-buffer "*shell*") shell-process)
        (progn
          (comint-send-string shell-process (format "cd %s\n" default-directory))
          (switch-to-buffer-other-window "*shell*"))
      (shell))))

(define-minor-mode vz/term-mode
  "Minor mode for binding ^D in *term* buffers")

(defvar vz/term-mode--frame nil
  "Frame variable that *term* buffer uses")

(make-variable-buffer-local 'vz/term-mode--frame)

(defun vz/term-mode-sentinel (process output)
  "Process sentinel to auto kill associated buffer and frame in term-mode"
  (unless (process-live-p process)
    (let* ((buf (process-buffer process))
           (frame (alist-get 'vz/term-mode--frame (buffer-local-variables buf))))
      (kill-buffer buf)
      (when (frame-live-p frame)
          (delete-frame frame)))))

(defun vz/kill-dead-term ()
  "Remove all dead *term* buffers"
  (interactive)
  (dolist (buf (seq-filter #'(lambda (buf)
                               (and (string-prefix-p "*term-" (buffer-name buf))
                                    (not (get-buffer-process buf))))
                           (buffer-list)))
    (kill-buffer buf)))

(add-hook 'shell-mode-hook #'vz/shell-mode-init)

(general-nmap
  :prefix "SPC"
  "ps" #'vz/popup-shell)

(general-nmap
  :keymaps 'comint-mode-map
  "<RET>" #'vz/comint-send-input)

(general-nmap
  :keymaps 'comint-mode-map
  :prefix "["
  "w" #'comint-write-output
  "d" #'comint-delete-output
  "j" #'comint-next-prompt
  "k" #'comint-previous-prompt
  "c" #'comint-clear-buffer)

(general-imap
  :keymaps 'comint-mode-map
  "<S-return>" #'comint-accumulate)

(general-nmap
  :keymaps 'shell-mode-map
  "SPC j" #'vz/shell-jump-to-dir
  "[/" #'vz/shell-insert-from-hist)

(general-imap
  :keymaps 'shell-mode-map
  "C-c" #'comint-interrupt-subjob
  "C-z" #'comint-stop-subjob
  "C-l" #'comint-clear-buffer
  "C-/" #'vz/shell-insert-from-hist
  "C-d" #'comint-send-eof
  "C-j" #'vz/shell-jump-to-dir)

(general-vmap
  :keymaps 'comint-mode-map
  "<RET>" #'vz/comint-send-input)
