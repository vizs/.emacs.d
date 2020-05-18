;; NOTE: * you can see if you are using comint-mode if you check INSIDE_EMACS
;;       * man is significantly faster than woman

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
  (when (file-regular-p "/tmp/shhist")
    (delete-file "/tmp/shhist"))
  (call-process "mksh" nil nil nil "-ic" "fc -r -l -n 1 >/tmp/shhist")
  (split-string (vz/fread "/tmp/shhist") "\n" nil "\t"))

(defun vz/shell-insert-from-hist ()
  "Search for command in history and run it"
  (interactive)
  (let ((input (comint-get-old-input-default)))
    (unless (string-empty-p input)
      (comint-delete-input))
    (comint-send-string
     (get-buffer-process (current-buffer))
     (concat (ivy-read "> " (vz/shell-history) :initial-input input) "\n"))))

(defun vz/popup-shell ()
  "Open M-x shell in project's PWD"
  (interactive)
  (when (get-buffer "*shell*")
    (comint-send-string (get-buffer-process "*shell*")
                        (format "cd %s\n" default-directory))
    (switch-to-buffer-other-window "*shell*"))
  (shell))

(define-minor-mode vz/term-mode
  "Minor mode for binding ^D in *term* buffers")

(defun vz/term-mode-sentinel (process output)
  "Process sentinel to auto kill associated buffer and frame in term-mode"
  (unless (process-live-p process)
    (kill-buffer (process-buffer process))
    (delete-frame vz/term-mode--frame)))

(defun vz/term-mode-kill-dead ()
  "Remove all dead buffers"
  (interactive)
  (dolist (buf
           (seq-filter
            #'(lambda (buf) (and (string-prefix-p "*term-" (buffer-name buf))
                                 (not (get-buffer-process buf))))
            (buffer-list)))
    (kill-buffer buf)))

(add-hook 'shell-mode-hook #'vz/shell-mode-init)

(general-nmap
  :prefix "SPC"
  "ps" #'vz/popup-shell)

(general-nmap :keymaps 'comint-mode-map
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
  :prefix "["
  "/" #'vz/shell-insert-from-hist)

(general-imap
  :keymaps 'shell-mode-map
  "C-c" #'comint-interrupt-subjob
  "C-z" #'comint-stop-subjob
  "C-l" #'comint-clear-buffer
  "C-/" #'vz/shell-insert-from-hist
  "C-d" #'comint-send-eof)

(general-vmap
  :keymaps 'comint-mode-map
  "<RET>" #'vz/comint-send-input)
