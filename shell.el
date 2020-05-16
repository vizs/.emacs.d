;; NOTE: * you can see if you are using comint-mode if you check INSIDE_EMACS
;;       * man is significantly faster than woman

;; TODO: get mksh history synced

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
   comint-process-echoes t ;; Disables duplicates
   Man-notify-method 'quiet)
  (add-hook 'comint-preoutput-filter-functions #'shell-sync-dir-with-prompt))

(defun vz/comint-send-input (&optional start end)
  "Send region if present, otherwise current line to current buffer's process"
  (interactive "r")
  (if (use-region-p)
    (comint-send-string
     (get-buffer-process (current-buffer))
     (concat (buffer-substring (or start (region-beginning))
                               (or end (region-end))) "\n"))
  (comint-send-input))
  (when (evil-visual-state-p)
  (evil-exit-visual-state)))

(add-hook 'shell-mode-hook #'vz/shell-mode-init)

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
  :keymaps #'comint-mode-map
  "<S-return>" #'comint-accumulate)

(general-imap
  :keymaps 'shell-mode-map
  "C-c" #'comint-interrupt-subjob
  "C-z" #'comint-stop-subjob
  "C-l" #'comint-clear-buffer)

(general-vmap
  :keymaps #'comint-mode-map
  "<RET>" #'vz/comint-send-input)
