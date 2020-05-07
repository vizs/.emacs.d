;; NOTE: * you can see if you are using comint-mode if you check INSIDE_EMACS
;;       * man is significantly faster than woman

;; TODO: get mksh history synced

(setq explicit-shell-file-name (or (getenv "SHELL") "/bin/sh"))

;; Disable colours in shell-mode
(setf ansi-color-for-comint-mode 'filter)
(setq-default
 shell-font-lock-keywords nil
 comint-buffer-maximum-size 2000)

(defun vz/shell-mode-init ()
  (dolist (h '(comint-truncate-buffer comint-watch-for-password-prompt))
	`(add-hook 'comint-output-filter-functions ,h))
  (shell-dirtrack-mode nil)
  (setq
   comint-process-echoes t ;; Disables duplicates
   Man-notify-method 'quiet))

(defun vz/comint-send-input (&optional start end)
  "Send region if present, otherwise current line to current buffer's process"
  (interactive "r")
  (if (use-region-p)
	  (comint-send-string
	   (get-buffer-process (current-buffer))
	   (concat (buffer-substring (or start (region-beginning))
								 (or end (region-end)))
			   "\n"))
	(comint-send-input))
  (when (evil-visual-state-p)
	(evil-exit-visual-state)))

;; Disable duplicates
(add-hook 'shell-mode-hook #'vz/shell-mode-init)

;; TODO: consider changing this to comint-mode-map?
(general-nmap
  :keymaps 'shell-mode-map
  "<RET>" 'vz/comint-send-input
  :prefix "["
  "w" 'comint-write-output
  "d" 'comint-delete-output
  "j" 'comint-next-prompt
  "k" 'comint-previous-prompt
  "c" 'comint-clear-buffer)

(general-imap
  :keymaps 'shell-mode-map
  "C-c" 'comint-interrupt-subjob
  "C-z" 'comint-stop-subjob
  "C-l" 'comint-clear-buffer)

(general-vmap
  :keymaps 'shell-mode-map
  "<RET>" 'vz/comint-send-input)
