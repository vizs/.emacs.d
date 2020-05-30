;; -*- lexical-binding: t; -*-

(setq explicit-shell-file-name (or (getenv "SHELL") "/bin/sh"))

;; Disable colours in shell-mode
(setf ansi-color-for-comint-mode 'filter)
(setq-default shell-font-lock-keywords nil
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

(defun vz/shell-history ()
  "Returns current shell's history as a list"
  (call-process "mksh" nil nil nil "-ic" "fc -r -l -n 1 >/tmp/shhist")
  (split-string (vz/fread "/tmp/shhist") "\n" nil "\t"))

(defun vz/shell-insert-from-hist ()
  "Search for command in history and run it"
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (unless (process-running-child-p proc)
      (let* ((input (comint-get-old-input-default))
             (init-input (unless (string-empty-p input) (concat "^" input)))
             (cmd (ivy-read "> " (vz/shell-history) :initial-input init-input)))
        (unless (string-empty-p input)
          (comint-delete-input))
        (comint-send-string proc (concat cmd "\n"))
        (comint-add-to-input-history cmd)))))

(defun vz/shell-get-dir-alias ()
  (call-process "mksh" nil nil nil "-ic" "alias -d >/tmp/diralias")
  (split-string (vz/fread "/tmp/diralias") "\n"))

(defun vz/shell-jump-to-dir ()
  "Jump to directory alias"
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (unless (process-running-child-p proc)
      (comint-delete-input)
      (let* ((dir (ivy-read "> " (vz/shell-get-dir-alias)))
             (cmd (format "cd ~%s\n" (car (split-string dir "=")))))
        (comint-send-string proc cmd)
        (comint-add-to-input-history cmd)))))

(defvar vz/popup-shells nil
  "List of all shell buffers opened by vz/popup-shell")

(defun vz/popup-shell--add (buffer)
  "Add BUFFER to vz/popup-shells and add a process sentinel"
  (add-to-list 'vz/popup-shells buffer)
  (set-process-sentinel (get-buffer-process buffer)
                        #'(lambda (process output)
                            (unless (process-live-p process)
                              (let ((buf (process-buffer process)))
                                (setq vz/popup-shells (remove buf vz/popup-shells))
                                (kill-buffer buf))))))

(defun vz/popup-shell--switch (buffer cwd &optional dont-cd?)
  "Switch to CWD in BUFFER"
  (switch-to-buffer-other-window buffer)
  (unless dont-cd?
    (let ((input (comint-get-old-input-default))
          (process (get-buffer-process buffer)))
      (comint-delete-input)
      (comint-send-string process (format "cd %s\n" cwd))
      (unless (string= input "$ ")
        (comint-send-string process input)))))

(defun vz/popup-shell ()
  "Try to find ``free'' buffers that have the CWD as `default-directory' and switch
to it. If nothing is found, create a new buffer"
  (interactive)
  (if (null vz/popup-shells)
      (vz/popup-shell--add (shell))
    (let* ((free-buffers (seq-filter #'(lambda (buffer)
                                         (and (null (process-running-child-p
                                                     (get-buffer-process buffer)))
                                              (null (get-buffer-window buffer t))))
                                     vz/popup-shells))
           (cwd default-directory)
           (cwd-buffers (seq-filter #'(lambda (buffer)
                                        (with-current-buffer buffer
                                          (string= default-directory cwd)))
                                    free-buffers)))
      (cond
       (cwd-buffers
        (vz/popup-shell--switch (car cwd-buffers) cwd t))
       ((and (null cwd-buffers) free-buffers)
        (vz/popup-shell--switch (car free-buffers) cwd))
       (:else
        (vz/popup-shell--add (shell (format "%s*"
                                            (make-temp-name "*shell-")))))))))

(define-minor-mode vz/term-mode
  "Minor mode for binding ^D in *term* buffers")

(defvar vz/term-mode--frame nil
  "Frame variable that *term* buffer uses")

(make-variable-buffer-local 'vz/term-mode--frame)

(defun vz/term-mode-sentinel (process output)
  "Process sentinel to auto kill associated buffer and frame in term-mode"
  (unless (process-live-p process)
    (let* ((b (process-buffer process))
           (f (alist-get 'vz/term-mode--frame (buffer-local-variables b))))
      (kill-buffer b)
      (when (frame-live-p f)
          (delete-frame f)))))

(defun vz/kill-dead-term ()
  "Remove all dead *term* buffers"
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (dolist (buf (seq-filter
                  #'(lambda (buf)
                      (with-current-buffer buf
                        (and (string-prefix-p "*term-" (buffer-name buf))
                             (or (not (frame-live-p vz/term-mode--frame))
                                 (not (get-buffer-process buf))))))
                  (buffer-list)))
      (kill-buffer buf))))

(general-nmap
  "SPC ps" #'vz/popup-shell)

(general-nmap
  :keymaps 'shell-mode-map
  "SPC j" #'vz/shell-jump-to-dir
  "[/"    #'vz/shell-insert-from-hist)

(general-imap
  :keymaps 'shell-mode-map
  "C-c" #'comint-interrupt-subjob
  "C-z" #'comint-stop-subjob
  "C-l" #'comint-clear-buffer
  "C-/" #'vz/shell-insert-from-hist
  "C-d" #'comint-send-eof
  "C-j" #'vz/shell-jump-to-dir)

(defun vz/shell-mode-init ()
  (add-hook 'comint-output-filter-function #'comint-truncate-buffer)
  (add-hook 'comint-output-filter-function #'comint-watch-for-password-prompt)
  (shell-dirtrack-mode nil)
  (setq-local
   ;; comint-prompt-read-only t
   inhibit-field-text-motion t
   comint-process-echoes t)
  (setq Man-notify-method 'quiet)
  (add-hook 'comint-preoutput-filter-functions #'shell-sync-dir-with-prompt))

(add-hook 'shell-mode-hook #'vz/shell-mode-init)
