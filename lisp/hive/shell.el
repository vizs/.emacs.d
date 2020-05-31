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
        (unless (s-blank? input)
          (comint-delete-input))
        (comint-send-string proc (concat cmd "\n"))
        (comint-add-to-input-history cmd)
        (vz/term-minor-mode-set-title cmd)))))

(defun vz/shell-get-dir-alias ()
  (call-process "mksh" nil nil nil "-ic" "alias -d >/tmp/diralias")
  (s-split "\n" (vz/fread "/tmp/diralias")))

(defun vz/shell-jump-to-dir ()
  "Jump to directory alias"
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
        (input (comint-get-old-input-default)))
    (unless (process-running-child-p proc)
      (comint-delete-input)
      (let ((cmd (->>
                  (vz/shell-get-dir-alias)
                  (ivy-read "> ")
                  (s-split "=")
                  (car)
                  (format "cd ~%s\n"))))
        (comint-send-string proc cmd)
        (comint-add-to-input-history cmd)
        (vz/term-minor-mode-set-title cmd))
      (unless (s-blank? input)
        (comint-send-string proc input)))))

(defvar vz/popup-shells nil
  "List of all shell buffers opened by vz/popup-shell")

(defun vz/popup-shell--add (buffer)
  "Add BUFFER to vz/popup-shells and add a process sentinel"
  (add-to-list 'vz/popup-shells buffer)
  (set-process-sentinel
   (get-buffer-process buffer)
   (fn
    (unless (process-live-p <>)
      (let ((buf (process-buffer <>)))
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
      (unless (s-equals? input "$ ")
        (comint-send-string process input)))))

(defun vz/popup-shell ()
  "Try to find ``free'' buffers that have the CWD as `default-directory' and switch
to it. If nothing is found, create a new buffer"
  (interactive)
  (if (null vz/popup-shells)
      (vz/popup-shell--add (shell))
    (let* ((free-buffers (-filter
                          (fn
                           (and (null (process-running-child-p
                                       (get-buffer-process <>)))
                                (null (get-buffer-window <> t))))
                          vz/popup-shells))
           (cwd default-directory)
           (cwd-buffers (-filter
                         (fn:
                          with-current-buffer <>
                          (s-equals? default-directory cwd))
                         free-buffers)))
      (cond
       (cwd-buffers
        (vz/popup-shell--switch (car cwd-buffers) cwd t))
       ((and (null cwd-buffers) free-buffers)
        (vz/popup-shell--switch (car free-buffers) cwd))
       (:else
        (vz/popup-shell--add (shell (format "%s*"
                                            (make-temp-name "*shell-")))))))))

(defun vz/shell--get-prompt (prompts point)
  "Return all prompts in an alist (prompt-string . point)"
  (save-excursion
    (goto-char point)
    (let ((pt (re-search-forward "^$ " nil t 1)))
      (if (null pt)
          prompts
        (goto-char pt)
        (vz/shell--get-prompt
         (cons (-->
                (thing-at-point 'line t)
                (replace-regexp-in-string "\n$" "" it)
                (format "%s:%d" it pt))
               prompts)
         pt)))))

(defun vz/shell-jump-to-prompt ()
  "Jump to prompt by selecting it in ivy"
  (interactive)
  (->>
   (vz/shell--get-prompt '() (point-min))
   (ivy-read "> ")
   (s-split ":")
   (-last-item)
   (string-to-number)
   (goto-char))
  (when (fboundp 'beacon-blink)
    (let ((beacon-blink-duration 1))
      (beacon-blink))))

(define-minor-mode vz/term-minor-mode
  "Minor mode for binding ^D in *term* buffers")

(defvar vz/term-minor-mode--frame nil
  "Frame variable that *term* buffer uses")

(make-variable-buffer-local 'vz/term-minor-mode--frame)

(defun vz/term-minor-mode-sentinel (process output)
  "Process sentinel to auto kill associated buffer and frame in term-mode"
  (unless (process-live-p process)
    (let* ((b (process-buffer process))
           (f (alist-get 'vz/term-minor-mode--frame
                         (buffer-local-variables b))))
      (kill-buffer b)
      (when (frame-live-p f)
          (delete-frame f)))))

(defun vz/kill-dead-term ()
  "Remove all dead *term* buffers"
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (-each
        (-filter
         (fn:
          with-current-buffer <>
          (and (s-prefix? "*term-" (buffer-name <>))
               (or (not (frame-live-p vz/term-minor-mode--frame))
                   (not (get-buffer-process <>)))))
         (buffer-list))
      (fn: kill-buffer <>))))

(defun vz/term-minor-mode-set-title (string)
  "Set frame title when `vz/term-minor-mode' is active"
  (when (bound-and-true-p vz/term-minor-mode)
    (->>
     (s-trim-right string)
     (format "term: %s")
     (set-frame-parameter vz/term-minor-mode--frame 'title)))
  t)

(add-hook 'comint-input-filter-functions #'vz/term-minor-mode-set-title)

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
   comint-process-echoes t
   vz/jump-func #'vz/shell-jump-to-prompt)
  (setq Man-notify-method 'quiet)
  (add-hook 'comint-preoutput-filter-functions #'shell-sync-dir-with-prompt))

(add-hook 'shell-mode-hook #'vz/shell-mode-init)
