;; -*- lexical-binding: t; -*-

;; * Basic configuration
;; ** Set path to shell

(setq explicit-shell-file-name (or (getenv "SHELL") "/bin/sh"))

;; ** Disable colours in shell-mode

(setf ansi-color-for-comint-mode 'filter)
(setq-default shell-font-lock-keywords nil
              comint-buffer-maximum-size 2000)

;; ** Track $PWD more effectively

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

;; ** Initialise

(defun vz/shell-mode-init ()
  (shell-dirtrack-mode nil)
  (add-hook 'comint-output-filter-function #'comint-truncate-buffer)
  (add-hook 'comint-output-filter-function #'comint-watch-for-password-prompt)
  (setq-local
   ;; comint-prompt-read-only t
   inhibit-field-text-motion t
   comint-process-echoes t)
  (setq Man-notify-method 'quiet)
  (add-hook 'comint-preoutput-filter-functions #'shell-sync-dir-with-prompt nil t))

(add-hook 'shell-mode-hook #'vz/shell-mode-init)

;; * Helper functions
;; ** Are we inside a shell?

;; TODO: Maybe we shouldn't consider nix-shell shells?

(defun vz/inside-shell? (&optional buffer)
  "Is the current ``active'' process a shell?"
  (let* ((buffer (or buffer (current-buffer)))
         (child  (process-running-child-p (get-buffer-process buffer))))
    (or (null child)
        (s-equals? (asoc-get (process-attributes child) 'comm)
                   (f-filename explicit-shell-file-name)))))

;; * Shell history tracking

;; TODO: Maybe track command run in nix-shell separately?

;; ** Variables

(defvar vz/shell-history-cache-file (~ ".cache/mksh_history.el")
  "Path to file to save elisp data about shell history")

;; ** Functions

(defun vz/shell-history--get ()
  "Get shell history"
  (if (f-exists? vz/shell-history-cache-file)
      (read-from-whole-string (f-read vz/shell-history-cache-file))
    '()))

(defun vz/shell-history--write (string)
  "Add STRING to `default-directory' entry"
  (let* ((string  (s-trim (substring-no-properties string)))
         (history (vz/shell-history--get))
         (oentry  (asoc-get history default-directory '())))
    (unless (s-blank? string)
      (-->
       (asoc-put! history default-directory
                  (cons string oentry)
                  t)
       (f-write (format "%S" it) 'utf-8 vz/shell-history-cache-file)))))

(defun vz/shell-history--sort (cwd)
  "Sort shell-history according to CWD"
  (-->
   (vz/shell-history--get)
   (append (asoc-get it cwd '())
           (->>
            it
            (asoc-filter (fn: not (s-equals? <> cwd)))
            (asoc-values)
            (-flatten)))))

(add-hook 'comint-input-filter-functions
          (defun vz/shell-history-input-hook (string)
            "Function to add to `comint-input-filter-functions'"
            (when (and (eq major-mode 'shell-mode)
                       (vz/inside-shell?))
              (vz/shell-history--write string))
            t))

(defun vz/shell-insert-from-hist ()
  "Search for command in history and run it"
  (interactive)
  (let ((process (get-buffer-process (current-buffer))))
    (when (vz/inside-shell?)
      (let* ((input  (comint-get-old-input-default))
             (iinput (unless (s-blank? input) (concat "^" input)))
             (cmd    (ivy-read "> "
                               (vz/shell-history--sort default-directory)
                               :initial-input iinput :sort nil)))
        (unless (s-blank? input)
          (comint-delete-input))
        (comint-send-string process (concat cmd "\n"))
        (comint-add-to-input-history cmd)))))
;;(vz/term-minor-mode-set-title cmd)))))

;; * History from mksh

(defun vz/shell-mksh-history ()
  "Returns current shell's history as a list"
  (call-process "mksh" nil nil nil "-ic" "fc -r -l -n 1 >/tmp/shhist")
  (split-string (f-read "/tmp/shhist") "\n" nil "\t"))

(defun vz/shell-insert-from-mksh-hist ()
  "Search for command in mksh history and run it"
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (when (vz/inside-shell?)
      (let* ((input (comint-get-old-input-default))
             (init-input (unless (string-empty-p input) (concat "^" input)))
             (cmd (ivy-read "> " (vz/shell-mksh-history)
                            :initial-input init-input :sort nil)))
        (unless (s-blank? input)
          (comint-delete-input))
        (comint-send-string proc (concat cmd "\n"))
        (comint-add-to-input-history cmd)
        ;;(vz/term-minor-mode-set-title cmd)
        (vz/shell-history--write cmd)))))

;; * Jump to directory alias

(defun vz/shell-get-dir-alias ()
  (call-process "mksh" nil nil nil "-ic" "alias -d >/tmp/diralias")
  (s-split "\n" (f-read "/tmp/diralias")))

(defun vz/shell-jump-to-dir ()
  "Jump to directory alias"
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
        (input (comint-get-old-input-default)))
    (when (vz/inside-shell?)
      (comint-delete-input)
      (let ((cmd (->>
                  (vz/shell-get-dir-alias)
                  (ivy-read "> ")
                  (s-split "=")
                  (car)
                  (format "cd ~%s\n"))))
        (comint-send-string proc cmd)
        (comint-add-to-input-history cmd))
      ;;(vz/term-minor-mode-set-title cmd))
      (unless (s-blank? input)
        (comint-send-string proc input)))))

;; * Popup a shell in `default-directory'
;; ** Variables

(defvar vz/popup-shells nil
  "List of all shell buffers opened by vz/popup-shell")

;; ** Functions

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
                           (and (vz/inside-shell? <>)
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

;; * Jump to prompt in ivy

(defun vz/shell--get-prompt (prompts point)
  "Return all prompts in an alist (prompt-string . point)"
  (save-excursion
    (goto-char point)
    (let ((pt (re-search-forward "^!?\\$ " nil t 1)))
      (if (null pt)
          ;; Does not include current "active" prompt
          (cdr prompts)
        (goto-char pt)
        (vz/shell--get-prompt
         (cons (->>
                (thing-at-point 'line t)
                (s-replace-regexp "\n$" "")
                (format "%2$s:%1$d" pt))
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
  (vz/beacon-highlight))

;; * Emacs frame as terminal

;; TODO: Auto-kill buffer by adding a function to `delete-frame-functions'

;; ** Variables

(define-minor-mode vz/term-minor-mode
  "Minor mode for binding C-d in *term* buffers")

(defvar vz/term-minor-mode--frame nil
  "Frame variable that *term-asdf* buffer uses")

(make-variable-buffer-local 'vz/term-minor-mode--frame)

;; ** Functions

(defun vz/term-minor-mode-sentinel (process output)
  "Process sentinel to auto kill associated buffer and frame in term-mode"
  (unless (process-live-p process)
    (let* ((b (process-buffer process))
           (f (asoc-get (buffer-local-variables b)
                        'vz/term-minor-mode--frame)))
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
     (set-frame-parameter vz/term-minor-mode--frame 'title))))

;; (add-hook 'comint-input-filter-functions #'vz/term-minor-mode-set-title)

;; * Keybindings

(defun vz/shell-send-keysequence-to-process (key)
  "Send keysequence to current running process"
  (interactive "sPress keysequence:")
  (comint-send-string (get-buffer-process (current-buffer)) key))

(general-nmap
  "SPC ps" #'vz/popup-shell)

(general-nmap
  :keymaps 'shell-mode-map
  "SPC j" #'vz/shell-jump-to-prompt
  "[/"    #'vz/shell-insert-from-hist)

(general-imap
  :keymaps 'shell-mode-map
  "C-k" #'vz/shell-send-keysequence-to-process
  "C-c" #'comint-interrupt-subjob
  "C-z" #'comint-stop-subjob
  "C-l" #'comint-clear-buffer
  "C-/" #'vz/shell-insert-from-hist
  "C-?" #'vz/shell-insert-from-mksh-hist
  "C-d" #'comint-send-eof
  "C-j" #'vz/shell-jump-to-dir)
