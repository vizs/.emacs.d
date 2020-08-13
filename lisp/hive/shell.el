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
        (f-equal? (asoc-get (process-attributes child) 'comm)
                   (f-filename explicit-shell-file-name)))))

;; * Shell history tracking

;; TODO: Maybe track command run in nix-shell separately?

;; ** Variables

(defvar vz/shell-history-cache-file (~ ".cache/mksh_history.el")
  "Path to file to save elisp data about shell history")

;; ** Functions

(defun vz/shell-history--get ()
  "Get shell history."
  (if (f-exists? vz/shell-history-cache-file)
      (read-from-whole-string (f-read vz/shell-history-cache-file))
    '()))

(defun vz/shell-history--write (string)
  "Add STRING to `default-directory' entry."
  (let ((string  (s-trim (substring-no-properties string)))
        (history (vz/shell-history--get)))
    (unless (s-blank? string)
      (f-write (format "%S"
                       (asoc-put! history
                                  default-directory
                                  (cons string
                                        (asoc-get history default-directory '()))
                                  t))
               'utf-8 vz/shell-history-cache-file))))

(defun vz/shell-history--sort (cwd)
  "Sort shell-history according to CWD."
  (let ((history (vz/shell-history--get)))
    (append (asoc-get history cwd '())
            (-flatten (asoc-filter (fn (not (f-equal? <> cwd)))
                                   (asoc-values history))))))

(add-hook 'comint-input-filter-functions
          (defun vz/shell-history-input-hook (string)
            "Function to add to `comint-input-filter-functions'"
            (when (and (eq major-mode 'shell-mode)
                       (vz/inside-shell?))
              (vz/shell-history--write string))
            t))

(defun vz/shell-insert-from-hist ()
  "Search for command in history and run it."
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
;; (vz/term-minor-mode-set-title cmd)))))

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
  (-map (fn (cadr (s-split "=" <>)))
        (-drop-last
         1
         (s-split "\n" (f-read "/tmp/diralias")))))

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
                  (format "cd %s\n"))))
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

(defun vz/popup-shell--add (buffer &optional cwd)
  "Add BUFFER to vz/popup-shells and add a process sentinel.
If CWD is non-nil, then cd to CWD."
  (add-to-list 'vz/popup-shells buffer)
  (set-process-sentinel
   (get-buffer-process buffer)
   (fn (unless (process-live-p <>)
         (let ((buf (process-buffer <>)))
           (setq vz/popup-shells (remove buf vz/popup-shells))
           (kill-buffer buf)))))
  (when cwd
    (with-current-buffer buffer
      (comint-send-string (get-buffer-process buffer)
                          (format "cd %s\n" cwd)))))

(defun vz/popup-shell--switch (buffer cwd &optional dont-cd?)
  "Switch to CWD in BUFFER"
  (switch-to-buffer-other-window buffer)
  (unless dont-cd?
    (let ((input (comint-get-old-input-default))
          (process (get-buffer-process buffer)))
      (comint-delete-input)
      (comint-send-string process (format "cd %s\n" cwd))
      (unless (s-matches? input (rx (= 1 (or "$" "#" "%" "μ"))))
        (comint-send-string process input)))))

(defun vz/popup-shell ()
  "Try to find ``free'' buffers that have the CWD as `default-directory' and switch
to it. If nothing is found, create a new buffer"
  (interactive)
  (let ((cwd (condition-case nil
                  (f-dirname (buffer-file-name))
                (error default-directory))))
    (if (null vz/popup-shells)
        (vz/popup-shell--add (shell) cwd)
      (let* ((free-buffers (-filter
                            (fn (and (vz/inside-shell? <>)
                                     (null (get-buffer-window <> t))))
                            vz/popup-shells))
             (cwd-buffers (-filter
                           (fn (with-current-buffer <>
                                 (f-equal? default-directory cwd)))
                           free-buffers)))
        (cond
         (cwd-buffers
          (vz/popup-shell--switch (car cwd-buffers) cwd t))
         ((and (null cwd-buffers) free-buffers)
          (vz/popup-shell--switch (car free-buffers) cwd))
         (t
          (vz/popup-shell--add (shell (vz/uniqify "*shell")) cwd)))))))

;; * Jump to prompt

(defun vz/shell--get-prompts (prompts point)
  "Return all prompts as a propertized string"
  (save-excursion
    (goto-char point)
    (if-let ((pt (re-search-forward (rx (zero-or-one "!")
                                        (= 1 (or "$" "μ" "#" "%"))
                                        (0+ any)
                                        eol)
                                    nil t 1)))
        (vz/shell--get-prompts (cons (propertize
                                    (s-replace-regexp "\n$" "" (thing-at-point 'line t))
                                    'pos pt) prompts)
                             pt)
      (cdr prompts))))

(defun vz/shell-jump-to-prompt ()
  "Jump to prompt by selecting it in ivy. The most recent is towards the top."
  (interactive)
  (->>
   (ivy-read "> " (vz/shell--get-prompts '() (point-min))
             :sort nil)
   (get-text-property 0 'pos)
   (goto-char))
  (vz/beacon-highlight))

;; * Emacs frame as terminal
;; ** Variables

(define-minor-mode vz/term-minor-mode
  "Minor mode for binding C-d in *term* buffers")

(defvar vz/term-minor-mode-frame nil
  "Frame variable that *term-asdf* buffer uses")

(make-variable-buffer-local 'vz/term-minor-mode-frame)

(defvar vz/term-minor-mode-frames nil
  "Frames used by *term-asdf* buffers")

;; ** Functions

(defun vz/term-minor-mode-sentinel (process output)
  "Process sentinel to auto kill associated buffer and frame in term-mode"
  (unless (process-live-p process)
    (let* ((b (process-buffer process))
           (f (asoc-get (buffer-local-variables b)
                        'vz/term-minor-mode-frame)))
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
               (or (not (frame-live-p vz/term-minor-mode-frame))
                   (not (get-buffer-process <>)))))
         (buffer-list))
      #'kill-buffer)))

(defun vz/term-minor-mode-set-title (string)
  "Set frame title when `vz/term-minor-mode' is active"
  (when (bound-and-true-p vz/term-minor-mode)
    (->>
     (s-trim-right string)
     (format "term: %s")
     (set-frame-parameter vz/term-minor-mode-frame 'title))))

;; (add-hook 'comint-input-filter-functions #'vz/term-minor-mode-set-title)

;; `term-buffer' is set to the name of the shell buffer when the frame is created.
;; `term-buffer' is a frame parameter.

(defun vz/term-minor-mode-on-delete-frame (frame)
  "If FRAME is a member of `vz/term-minor-mode-frames', then kill the
term buffer associated with it"
  (when (member frame vz/term-minor-mode-frames)
    (setq vz/term-minor-mode-frames (remove frame vz/term-minor-mode-frames))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer (frame-parameter frame 'term-buffer)))))

(add-hook 'delete-frame-functions #'vz/term-minor-mode-on-delete-frame)

;; * Keybindings

(defun vz/shell-send-keysequence-to-process (key)
  "Send keysequence to current running process"
  (interactive "sPress keysequence:")
  (comint-send-string (get-buffer-process (current-buffer)) key))

(vz/bind
 "C-c p" #'vz/popup-shell
 :map shell-mode-map
 "C-c j" #'vz/shell-jump-to-dir
 "C-c ?" #'vz/shell-insert-from-mksh-hist
 "C-c /" #'vz/shell-insert-from-hist
 "C-c J" #'vz/shell-jump-to-prompt)

;; Local Variables:
;; eval: (outline-minor-mode)
;; outline-regexp: ";; [*]+"
;; End:
