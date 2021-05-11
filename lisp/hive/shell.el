;; -*- lexical-binding: t; -*-

;; TODO: Fontify input to shell process that have been written by
;; editing the output from the shell process.
;;
;; TODO: Completely remove the need for prompt and fontify input
;; differently when in nix-shell

;; * Basic configuration
;; ** Set path to shell
(setq explicit-shell-file-name (or (getenv "SHELL") "/bin/sh"))

;; ** Disable colours in shell-mode
(setf ansi-color-for-comint-mode 'filter)
(setq-default shell-font-lock-keywords nil
              comint-buffer-maximum-size 2000)

;; ** Add doas prompt to password regexp
(when (< emacs-major-version 28)
  (setq comint-password-prompt-regexp
        (format "%s\\|%s"
                comint-password-prompt-regexp
                "doas (.*) password: ")))

;; ** Track $PWD more effectively
;; from http://0x0.st/Hroa
;; (defun shell-sync-dir-with-prompt (string)
;;   "Set prompt to `|Pr0mPT|${PWD}|...'"
;;   (if (string-match "|Pr0mPT|\\([^|]*\\)|" string)
;;       (let ((cwd (match-string 1 string)))
;;         (setq default-directory
;;               (if (string-equal "/" (substring cwd -1))
;;                   cwd
;;                 (setq cwd (concat cwd "/"))))
;;         (replace-match "" t t string 0))
;;     string))

;; ** Initialise
(defun vz/shell-mode-init ()
  (shell-dirtrack-mode nil)
  (add-hook 'comint-output-filter-function #'comint-truncate-buffer)
  (add-hook 'comint-output-filter-function #'comint-watch-for-password-prompt)
  (setq-local inhibit-field-text-motion t
              comint-process-echoes t))

(add-hook 'shell-mode-hook #'vz/shell-mode-init)

;; * Helper functions
;; ** Are we inside a shell?
(defun vz/inside-shell? (&optional buffer)
  "Is the current ``active'' process a shell?"
  (let ((child  (process-running-child-p (get-buffer-process
                                          (or buffer (current-buffer))))))
    (or (null child)
        (s-equals? (asoc-get (process-attributes child) 'comm)
                   (f-base explicit-shell-file-name)))))

;; ** Ivy action for running commands from shell history
(defun vz/shell-history--ivy-action (command)
  (comint-delete-input)
  (comint-send-string (get-buffer-process (current-buffer))
                      (concat command "\n"))
  (comint-add-to-input-history command)
  (vz/shell-history--write command)
  (vz/eterm--set-title command))

;; * Shell history tracking
;; ** Variables
(defvar vz/shell-history-cache-file (~ ".cache/shell_history.el")
  "Path to file to save elisp data about shell history")

;; ** Functions
(defun vz/shell-history--get ()
  "Get shell history hashtable."
  (if (not (f-exists? vz/shell-history-cache-file))
      (make-hash-table :test #'equal :size 10000)
    (read-from-whole-string (f-read vz/shell-history-cache-file
                                    'utf-8))))

(defun vz/shell-history--write (entry &optional directory)
  "Prepend ENTRY to DIRECTORY key's value."
  (let ((entry (s-trim (substring-no-properties entry)))
        (directory (or directory default-directory))
        (ht (vz/shell-history--get))
        ;; From straight.el
        (print-level nil)
        (print-length nil))
    (unless (s-blank? entry)
      (puthash directory
               (cons entry (gethash directory ht))
               ht)
      (f-write (prin1-to-string ht) 'utf-8 vz/shell-history-cache-file))))

(defun vz/shell-history--sort (dir)
  "Sort shell history according to DIR."
  (let* ((ht (vz/shell-history--get))
         (his (gethash dir ht)))
    (dolist (d (hash-table-keys ht))
      (unless (equal d dir)
        (setq his (append his (gethash d ht)))))
    his))

(defun vz/shell-insert-from-hist ()
  (interactive)
  (when-let ((proc (get-buffer-process (current-buffer)))
             (_ (vz/inside-shell?)))
    (ivy-read
     "> " (vz/shell-history--sort default-directory)
     :preselect (comint-get-old-input-default)
     :sort nil
     :action #'vz/shell-history--ivy-action)))

(add-hook 'comint-input-filter-functions
          (defun vz/shell-history--input-hook (string)
            "Add string to shell history."
            (when (and (derived-mode-p 'shell-mode)
                       (vz/inside-shell?))
              (vz/shell-history--write string))
            t))

(defun vz/shell-shell-history ()
  "Returns current shell's history as a list"
  (with-temp-buffer
    (call-process "mksh" nil (list (current-buffer) nil) t "-ic" "fc -r -l -n 1")
    (split-string (buffer-substring (point-min) (point-max)) "\n" nil "\t")))

;; * History from shell
(defun vz/shell-insert-from-shell-hist ()
  "Search for command in shell history and run it"
  (interactive)
  (when-let ((proc (get-buffer-process (current-buffer)))
             (_ (vz/inside-shell?)))
    (ivy-read
     "> " (vz/shell-shell-history)
     :preselect (comint-get-old-input-default)
     :sort nil
     :action #'vz/shell-history--ivy-action)))

;; * Jump to directory alias
(defun vz/shell-get-dir-alias ()
  (with-temp-buffer
    (call-process "mksh" nil (list (current-buffer) nil) t "-ic" "alias -d")
    (split-string (buffer-substring (point-min) (point-max))
                  "\n" nil (rx alnum "="))))

(defun vz/shell-jump-to-dir ()
  "Read directory from mksh's directory alias list and invoke
`read-directory-name' with the selected directory as input."
  (interactive)
  (when-let ((input (comint-get-old-input-default))
             (_ (vz/inside-shell?)))
    (vz/shell-history--ivy-action
     (concat "cd " (shell-quote-argument
                    (expand-file-name (read-directory-name "Go to: "
                                          (completing-read "Jump to: " (cons "." (vz/shell-get-dir-alias))))))))
    (comint-send-string (get-buffer-process (current-buffer)) input)))

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
   (lambda (p)
     (unless (process-live-p p)
       (let ((buf (process-buffer p)))
         (setq vz/popup-shells (remove buf vz/popup-shells))
         (kill-buffer buf)))))
  (when cwd
    (with-current-buffer buffer
      (comint-send-string (get-buffer-process buffer)
                          (format "cd %s\n" (shell-quote-argument cwd))))))

(defun vz/popup-shell--switch (buffer cwd &optional dont-cd?)
  "Switch to CWD in BUFFER"
  (switch-to-buffer-other-window buffer)
  (unless dont-cd?
    (let ((input (comint-get-old-input-default))
          (process (get-buffer-process buffer)))
      (comint-delete-input)
      (comint-send-string process (format "cd %s\n" (shell-quote-argument cwd)))
      (unless (s-matches? input (rx (= 1 (or "$" "#" "%" "μ"))))
        (comint-send-string process input)))))

(defun vz/popup-shell ()
  "If the frame is a terminal frame, then open the associated
shell buffer otherwise try to find ``free'' buffers that have the
CWD as `default-directory' and switch to it. If nothing is found,
create a new buffer."
  (interactive)
  (if (seq-contains-p vz/eterm--frames (selected-frame))
      (switch-to-buffer-other-window (frame-parameter (selected-frame) 'term-buffer))
    (let ((cwd (condition-case nil
                   (f-dirname (buffer-file-name))
                 (error (f-full default-directory)))))
      (if (null vz/popup-shells)
          (vz/popup-shell--add (shell) cwd)
        (let* ((free-buffers (seq-filter (lambda (buf) (and (vz/inside-shell? buf)
                                                            (null (get-buffer-window buf t))))
                                         vz/popup-shells))
               (cwd-buffers (seq-filter (lambda (buf) (with-current-buffer buf
                                                        (f-equal? default-directory cwd)))
                                        free-buffers)))
          (cond
           (cwd-buffers
            (vz/popup-shell--switch (car cwd-buffers) cwd t))
           ((and (null cwd-buffers) free-buffers)
            (vz/popup-shell--switch (car free-buffers) cwd))
           (t
            (vz/popup-shell--add (shell (vz/uniqify "*shell")) cwd))))))))

;; * Jump to prompt
(defvar-local vz/shell--prompt-alist nil
  "An alist of prompt string and its position in buffer.")

(defun vz/shell-clear-buffer ()
  "Just like `comint-clear-buffer' except it also sets
`vz/shell--prompt-alist' to nil."
  (interactive)
  (comint-clear-buffer)
  (setq-local vz/shell--prompt-alist nil))

(add-hook 'comint-input-filter-functions
          (defun vz/shell--prompt-add-hook (prompt)
            (when (and (derived-mode-p 'shell-mode)
                       (vz/inside-shell?))
              (setq-local vz/shell--prompt-alist
                          (cons `(,(s-trim-right prompt) . ,(1- (point))) vz/shell--prompt-alist)))
            t))

(defun vz/shell-jump-to-prompt ()
  "Jump to prompt by selecting it in ivy."
  (interactive)
  (ivy-read "> " vz/shell--prompt-alist
            :sort nil
            :action (lambda (p)
                      (goto-char (cdr p))
                      (vz/beacon-highlight))))

;; * Emacs frame as a terminal
;; ** Variables
(defvar-local vz/eterm--frame nil
  "Frame assigned to eterm buffer.")

(defvar vz/eterm--frames nil
  "List of frames assinged to eterm buffers.")

;; ** Functions
(defun vz/eterm--sentinel (process _output)
  "Process sentinel to auto kill associated buffer of eterm
frames."
  (unless (process-live-p process)
    (let* ((b (process-buffer process))
           (f (asoc-get (buffer-local-variables b)
                        'vz/eterm--frame))
           (kill-buffer-query-functions nil))
      (kill-buffer b)
      (when (frame-live-p f)
        (delete-frame f)))))

(defun vz/kill-dead-term ()
  "Remove all dead eterm buffers."
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (seq-each
     (seq-filter
      (lambda (x) (with-current-buffer x
                    (and (s-prefix? "*term-" (buffer-name x))
                         (or (not (frame-live-p vz/eterm--frame))
                             (not (get-buffer-process x))))))
      (buffer-list))
     #'kill-buffer)))

(defun vz/eterm--set-title (string)
  "Set frame title for eterm frames."
  (set-frame-parameter vz/eterm--frame 'title
                       (format "term@%s: %s" default-directory (s-trim-right string))))

(add-hook 'comint-input-filter-functions #'vz/eterm--set-title)

;; `term-buffer' is set to the name of the shell buffer when the frame is created.
;; `term-buffer' is a frame parameter.

(defun vz/eterm--on-delete-frame (frame)
  "If FRAME is a member of `vz/eterm--frames', then kill the
shell buffer associated with it"
  (when (and (member frame vz/eterm--frames)
                                        ;(vz/inside-shell? (frame-parameter frame 'term-buffer))
             )
    (setq vz/eterm--frames (remove frame vz/eterm--frames))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer (frame-parameter frame 'term-buffer)))))

(add-hook 'delete-frame-functions #'vz/eterm--on-delete-frame)

;; * Keybindings
(defun vz/shell-send-keysequence-to-process (key)
  "Send keysequence to current running process"
  (interactive "sPress keysequence:")
  (comint-send-string (get-buffer-process (current-buffer)) key))

(defun vz/shell-quote-region-or-till-eol ()
  "Shell quote the region or from point to EOL."
  (interactive)
  (insert
   (shell-quote-argument
    (if (region-active-p)
        (delete-and-extract-region (region-beginning) (region-end))
      (delete-and-extract-region (point) (line-end-position))))))

(vz/bind
 :prefix "C-c"
 "p" #'vz/popup-shell
 :map shell-mode-map
 "j" #'vz/shell-jump-to-dir
 "?" #'vz/shell-insert-from-shell-hist
 "/" #'vz/shell-insert-from-hist
 "J" #'vz/shell-jump-to-prompt
 "k" #'vz/shell-send-keysequence-to-process
 "C-k" #'vz/shell-quote-region-or-till-eol
 [remap comint-clear-buffer] #'vz/shell-clear-buffer)

;; Local Variables:
;; eval: (outline-minor-mode)
;; outline-regexp: ";; [*]+"
;; End:
