;; -*- lexical-binding: t; -*-

(setq vz/plumb-video-exts '("mp4" "mkv" "webm" "gif")
      vz/plumb-audio-exts '("mp3" "flac" "ogg")
      vz/plumb-img-exts '("png" "jpg" "jpeg" "JPEG" "JPG"))

(defun vz/plumb-download-url (url)
  "Download the given url and return the place in which it is stored"
  (let ((file-name (make-temp-name "/tmp/plumb.")))
    (async-shell-command (format "curl-O%s %s" file-name url))))

(defun vz/plumb-yt (string)
  (async-shell-command (format "mpv %s" string)))

(defun vz/plumb-img (path)
  (async-shell-command (format "meh %s" path)))

(defun vz/plumb-pdf (path)
  (async-shell-command (format "zathura %s" path)))

;; Maybe consider mime types too?
(defun vz/plumb--file (ext path)
  (pcase ext
    ("txt" (view-file-other-window path))
    (`(or ,@vz/img-exts) (vz/plumb-img path))
    ("pdf" (vz/plumb-pdf path))))

(defun vz/plumb-file (string &optional line)
  (if (bound-and-true-p vz/term-mode)
      (vz/plumb--file (replace-regexp-in-string "^.*\\." "" string) string)
    (progn (find-file string)
           (when line (goto-line line)))))

(defun vz/plumb-url (string)
  (let* ((file-name (replace-regexp-in-string "^.*/" "" string))
         (ext (replace-regexp-in-string "^.*\\." "" file-name))
         (path nil))
    (cond
     ((member ext (append vz/plumb-video-exts vz/plumb-audio-exts))
      (vz/plumb-yt string))
     (:else
      (browse-url-chromium string)))))

;; Checking if string has non-english letters and translating would be nice
;; TODO: Look into using eval-in-repl
(defun vz/plumb-eval (string)
  (pcase major-mode
    ('python-mode (python-send-string string))
    ;;('scheme-mode (vz/scheme-send-string string))
    ('emacs-lisp-mode (eval (read string)))
    (- (shell-command string))))

;; When using use-package, it errors -- void variable wand-helper:maybe-... 
(setq wand:*rules*
   (list
    (wand:create-rule :match "[A-Za-z0-9]+([0-9a-z]+)"
                      :capture :whole
                      :action (fn:
                               switch-to-buffer-other-window (man <>)))
    (wand:create-rule :match "https?://youtube.com"
                      :capture :whole
                      :action #'vz/plumb-yt)
    (wand:create-rule :match "https?://youtu.be"
                      :capture :whole
                      :action #'vz/plumb-yt)
    (wand:create-rule :match "https?://"
                      :capture :whole
                      :action #'vz/plumb-url)
    (wand:create-rule :match "file://"
                      :capture :after
                      :action #'vz/plumb-file)
    (wand:create-rule :match ".*:[0-9]+$"
                      :capture :whole
                      :action (fn:
                               -->
                               (s-split ":" <>)
                               (vz/plumb-file
                                (car it)
                                (string-to-number (cadr it)))))
    (wand:create-rule :match "\\..+$"
                      :capture :whole
                      :action #'vz/plumb-file)
    (wand:create-rule :match "\\$ "
                      :capture :after
                      :action #'shell-commmand)
    (wand:create-rule :match ".*"
                      :capture :whole
                      :action #'vz/plumb-eval)
    ))

(defun vz/plumb ()
  "Plumb the region or thing at point"
  (interactive)
  (wand:execute (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning)
                                                    (region-end))
                  (ivy-thing-at-point))))

(defun vz/plumb-link-select ()
  "Select link in current buffer and plumb it"
  (interactive)
  (wand:execute
   (plist-get (link-hint--process (link-hint--get-links)) :args)))

;; (general-define-key
;;  :states '(normal visual)
;;  :keymaps 'override
;;  "|"      #'vz/plumb
;;  "SPC lp" #'vz/plumb-link-select)
