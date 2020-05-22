(setq-ns vz/plumb
 video-exts '("mp4" "mkv" "webm" "gif")
 audio-exts '("mp3" "flac" "ogg")
 img-exts '("png" "jpg" "jpeg" "JPEG" "JPG"))

(defun vz/plumb-download-url (url)
  "Download the given url and return the place in which it is stored"
  (let ((file-name (make-temp-name "/tmp/plumb.")))
    (call-process "curl" nil nil 0 "-O" file-name url)))

(defun vz/plumb-yt (string)
  (start-process "plumb-yt" nil "mpv" string))

;; TODO: * Open in emacs if file size isn't large
(defun vz/plumb-img (path)
  (call-process "meh" nil nil 0 path))

(defun vz/plumb-pdf (path)
  (call-process "zathura" nil nil 0 path))

;; Maybe consider mime types too?
(defun vz/plumb--file (ext path)
  (pcase ext
    ("txt" (view-file-other-window path))
    (`(or ,@vz/img-exts) (vz/plumb-img path))
    ("pdf" (vz/plumb-pdf path))))

(defun vz/plumb-file (string &optional line)
  (if (bound-and-true-p vz/term-mode)
      (vz/plumb--file (replace-regexp-in-string "^.*\\." string) string)
    (progn (find-file string)
           (when line (goto-line line)))))

(defun vz/plumb-url (string)
  (let* ((file-name (replace-regexp-in-string "^.*/" "" string))
         (ext (replace-regexp-in-string "^.*\\." "" file-name))
         (path nil))
    (cond
     ((or (string-empty-p file-name) (string= ext "html"))
      (browse-url-chromium string))
     ((member ext (append vz/plumb-video-exts vz/plumb-audio-exts))
      (vz/plumb-yt string))
     (:else
      (setq-local path vz/plumb-download-url string)
      (vz/plumb-file ext path)
      (delete-file path)))))

;; Checking if string has non-english letters and translating would be nice
(defun vz/plumb-eval (string)
  (pcase major-mode
    ('python-mode (python-send-string string))
    ;;('scheme-mode (vz/scheme-send-string string))
    ('emacs-lisp-mode (eval (read string)))
    (- (shell-command string))))

;; When using use-package, it errors -- void variable wand-helper:maybe-... 
(when (vz/load-pkg "wand")
  (setq wand:*rules*
   (list
    (wand:create-rule :match "[A-Za-z0-9]+([0-9a-z]+)"
                      :capture :whole
                      :action #'(lambda (string)
                                  (switch-to-buffer-other-window (man string))))
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
                      :action #'(lambda (string)
                                  (let ((f (split-string string ":")))
                                    (vz/plumb-file (car f)
                                                (string-to-number (cadr f))))))
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
    (interactive)
    (wand:execute (if (region-active-p)
                      (buffer-substring-no-properties (region-beginning)
                                                      (region-end))
                    (ivy-thing-at-point))))
  (general-define-key
   :states '("normal" "visual")
   :keymaps 'override
   "|" 'vz/plumb))
