(use-package circe)

(setq
 vz/circe-mynicks '("viz" "_viz_")
 vz/circe-mynicks-re (seq-reduce (lambda (res x) (concat res "\\|" x))
                                 (cdr vz/circe-mynicks) (car vz/circe-mynicks))
 circe-network-options
 `(("Freenode"
	  :nick "_viz_"
	  :tls nil
	  :port 6667
	  :channels (:afterauth "#nixhub" "#macs" "#vis-editor" "#unixporn" "#nixos")
	  :nickserv-nick "_viz_"
	  :nickserv-password ,(pass "irc/Freenode"))
   ("Madhouse"
	  :host "irc.astrak.co"
	  :port 6667
	  :nick "_viz_"
	  :channels (:afterauth "#mh-general" "#mh-linux" "#mh-unixporn"
	  					            "#mh-memes" "#mh-scripting")
	  :nickserv-nick "_viz_"
	  :nickserv-password ,(pass "irc/Madhouse"))
   ("Discord Nixhub"
	  :host "localhost"
    :reduce-lurker-spam t
	  :user "viz"
	  :port 6667
	  :channels ("#home" "#man" "#programming" "#devnull" "#music")
	  :pass ,(format "%s:361910177961738242" (pass "misc/discord")))
   ("Discord Frens"
	  :host "localhost"
    :reduce-lurker-spam t
	  :user "viz"
	  :port 6667
	  :channel ("#general" "#commands")
	  :pass ,(format "%s:702578317081182258" (pass "misc/discord")))
   ))

(setq-ns circe
 ;; reduce-lurker-spam t
 use-cycle-completion t
 highlight-nick-type 'message
 server-buffer-name "{network}:{host}"
 split-line-length 70)

(setq-ns lui
  fill-type "         " ;; 9 spaces
  time-stamp-format "%H:%M"
  time-stamp-position 'right-margin)

(defun vz/circe-init ()
  (unless (boundp 'vz/circe--old-nick)
    (setq vz/circe--old-nick ""))
  ;; (variable-pitch-mode 1)
  (setq mode-line-format "    %b"))

(defun vz/circe-draw-msg-generic (nick body &rest body-face)
  (let* ((nick (cond ((member nick vz/circe-mynicks) "me")
                     ((string= nick vz/circe--old-nick) "")
                     (:else nick)))
        (lnick (length nick)))
    (when nick (setq vz/circe--old-nick nick))
    (concat
     (propertize
      (if (> lnick 8)
          (concat (substring nick 0 7) "…")
        (concat (make-string (- 8 lnick) ? ) nick))
      'face (if (string= nick "me")
                'circe-my-message-face
              'circe-originator-face))
     (propertize
      (concat " " body)
      'face (cond
             ((string-match-p vz/circe-mynicks-re body) 'circe-highlight-nick-face)
             ((not (null body-face)) body-face)
             (:else 'default))))))

(defun vz/circe-draw-prompt ()
  (lui-set-prompt (vz/circe-draw-msg-generic (buffer-name) "")))
  
;; I'm quite fond of the old discord join messages
(defun vz/circe-join-msg (nick)
  (let ((msg '("%s has come hear to chew bubblegum and damn they are all out of gum"
               "%s has joined the party"
               "A %s has spawned in the channel"
               "%s has just arrived. Seems OP - nerf please"
               "Roses are red, violets are blue, %s has joined the channel with you"
               )))
    (format (nth (random 0 (1- (length msg)) msg) nick))))

(defun vz/circe-handle-msg (type args)
  (let ((nick (plist-get args :nick))
        (body (plist-get args :body))
        (reason (plist-get args :reason))
        (old-nick (plist-get args :old-nick))
        (new-nick (plist-get args :new-nick))
        )
    (pcase type
      ('say      (vz/circe-draw-msg-generic nick body))
      ('self-say (vz/circe-draw-msg-generic "me" body 'circe-my-message-face))
      ('join     (vz/circe-draw-msg-generic ">" (vz/circe-join-msg nick)))
      ('left     (vz/circe-draw-msg-generic "<" reason))
      ('part     (vz/circe-draw-msg-generic "<" reason))
      ('action   (vz/circe-draw-msg-generic "*" (concat nick " " body)))
      ('nick-ch  (vz/circe-draw-msg-generic old-nick (concat "is now " new-nick)))
      )))

(setq-ns circe-format
  say                (lambda (&rest args) (vz/circe-handle-msg 'say args))
  self-say           (lambda (&rest args) (vz/circe-handle-msg 'self-say args))
  action             (lambda (&rest args) (vz/circe-handle-msg 'action args))
  server-quit        (lambda (&rest args) (vz/circe-handle-msg 'left args))
  server-join        (lambda (&rest args) (vz/circe-handle-msg 'join args))
  server-part        (lambda (&rest args) (vz/circe-handle-msg 'part args))
  server-nick-change (lambda (&rest args) (vz/circe-handle-msg 'nick-ch args)))

(add-hook 'circe-chat-mode-hook #'vz/circe-draw-prompt)
(add-hook 'circe-channel-mode-hook #'vz/circe-init)
