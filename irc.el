;; Dangling parenthesis are present for making my life in the future easier

(use-package circe)

(setq
 vz/circe-mynicks '("viz" "_viz_")
 vz/circe-mynicks-re (seq-reduce #'(lambda (res x)
                                     (format "%s\\|.*%s.*" res x))
                                 (cdr vz/circe-mynicks)
                                 (format ".*%s.*" (car vz/circe-mynicks)))
 circe-network-options
 `(("Freenode"
	  :nick "_viz_"
    :port (6667 . 6697)
	  :channels (:after-auth "#nixhub" "#vis-editor" "#unixporn" "#nixos" "#emacs")
    :nickserv-nick "_viz_"
	  :nickserv-password ,(pass-irc "Freenode"))
   ("Discord Madhouse"
	  :host "irc.astrak.co"
	  :user "viz"
	  :channels ("#general" "#linux" "#unixporn" "#memes" "#scripting")
	  :pass ,(pass-discord 446727370964205585))
   ("Discord Nixhub"
	  :host "localhost"
	  :user "viz"
	  :port 6667
	  :channels ("#home" "#man" "#programming" "#devnull" "#music")
	  :pass ,(pass-discord 361910177961738242))
   ("Discord Frens"
	  :host "localhost"
	  :user "viz"
	  :port 6667
	  :channel ("#general" "#commands")
	  :pass ,(pass-discord 702578317081182258))
   ))

(setq-ns circe
 ;; reduce-lurker-spam t
 use-cycle-completion t
 highlight-nick-type 'message
 server-buffer-name "{network}:{host}")

(defun vz/circe-init ()
  (setq
   vz/circe--old-nick ""
   buffer-face-mode-face '(:family "Charter" :height 100)
   mode-line-format "    %b")
  (buffer-face-mode)
  (defface circe-my-message-body-face
    '((t :inherit circe-my-message-face :family "Charter" :height 100))
    "Face for self-say body")
  (dolist (f '(circe-prompt-face circe-originator-face circe-my-message-face))
    (set-face-attribute f nil :inherit 'fixed-pitch)))

(defun vz/circe-draw-msg-generic (nick body &optional body-face)
  (let* ((nick (cond ((member nick vz/circe-mynicks) "me")
                     ((string= nick vz/circe--old-nick) "")
                     (:else nick)))
         (body-face (cond
                     ((string= "me" nick) 'circe-my-message-body-face)
                     ((string-match-p vz/circe-mynicks-re body) 'circe-highlight-nick-face)
                     ((null body-face) 'default)
                     (:else body-face)))
         (lnick (length nick))
         (lbody (length body))
         (spaces (propertize (make-string 9 ? ) 'face 'circe-originator-face)))
    (unless (string-empty-p nick)
      (setq-local vz/circe--old-nick nick))
    (concat
     (propertize
      (if (> lnick 8)
          (concat (substring nick 0 7) "… ")
        (concat (make-string (- 8 lnick) ? ) nick " "))
      'face (if (string= nick "me") 'circe-my-message-face 'circe-originator-face))
     (propertize body 'face body-face))))

(defun vz/circe-draw-prompt ()
  (lui-set-prompt (vz/circe-draw-msg-generic (buffer-name) "")))
  
;; I'm quite fond of the old discord join messages
(defun vz/circe-join-msg (nick)
  (let ((msg '("%s joined your party"
               "%s joined. You must construct additional pylons"
               "Ermagherd. %s is here"
               "Welcome, %s. Stay awhile and listen"
               "Welcome, %s. We were expecting you ( ͡° ͜ʖ ͡°)"
               "Welcome, %s. We hope you brought pizza"
               "Welcome %s. Leave your weapons by the door"
               "A wild %s appeared"
               "Swoooosh. %s just landed"
               "Brace yourselves. %s just joined the server"
               "%s just joined. Hide your bananas"
               "%s just showed up. Hold my beer"
               "%s has just arrived. Seems OP - nerf please"
               "Roses are red, violets are blue, %s has joined the chat with you"
               )))
    (format (vz/random-choice msg) nick)))

(defun vz/circe-leave-msg (nick)
  (format "%s has left" nick))

(defun vz/circe-handle-msg (type args)
  (let ((nick (plist-get args :nick))
        (body (plist-get args :body))
        (reason (plist-get args :reason))
        (old-nick (plist-get args :old-nick))
        (new-nick (plist-get args :new-nick)))
    (unless (boundp 'vz/circe--old-nick) (setq-local vz/circe--old-nick ""))
    (pcase type
      ('say  (vz/circe-draw-msg-generic nick body))
      ('ssay (vz/circe-draw-msg-generic "me" body 'circe-my-message-body-face))
      ('join (vz/circe-draw-msg-generic ">" (vz/circe-join-msg nick)))
      ('part (vz/circe-draw-msg-generic "<" (vz/circe-leave-msg nick)))
      ('acn  (vz/circe-draw-msg-generic "*" (concat nick " " body)))
      ('nch  (vz/circe-draw-msg-generic old-nick (concat "is now " new-nick)))
      ('smsg (vz/circe-draw-msg-generic "***" body 'circe-server-face))
      )))

(setq-ns circe-format
  say                (lambda (&rest args) (vz/circe-handle-msg 'say args))
  self-say           (lambda (&rest args) (vz/circe-handle-msg 'ssay args))
  action             (lambda (&rest args) (vz/circe-handle-msg 'acn args))
  server-message     (lambda (&rest args) (vz/circe-handle-msg 'smsg args))
  server-quit        (lambda (&rest args) (vz/circe-handle-msg 'part args))
  server-join        (lambda (&rest args) (vz/circe-handle-msg 'join args))
  server-part        (lambda (&rest args) (vz/circe-handle-msg 'part args))
  server-nick-change (lambda (&rest args) (vz/circe-handle-msg 'nch args))
  server-rejoin      (lambda (&rest args) (vz/circe-handle-msg 'join args)))

(add-hook 'circe-chat-mode-hook #'vz/circe-draw-prompt)
(add-hook 'circe-chat-mode-hook #'vz/circe-init)

;; N e s t
(defun vz/circe-get-channels-cond (cond)
  "Get channels from all server buffer that match the condition cond"
  (flatten-list
   (mapcar #'(lambda (x) (with-current-buffer x (circe-server-channel-buffers)))
           (seq-filter cond (circe-server-buffers)))))

(defun vz/circe-jump-irc ()
  "Jump to irc channel"
  (interactive)
  (switch-to-buffer-other-window
   (ivy-read
    "> "
    (mapcar #'buffer-name
        (vz/circe-get-channels-cond
         #'(lambda (x) (not(string-prefix-p "Discord " (buffer-name x)))))))))

(defun vz/circe-jump-discord ()
  "Jump to discord channel"
  (interactive)
  (switch-to-buffer-other-window
   (ivy-read "> " (mapcar #'buffer-name
                          (vz/circe-get-channels-cond
                           #'(lambda (x) (string-prefix-p "Discord "
                                                          (buffer-name x))))))))

(general-nmap
  :prefix "SPC I"
  "i" #'vz/circe-jump-irc
  "d" #'vz/circe-jump-discord)

(setq-ns lui
  logging-directory (expand-file-name "~/.cache/irc-log")
  fill-type nil
  time-stamp-format "%H:%M"
  time-stamp-position 'right-margin)

(defun vz/lui-init ()
  (setq
   fringes-outside-margins t
   right-margin-width 5
   left-margin-width 5
   word-wrap t
   wrap-prefix (propertize (make-string 9 ? ) 'face 'circe-originator-face))
  (setf (cdr (assoc 'continuation fringe-indicator-alist)) nil))

(add-hook 'lui-mode-hook #'vz/lui-init)
