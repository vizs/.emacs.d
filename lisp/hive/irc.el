;; -*- lexical-binding: t; -*-

;; Dangling parenthesis are present for making my life in the future easier

(setq-ns vz/circe
 mynicks '("viz" "_viz_")
 ;; TODO: clean this up
 mynicks-re (-reduce-from
             (fn: format "%s\\|[ @]%s[,: ]\\|[ @]%s$" <1> <2> <2>)
             (funcall
              (fn: format "[ @]%s[,: ]\\|[ @]%s$" <> <>)
              (car vz/circe-mynicks))
             (cdr vz/circe-mynicks)))

(setq-ns circe
 network-options
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
    :pass ,(pass-discord 702578317081182258)))
 ; reduce-lurker-spam t
 use-cycle-completion t
 highlight-nick-type 'message
 server-buffer-name "{network}:{host}")

(defun vz/circe-init ()
  (setq
   vz/circe--old-nick ""
   buffer-face-mode-face '(:family "Charter" :height 100)
   mode-line-format nil)
  (buffer-face-mode)
  (defface circe-my-message-body-face
    `((t :inherit circe-my-message-face :family ,vz/variable-font
         :height 100))
    "Face for self-say body")
  (vz/set-monospace-faces '(circe-prompt-face
                            circe-originator-face
                            circe-my-message-face)))

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
      'face (if (string= nick "me") 'circe-my-message-face
                                    'circe-originator-face))
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
  say                (fn: vz/circe-handle-msg 'say   <rest>)
  self-say           (fn: vz/circe-handle-msg 'ssay  <rest>)
  action             (fn: vz/circe-handle-msg 'acn   <rest>)
  server-message     (fn: vz/circe-handle-msg 'smsg  <rest>)
  server-notice      (fn: vz/circe-handle-msg 'smsg  <rest>)
  server-quit        (fn: vz/circe-handle-msg 'part  <rest>)
  server-join        (fn: vz/circe-handle-msg 'join  <rest>)
  server-topic       (fn: vz/circe-handle-msg 'smsg  <rest>)
  server-part        (fn: vz/circe-handle-msg 'part  <rest>)
  server-nick-change (fn: vz/circe-handle-msg 'nch   <rest>)
  server-rejoin      (fn: vz/circe-handle-msg 'join  <rest>))

(add-hook 'circe-chat-mode-hook #'vz/circe-draw-prompt)
(add-hook 'circe-chat-mode-hook #'vz/circe-init)

(defun vz/circe-get-channels-cond (cond)
  "Get channels from all server buffer that match the condition cond"
  (-flatten
   (-map
    (fn: with-current-buffer <> (circe-server-channel-buffers))
    (-filter cond (circe-server-buffers)))))

(defun vz/circe-jump-irc ()
  "Jump to irc channel"
  (interactive)
  (->>
   (-map #'buffer-name (vz/circe-get-channels-cond
                        (fn: not
                             (string-prefix-p "Discord "
                                              (buffer-name <>)))))
   (ivy-read "> ")
   (switch-to-buffer-other-window)))

(defun vz/circe-jump-discord ()
  "Jump to discord channel"
  (interactive)
  (->>
   (-map #'buffer-name (vz/circe-get-channels-cond
                        (fn: string-prefix-p "Discord "
                             (buffer-name <>))))
   (ivy-read "> ")
   (switch-to-buffer-other-window)))

(general-nmap
  :prefix "SPC i"
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
