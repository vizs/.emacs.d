(defun vz:circe-networks ()
 (setq circe-network-options
       '(("Freenode"
          :nick "viz"
          :channels (:afterauth "#emacs" "##cordance")
          :nickserv-nick "_viz_"
          :nickserv-password (lambda (x) vz:freenode-passwd))
         ("disc:r/up"
          :host "127.0.0.1"
          :user "viz"
          :port 6667
          :channels ("#home" "#man" "#programming" "#ricing")
          :pass (lambda (x) vz:disc-rup)))))

(defun vz:circe-general ()
  (require 'lui-autopaste)
  (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)
  (setq circe-use-cycle-completion t
        circe-reduce-lurker-spam t)
  (setq lui-logging-directory "~/var/cache/irc-log")
  (load "lui-logging" nil t)
  (enable-lui-logging-globally))

(defun vz:circe-format ()
  (setq circe-format-self-say "{nick} {body}")
  (setq circe-format-self-action "{nick} {body}")
  (setq circe-format-self-message-action "{nick} {body}")
  (setq circe-format-self-message"{nick} {body}")
  (setq circe-format-say "{nick} {body}")
  (setq circe-format-action "{nick} {body}")
  (setq circe-format-message-action "{nick} {body}")
  (setq circe-format-message"{nick} {body}")
  (setq lui-time-stamp-position nil))

(defun vz:circe-prompt ()
  (lui-set-prompt
   (concat (propertize (buffer-name) 'face 'circe-prompt-face) " ")))

(use-package circe
  :config
  (vz:circe-networks)
  (vz:circe-format)
  (vz:theme-circe)
  (add-hook 'circe-chat-mode-hook 'vz:circe-prompt)
  (add-hook 'circe-chat-mode-hook '(lambda () (setq mode-line-format nil)))
  (add-hook 'circe-channel-mode-hook '(lambda () (setq mode-line-format nil))))
