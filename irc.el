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

(defun vz:theme-circe ()
  (set-face-attribute 'circe-prompt-face          nil :background "#ffffff"
                                                      :foreground "#a7a7a7")
  (set-face-attribute 'circe-highlight-nick-face  nil :foreground "#a7a7a7")
  (set-face-attribute 'circe-my-message-face      nil :foreground "#5f5a60")
  (set-face-attribute 'circe-server-face          nil :foreground "#a7a7a7")
  (set-face-attribute 'circe-originator-face      nil :foreground "#a7a7a7")
  (set-face-attribute 'lui-highlight-face         nil :foreground "#a7a7a7")
  (set-face-attribute 'lui-irc-colors-fg-6-face   nil :foreground "#a7a7a7")
  (set-face-attribute 'lui-irc-colors-fg-5-face   nil :foreground "#a7a7a7")
  (set-face-attribute 'lui-irc-colors-fg-7-face   nil :foreground "#a7a7a7")
  (set-face-attribute 'lui-irc-colors-fg-8-face   nil :foreground "#a7a7a7")
  (set-face-attribute 'lui-irc-colors-fg-2-face   nil :foreground "#a7a7a7")
  (set-face-attribute 'lui-irc-colors-fg-13-face  nil :foreground "#a7a7a7")
  (set-face-attribute 'lui-button-face            nil :foreground "#a7a7a7"))

(defun vz:circe-general ()
  (require 'lui-autopaste)
  (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)
  (setq circe-use-cycle-completion t
        circe-reduce-lurker-spam t)
  (setq lui-logging-directory "~/var/cache/irc-log")
  (setq circe-use-cycle-completion t)
  (load "lui-logging" nil t)
  (enable-lui-logging-globally))

(defun vz:circe-format ()
  (setq circe-format-self-say            "{nick} {body}"
        circe-format-self-action         "{nick} {body}"
        circe-format-self-message-action "{nick} {body}"
        circe-format-self-message        "{nick} {body}"
        circe-format-say                 "{nick} {body}"
        circe-format-action              "{nick} {body}"
        circe-format-message-action      "{nick} {body}"
        circe-format-message             "{nick} {body}"
        lui-time-stamp-position                     nil))

(defun vz:circe-prompt ()
  (lui-set-prompt
   (concat (propertize (buffer-name) 'face 'circe-prompt-face) " ")))

(use-package circe
  :config
  (vz:circe-networks)
  (vz:circe-general)
  (vz:circe-format)
  (vz:theme-circe)
  (add-hook 'circe-chat-mode-hook 'vz:circe-prompt)
  (add-hook 'circe-chat-mode-hook 'vz:minimal-ui))
