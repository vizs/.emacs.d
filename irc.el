;; TODO: make own message face

(use-package circe
  :config
  ;; freenode and discord
  (setq circe-network-options
        '(("Freenode"
           :nick "viz"
           :channels (:afterauth "#emacs" "##cordance")
           :nickserv-nick "_viz_"
           :nickserv-password (lambda (x) vz/freenode-passwd))
          ("disc:r/up"
           :host "127.0.0.1"
           :user "viz"
           :port 6667
           :channels ("#home" "#man" "#programming" "#ricing")
           :pass (lambda (x) vz/disc-rup))))

  ;; set faces
  (custom-set-faces
   `(circe-prompt-face          ((t :background ,vz/color0 :foreground ,vz/color8)))
   `(circe-highlight-nick-face  ((t :foreground ,vz/color8)))
   `(circe-my-message-face      ((t :foreground ,vz/color7)))
   `(circe-server-face          ((t :foreground ,vz/color8)))
   `(circe-originator-face      ((t :foreground ,vz/color8)))
   `(lui-highlight-face         ((t :foreground ,vz/color8)))
   `(lui-irc-colors-fg-6-face   ((t :foreground ,vz/color8)))
   `(lui-irc-colors-fg-5-face   ((t :foreground ,vz/color8)))
   `(lui-irc-colors-fg-7-face   ((t :foreground ,vz/color8)))
   `(lui-irc-colors-fg-8-face   ((t :foreground ,vz/color8)))
   `(lui-irc-colors-fg-2-face   ((t :foreground ,vz/color8)))
   `(lui-irc-colors-fg-13-face  ((t :foreground ,vz/color8)))
   `(lui-button-face            ((t :foreground ,vz/color8))))

  ;; lui settings
  (require 'lui-autopaste)
  (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)
  (setq circe-use-cycle-completion t
        circe-reduce-lurker-spam   t
        circe-split-line-length    250
        lui-logging-directory "~/var/cache/irc-log"
        circe-use-cycle-completion t)
  (load "lui-logging" nil t)
  (enable-lui-logging-globally)
  (circe-set-display-handler "353" 'circe-display-ignore)
  (circe-set-display-handler "366" 'circe-display-ignore)

  ;; message format
  (setq circe-format-self-say            " {nick:-5s} {body}"
        circe-format-self-action         " {nick:-5s} {body}"
        circe-format-self-message-action " {nick:-5s} {body}"
        circe-format-self-message        " {nick:-5s} {body}"
        circe-format-say                 " {nick:-5s} {body}"
        circe-format-action              " {nick:-5s} {body}"
        circe-format-message-action      " {nick:-5s} {body}"
        circe-format-message             " {nick:-5s} {body}"
        lui-time-stamp-position                     nil)

  ;; prompt
  (lui-set-prompt
   (concat (propertize (buffer-name)
                       'face 'circe-prompt-face) " ")))
