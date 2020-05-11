(use-package circe)

(setq
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
	:user "viz"
	:port 6667
	:channels ("#home" "#man" "#programming" "#devnull")
	:pass ,(format "%s:361910177961738242" (pass "misc/discord")))
   ("Discord Frens"
	:host "localhost"
	:user "viz"
	:port 6667
	:channel ("#general" "#commands")
	:pass ,(format "%s:702578317081182258" (pass "misc/discord")))
   ))

(dolist (f '(lui-irc-colors-bg-0-face  lui-irc-colors-bg-1-face
			 lui-irc-colors-bg-2-face  lui-irc-colors-bg-3-face
			 lui-irc-colors-bg-4-face  lui-irc-colors-bg-5-face
			 lui-irc-colors-bg-6-face  lui-irc-colors-bg-7-face
			 lui-irc-colors-bg-8-face  lui-irc-colors-bg-9-face
			 lui-irc-colors-bg-10-face lui-irc-colors-bg-11-face
			 lui-irc-colors-bg-12-face lui-irc-colors-bg-13-face
			 lui-irc-colors-bg-14-face lui-irc-colors-bg-15-face))
  `(set-face-attribute ,f nil :background "#ffffea"))

(setq
 ;; circe-reduce-lurker-spam t
 circe-use-cycle-completion t
 circe-split-line-length 80
 circe-highlight-nick-type 'message
 circe-server-buffer-name "{network}:{host}"
 )
