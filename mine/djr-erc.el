(require 'erc)
(require 'erc-services)

(setq erc-auto-query 'frame
      erc-track-when-inactive t
      erc-buffer-activity-timeout 0
      erc-hide-list '("JOIN" "PART" "QUIT")
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT")
      erc-track-position-in-mode-line t ;; Add to global-mode-string
      erc-kill-buffer-on-part t)

(setq erc-modules (quote (autojoin
			  button
			  completion
			  fill
			  irccontrols
			  keep-place
			  list
			  match
			  menu
			  move-to-prompt
			  netsplit
			  networks
			  noncommands
			  pcomplete
			  readonly
			  ring
			  services
			  spelling
			  stamp
			  scrolltobottom
			  track)))

(erc-update-modules)

(setq erc-user-full-name "Danie Roux"
      erc-keywords '("danieroux")
      erc-nick "danieroux")

(setq erc-prompt-for-nickserv-password nil
      erc-nickserv-passwords `((freenode (("danieroux" . ,erc-password)))))

(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs")))

(defun djr/irc ()
  (interactive)
  (erc :server "irc.freenode.net"
       :port 6667))

(provide 'djr-erc)
