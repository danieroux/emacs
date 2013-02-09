(require 'erc)
(require 'erc-services)

(erc-services-mode 1)

(setq erc-auto-query 'frame
      erc-user-full-name "Danie Roux"
      erc-keywords '("danieroux")
      erc-nick "danieroux"
      erc-track-when-inactive t
      erc-hide-list '("JOIN" "PART" "QUIT")
      erc-autojoin-mode 1
      erc-prompt-for-nickserv-password nil)

(setq erc-autojoin-channels-alist
      '(("freenode.net" "#clojure.za" "#clojure")))

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
			 readonly
			 ring
			 stamp
			 track)))

(erc-update-modules)

(setq erc-nickserv-passwords
      `((freenode (("danieroux" . ,erc-password)))
	(another (("nickname" . "password")))))
	    
(defun djr/irc ()
  (interactive)
  (erc :server"irc.freenode.net"
       :port 6667))

(provide 'djr-erc)
