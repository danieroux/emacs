(require 'erc)

(setq erc-auto-query 'frame
      erc-user-full-name "Danie Roux"
      erc-keywords '("danieroux")
      erc-nick "danieroux"
      erc-track-when-inactive t
      erc-autojoin-mode 1)

(setq erc-autojoin-channels-alist
      '(("freenode.net" "#clojure.za")))

(defun djr/irc ()
  (interactive)
  (erc :server "irc.freenode.net" :port 6667))

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

(provide 'djr-erc)
