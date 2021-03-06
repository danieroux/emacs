(use-package hydra
  :ensure t
  :defer t
  :pin "melpa"
  :bind* (("C-x l" . hydra-launch/body)
          ("C-c p" . hydra-projectile/body))

  :config
  (progn
    (defhydra hydra-launch ()
      "Launch random useful things"
      ("n" elfeed "Elfeed")
      ("t" djr/twittering-fix-clobbering "Twitter")
      ("c" (org-clock-in '(4)) "Clock-in")
      ("i" rcirc "IRC")
      ("d" deft "Deft"))

    (defhydra hydra-mail (:color blue)
      "Mail"
      ("m" djr/mu4e-inbox "inbox")
      ("c" djr/mu4e-compose-new-with-follow-up "compose new with follow up")
      ("C" mu4e-compose-new "compose new")
      ("b" mu4e-headers-search-bookmark "bookmarks")
      ("u" djr/sync-mail-and-update-mu4e-quickly "quick mbsync update")
      ("U" djr/sync-mail-and-update-mu4e "full mbsync update")
      ("f" smtpmail-send-queued-mail "flush queue")
      ("p" (switch-to-buffer (get-buffer djr/mbsync-buffer-name)) "mbsync process buffer")
      ("Q" mu4e~main-toggle-mail-sending-mode "toggle mail sending mode"))

    (defhydra hydra-projectile (:color teal)
      "
Find File          Search/Tags       Buffers                  Cache
------------------------------------------------------------------------------------------
^^_f_: file dwim       _a_: ag             ^^_i_: Ibuffer               _c_: cache clear
^^_r_: recent file     _g_: update gtags   ^^_K_: Kill all buffers      _x_: remove known project
^^_d_: dir             _o_: multi-occur    ^^_X_: cleanup non-existing  _z_: cache current

Operate On
---------------------
^^_D_: Dired
^^_m_: Magit
^^_e_: Eshell
"
      ("a"   projectile-ag                      nil)
      ("c"   projectile-invalidate-cache        nil)
      ("d"   projectile-find-dir                nil)
      ("e"   (eshell '(4))                      nil)
      ("D"   projectile-dired                   nil)
      ("f"   projectile-find-file-dwim          nil)
      ("g"   projectile-regenerate-tags         nil)
      ("i"   projectile-ibuffer                 nil)
      ("K"   projectile-kill-buffers            nil)
      ("m"   (magit-status (projectile-project-root)) nil)
      ("o"   projectile-multi-occur             nil)
      ("p"   projectile-switch-project          nil)
      ("r"   projectile-recentf                 nil)
      ("x"   projectile-remove-known-project    nil)
      ("X"   projectile-cleanup-known-projects  nil)
      ("z"   projectile-cache-current-file      nil)
      ("q"   nil                                "cancel" :color blue))))

(provide 'djr-hydra)
