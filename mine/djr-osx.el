;; (require 'djr-addressbook)

(setq locate-command "mdfind")

(set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)

(require 'gnutls)
(setq gnutls-verify-error t)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")

;(load "~/.finda/integrations/emacs/finda.el")

(setq ns-use-native-fullscreen nil)

(provide 'djr-osx)
