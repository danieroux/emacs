;; (require 'djr-addressbook)

(setq locate-command "mdfind")

(set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)

(require 'gnutls)
(setq gnutls-verify-error t)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")

(provide 'djr-osx)
