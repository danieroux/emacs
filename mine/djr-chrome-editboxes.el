; https://github.com/stsquad/emacs_chrome
; https://chrome.google.com/webstore/detail/edit-with-emacs/ljobjlafonikaiipfkggjbhkghgicgoh/related

(use-package edit-server
  :pin melpa
  :ensure t
  :idle (edit-server-start))

(provide 'djr-chrome-editboxes)
