; http://emacs-fu.blogspot.com/2009/04/adding-custom-menus.html

(easy-menu-define djr-menu global-map "MyMenu"
  '("MyMenu"
    ["Mail with follow up"  (djr/mu4e-compose-new-with-follow-up)]
    ("Agendas"
     ["Agenda-Org" (org-agenda)]
     ["Home"       (djr/agenda-home)]
     ["Notebook"   (djr/agenda-notebook)])
    ("Programs"
     ["MU4E" (mu4e)])))

(provide 'djr-easymenu)
