(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ledger-reports (quote (("last month balance" "ledger -f %(ledger-file) bal ") ("bal" "ledger -f %(ledger-file) bal") ("reg" "ledger -f %(ledger-file) reg") ("payee" "ledger -f %(ledger-file) reg -- %(payee)") ("account" "ledger -f %(ledger-file) reg %(account)") ("balance from Kitty start" "ledger -f %(ledger-file) bal ^Expenses ^Income -S date --period='from 2012/04/25'") ("monthly Expenses from Kitty start" "ledger -f %(ledger-file) reg ^Expenses --monthly --period='from 2012/04/25'") ("monthly expenses from Christa start" "ledger -f %(ledger-file) reg ^Expenses --monthly --period='from 2012/06/18'") ("balance from Christa start" "ledger -f %(ledger-file) bal ^Expenses ^Income -S date --period='from 2012/06/18'"))))
 '(org-drill-optimal-factor-matrix (quote ((5 (2.66 . 2.65) (2.32 . 2.32)) (4 (2.42 . 2.457) (2.1399999999999997 . 2.288) (1.9400000000000002 . 2.007) (2.8000000000000003 . 2.785) (2.6 . 2.6) (2.66 . 2.635) (2.7 . 2.691) (2.22 . 2.22) (2.1799999999999997 . 2.291) (2.9 . 2.898) (2.32 . 2.374) (2.38 . 2.511) (1.92 . 2.202) (2.56 . 2.546)) (3 (2.52 . 2.505) (2.22 . 2.22) (2.66 . 2.618) (1.92 . 2.186) (1.54 . 1.917) (1.9000000000000001 . 2.065) (2.08 . 2.142) (2.06 . 2.316) (2.16 . 2.398) (1.6800000000000002 . 1.987) (1.6600000000000001 . 2.101) (2.04 . 2.195) (2.6 . 2.588) (2.32 . 2.32) (2.3200000000000003 . 2.298) (2.56 . 2.529) (2.7 . 2.679) (2.8000000000000003 . 2.773) (2.46 . 2.443)) (2 (2.9 . 2.898) (2.52 . 2.557) (2.42 . 2.47) (2.66 . 2.674) (2.24 . 2.381) (2.1399999999999997 . 2.322) (2.56 . 2.584) (2.02 . 2.291) (2.22 . 2.22) (2.38 . 2.491) (2.08 . 2.142) (2.2800000000000002 . 2.407) (2.5 . 2.5) (2.1799999999999997 . 2.239) (2.7 . 2.679) (2.46 . 2.497) (2.6 . 2.588) (2.36 . 2.412) (2.2199999999999998 . 2.328) (1.8199999999999998 . 2.112) (2.04 . 2.104)) (1 (2.32 . 3.855) (2.66 . 4.28) (2.02 . 3.701) (2.18 . 3.995) (2.08 . 3.86) (2.8000000000000003 . 4.435) (2.24 . 3.846) (2.1399999999999997 . 3.715) (2.56 . 4.135) (2.38 . 3.985) (2.7 . 4.285) (2.2800000000000002 . 3.85) (2.46 . 3.995) (2.3200000000000003 . 4.14) (1.9000000000000001 . 3.72) (2.2199999999999998 . 3.725) (2.6 . 4.14) (2.1799999999999997 . 3.72) (2.36 . 3.86) (2.5 . 4.0) (1.96 . 3.58) (1.7000000000000002 . 3.44)))))
 '(safe-local-variable-values (quote ((my-org-really-auto-save)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
