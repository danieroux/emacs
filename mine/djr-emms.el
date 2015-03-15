(use-package emms-setup
  :pin "melpa"
  :ensure emms
  :bind* (("<f7>" . emms-previous)
	  ("S-<f7>" . djr/helm-emms)
	  ("<f8>" . emms-pause)
	  ("S-<f8>" . emms-playlist-mode-go)
	  ("<f9>" . emms-next)
	  ("S-<f9>" . djr/play-music))
  :config
  (progn
    ; https://www.gnu.org/software/emms/manual/Using-TagLib.html
    (require 'emms-info-libtag)
    (setq emms-info-libtag-program-name "~/.emacs.d/external/emms/src/emms-print-metadata")
    (setq emms-info-functions '(emms-info-libtag))

    (emms-all)
    (define-emms-simple-player afplay '(file)
      (regexp-opt '(".mp3" ".m4a" ".aac"))
      "afplay")
    (setq emms-player-list `(,emms-player-afplay))
    (setq emms-source-file-default-directory "~/Music")

    (require 'helm-emms)
    (defun djr/helm-emms ()
      (interactive)
      (helm :sources '(helm-source-emms-files)
	    :buffer "*Helm Emms*"))))

;; Filters don't work for me. I want to script what I'm playing, not setup filters to apply
;; (emms-browser-make-filter "the-chaconne" (emms-browser-filter-only-dir "~/Music/the-chaconne"))

(defun djr/play-the-chaconne ()
  (interactive)
  (djr/emms-play-least-heard-first
   "The Bach Chaconne"
   '(lambda ()
      (emms-add-directory "~/Music/the-chaconne"))))

(defun djr/play-music ()
  (interactive)
  (djr/emms-play-least-heard-first
   "All My Music"
   '(lambda ()
      (emms-add-directory-tree "~/Music/mine")
      (emms-add-directory-tree "~/Music/other")
      (emms-add-directory-tree "~/Music/iTunes/iTunes Media/Music"))))

(autoload 'emms-stop "emms-setup" "Play the least heard first" t nil)
(defun djr/emms-play-least-heard-first (playlist-name playlist-generation-fun)
  (emms-stop)
  (emms-playlist-set-playlist-buffer (emms-playlist-new (concat " *EMMS: " playlist-name "*")))
  (funcall playlist-generation-fun)
  (emms-shuffle)
  (emms-playlist-sort-by-last-played)
  (with-current-emms-playlist
   (emms-playlist-mode-play-smart)))

(provide 'djr-emms)
