(use-package emms-setup
  :pin "melpa"
  :ensure emms
  :commands (djr/emms-start-local-music-or-pause djr/emms-start-google-music-ifl djr/emms-play-the-chaconne)
  :bind* (("<f7>" . djr/emms-previous)
          ("S-<f7>" . emms-stop)
          ("<f8>" . djr/emms-start-local-music-or-pause)
          ("S-<f8>" . djr/emms-start-google-music-ifl)
          ("<f9>" . djr/emms-next))
  :config
  (progn
    (emms-all)

    (use-package emms-player-mpv
      :ensure t
      :config

      (progn

        ;; Redefine, to add aac
        (define-emms-simple-player mpv '(file url streamlist playlist)
          (concat "\\`\\(http\\|mms\\)://\\|"
                  (emms-player-simple-regexp
                   "ogg" "mp3" "wav" "mpg" "mpeg" "wmv" "wma"
                   "mov" "avi" "divx" "ogm" "ogv" "asf" "mkv"
                   "rm" "rmvb" "mp4" "flac" "vob" "m4a" "ape"
                   "flv" "webm" "aac"))
          "mpv" "--quiet" "--really-quiet")

        (add-to-list 'emms-player-list 'emms-player-mpv)


        ;; Break the emms model. emms wants to control the playing object. Be that playlist or file.
        ;; And really wants to go next - away from the current playlist
        (defun emms-player-mpv-stream-next ()
          "Break the emms model by going next in the current playing object."
          (interactive)
          (let ((cmd (emms-player-mpv--format-command "playlist-next")))
            (call-process-shell-command cmd nil nil nil)))

        (defun emms-player-mpv-stream-previous ()
          "Break the emms model by going next in the current playing object."
          (interactive)
          (let ((cmd (emms-player-mpv--format-command "playlist-previous")))
            (call-process-shell-command cmd nil nil nil)))))

    (defvar gmusicproxy-command "GMusicProxy --email <email> --password <password>"
      "Startup command for GMusicProxy (http://gmusicproxy.net)")

    (defvar *gmusicproxy* nil
      "Process handle for gmusicproxy")

    (defun djr/gmusicproxy-running-p ()
      (and *gmusicproxy*
           (eq 'run (process-status *gmusicproxy*))))

    (defun djr/gmusicproxy-start ()
      (let ((process-name (split-string gmusicproxy-command)))
        (when (and (not (djr/gmusicproxy-running-p))
                   (or (file-exists-p (car process-name))
                       (executable-find (car process-name))))
          (setq *gmusicproxy* (apply 'start-process (append (list "GMusicProxy" "*GMusicProxy*") process-name))))))

    (defun djr/gmusicproxy-wait-until-available ()
      (let ((output "")
            (old-filter (process-filter *gmusicproxy*)))

        (set-process-filter *gmusicproxy* (lambda (proc string)
                                            (setf output (concat output string))
                                            (funcall old-filter proc string)))

        (while (and (djr/gmusicproxy-running-p)
                    (not (string-match "Listening on port" output)))
          (accept-process-output *gmusicproxy*))

        (set-process-filter *gmusicproxy* old-filter)

        (if (djr/gmusicproxy-running-p)
            (message "GMusicProxy running.")
          (message "GMusicProxy could not start up, it seems."))))

    (defun djr/gmusicproxy-ensure ()
      (unless (djr/gmusicproxy-running-p)
        (djr/gmusicproxy-start)
        (djr/gmusicproxy-wait-until-available)))

    (defun djr/emms-start-google-music-ifl ()
      (interactive)
      (djr/gmusicproxy-ensure)
      (emms-play-streamlist "http://localhost:9999/get_ifl_station"))

    (defun djr/emms-start-google-music-search-artist (artist-search)
      (interactive "sArtist to search for: \n")
      (djr/gmusicproxy-ensure)
      (let ((url (url-encode-url (format "http://localhost:9999/get_by_search?type=artist&artist=%s&num_tracks=100" artist-search))))
        (emms-play-streamlist url)))

    (defun djr/emms-play-the-chaconne ()
      (interactive)
      (djr/emms-play-least-heard-first
       "The Bach Chaconne"
       '(lambda ()
          (emms-add-directory "~/Music/the-chaconne"))))

    (defun djr/emms-start-local-music-or-pause ()
      (interactive)
      (if emms-player-playing-p
          (emms-pause)
        (djr/emms-play-least-heard-first
         "All My Music"
         '(lambda ()
            (emms-add-directory-tree "~/Music/mine")
            (emms-add-directory-tree "~/Music/other")
            (emms-add-directory-tree "~/Music/iTunes/iTunes Media/Music")))))

    (defun djr/emms-streaming-p ()
      (eq 'streamlist (emms-track-type (emms-playlist-current-selected-track))))

    (defun djr/emms-play-france-inter ()
      (interactive)
      (emms-play-streamlist "http://www.listenlive.eu/franceinter128.m3u"))

    (defun djr/emms-next ()
      (interactive)
      (if (djr/emms-streaming-p)
          (emms-player-mpv-stream-next)
        (emms-next)))

    (defun djr/emms-previous ()
      (interactive)
      (if (djr/emms-streaming-p)
          (emms-player-mpv-stream-previous)
        (emms-previous)))

    (defun djr/emms-play-least-heard-first (playlist-name playlist-generation-fun)
      (emms-stop)
      (emms-playlist-set-playlist-buffer (emms-playlist-new (concat " *EMMS: " playlist-name "*")))
      (funcall playlist-generation-fun)
      (emms-shuffle)
      (emms-playlist-sort-by-last-played)
      (with-current-emms-playlist
       (emms-playlist-mode-play-smart)))))

(provide 'djr-emms)
