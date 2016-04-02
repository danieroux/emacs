(use-package emms-setup
  :pin "melpa"
  :ensure emms
  :bind* (("<f7>" . djr/emms-previous)
          ("S-<f7>" . emms-stop)
          ("<f8>" . djr/emms-start-local-music-or-pause)
          ("S-<f8>" . djr/emms-start-google-music-ifl)
          ("<f9>" . djr/emms-next))
  :config
  (progn
    (use-package emms-player-mpv
      :ensure t
      :config

      (progn
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
            (call-process-shell-command cmd nil nil nil))))))

  ;; https://www.gnu.org/software/emms/manual/Using-TagLib.html
  ;; (require 'emms-info-libtag)
  ;; (setq emms-info-libtag-program-name "~/.emacs.d/external/emms/src/emms-print-metadata")
  ;; (setq emms-info-functions '(emms-info-libtag))

  (emms-all)
  (setq emms-source-file-default-directory "~/Music"))

;; Filters don't work for me. I want to script what I'm playing, not setup filters to apply
;; (emms-browser-make-filter "the-chaconne" (emms-browser-filter-only-dir "~/Music/the-chaconne"))

(defun djr/emms-play-the-chaconne ()
  (interactive)
  (djr/emms-play-least-heard-first
   "The Bach Chaconne"
   '(lambda ()
      (emms-add-directory "~/Music/the-chaconne"))))

(defvar *gmusic-proxy* nil)

(defun djr/start-gmusic-proxy ()
  (interactive)
  (let ((process-name (split-string g-music-proxy-process)))
    (when (and (not *gmusic-proxy*)
               (or (file-exists-p (car process-name))
                   (executable-find (car process-name))))
      (setq *gmusic-proxy*  (apply 'start-process (append (list "GMusicProxy" "*GMusicProxy*") process-name))))))

(defun djr/emms-start-google-music-ifl ()
  (interactive)
  (emms-play-streamlist "http://localhost:9999/get_ifl_station"))

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
