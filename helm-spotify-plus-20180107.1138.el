;;; helm-spotify-plus.el --- Control Spotify search and select music with Helm.

;; Copyright (C)
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira> and Luis Moneda <https://github.com/lgmoneda>
;; Package: helm-spotify-plus
;; Package-Requires: ((emacs "24.4") (helm "2.0.0") (multi "2.0.1"))
;; Package-Version: 20180107.1138
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Makes it easier to browse Spotify API from Emacs.
;;; Code:


;;; API Reference: https://developer.spotify.com/technologies/web-api/
(require 'url)
(require 'json)
(require 'helm)
(require 'multi)
(require 'subr-x)

;;; Customs
(defgroup helm-spotify-plus nil
  "Customs for `helm-spotify-plus'"
  :group 'convenience)

(defcustom helm-spotify-plus-dbus-prefer-local t
  "Variable to define if DBUs interface should use the local machine by default over remote sessions."
  :type 'string :group 'helm-spotify-plus)

(defcustom helm-spotify-plus-market-region "US"
  "Variable to define what is the default market region.  Nil values to disable this filter."
  :type 'string :group 'helm-spotify-plus)

(defcustom helm-spotify-plus-page-number 5
  "Variable to control the number of pages of the requests.  5 pages with 50 candidates each, therefore there will be 250 candidates in your buffer."
  :type 'integer :group 'helm-spotify-plus)

      
(defun helm-spotify-plus-alist-get (symbols alist)
  "Look up the value for the chain of SYMBOLS in ALIST."
  (if symbols
      (helm-spotify-plus-alist-get (cdr symbols)
		 (assoc (car symbols) alist))
    (cdr alist)))

(defmulti helm-spotify-plus-play-href (href)
  "Get the Spotify app to play the object with the given HREF."
  system-type)

(defmulti-method helm-spotify-plus-play-href 'darwin
  (href)
  (shell-command (format "osascript -e 'tell application %S to play track %S'"
			 "Spotify"
			 href)))

(defvar helm-spotify-plus-dbus-call "dbus-send  --session --type=method_call --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 "
  "Variable to hold the dbus call string.")

(defmulti-method helm-spotify-plus-play-href 'gnu/linux
  (href)
  (if helm-spotify-plus-dbus-prefer-local
      (progn
	(call-process "/bin/bash" nil nil nil "-c" (concat helm-spotify-plus-dbus-call "org.mpris.MediaPlayer2.Player.Pause"))
	(call-process "/bin/bash" nil nil nil "-c" (format (concat helm-spotify-plus-dbus-call "org.mpris.MediaPlayer2.Player.OpenUri \"string:%s\"")
			 href)))
    (shell-command (concat helm-spotify-plus-dbus-call "org.mpris.MediaPlayer2.Player.Pause"))
    (shell-command (format (concat helm-spotify-plus-dbus-call "org.mpris.MediaPlayer2.Player.OpenUri \"string:%s\"")
			 href))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spotify controllers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun helm-spotify-plus-action (action)
  "Send a given ACTION to dbus."
  (if helm-spotify-plus-dbus-prefer-local
      (call-process "/bin/bash" nil nil nil "-c" (format (concat helm-spotify-plus-dbus-call "org.mpris.MediaPlayer2.Player.%s") action))
    (shell-command
     (format (concat helm-spotify-plus-dbus-call "org.mpris.MediaPlayer2.Player.%s") action))))

(defun helm-spotify-plus-action-darwin (action)
  "Send a given ACTION to osascript such as PLAY, PAUSE, NEXT"
  (let ((action-string (format "osascript -e 'tell application \"Spotify\" to %s'" action)))
    (shell-command action-string)))

(defun helm-spotify-plus-next ()
  "Play the next song."
  (interactive)
  (cond
   ((eq system-type 'gnu/linux)
    (helm-spotify-plus-action "Next"))
   ((eq system-type 'darwin)
    (helm-spotify-plus-action-darwin "next track"))
   ((eq system-type 'windows-nt)
    (message "Sorry, there is no support for Windows yet"))
   (t
    (message "Sorry, there is no support for your OS yet."))))

(defun helm-spotify-plus-pause ()
  "Pause the current song."
  (interactive)
  (cond
   ((eq system-type 'gnu/linux)
    (helm-spotify-plus-action "Pause"))
   ((eq system-type 'darwin)
    (helm-spotify-plus-action-darwin "pause"))
   ((eq system-type 'windows-nt)
    (message "Sorry, there is no support for Windows yet"))
   (t
    (message "Sorry, there is no support for your OS yet."))))

(defun helm-spotify-plus-play ()
  "Play a song."
  (interactive)
  (cond
   ((eq system-type 'gnu/linux)
    (helm-spotify-plus-action "Play"))
   ((eq system-type 'darwin)
    (helm-spotify-plus-action-darwin "play track"))
   ((eq system-type 'windows-nt)
    (message "Sorry, there is no support for Windows yet"))
   (t
    (message "Sorry, there is no support for your OS yet."))))

(defun helm-spotify-plus-previous ()
  "Plays previous song."
  (interactive)
  (cond
   ((eq system-type 'gnu/linux)
    (helm-spotify-plus-action "Previous"))
   ((eq system-type 'darwin)
    (helm-spotify-plus-action-darwin "previous track"))
   ((eq system-type 'windows-nt)
    (message "Sorry, there is no support for Windows yet"))
   (t
    (message "Sorry, there is no support for your OS yet."))))


(defun helm-spotify-plus-toggle-play-pause ()
  "Toggle between play and pause song."
  (interactive)
  (cond
   ((eq system-type 'gnu/linux)
    (helm-spotify-plus-action "PlayPause"))
   ((eq system-type 'darwin)
    (helm-spotify-plus-action-darwin "playpause"))
   ((eq system-type 'windows-nt)
    (message "Sorry, there is no support for Windows yet"))
   (t
    (message "Sorry, there is no support for your OS yet."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of spotify controllers definition. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar helm-spotify-plus-spotify-api-authentication-url "https://accounts.spotify.com/api/token")
(defvar helm-spotify-plus-client-key "515f0ff545a349bcadf98efab945972f")
(defvar helm-spotify-plus-client-secret "7618bf445df14b568782b13e37cf63e6")

(defun helm-spotify-plus-get-token ()
  "Get the token for the `helm-spotify-plus' Web App."
  (let ((url-request-method "POST")
        (url-request-data "&grant_type=client_credentials")
        (url-request-extra-headers
         `(("Content-Type" . "application/x-www-form-urlencoded")
           ("Authorization" . ,(concat "Basic " (base64-encode-string (concat helm-spotify-plus-client-key ":" helm-spotify-plus-client-secret) t))))))
    (with-current-buffer
        (url-retrieve-synchronously helm-spotify-plus-spotify-api-authentication-url)
      (goto-char url-http-end-of-headers)
      (let* ((response (json-read))
             (token-type (alist-get 'token_type response))
             (token (alist-get 'access_token response)))
        (cons token-type token)))))

(defmulti-method helm-spotify-plus-play-href 'windows-nt
  (href)
  (shell-command (format "explorer %S" href)))

(defmulti-method-fallback helm-spotify-plus-play-href
  (href)
  (message "Sorry, helm-spotify does not support playing tracks on %S." system-type))

(defun helm-spotify-plus-play-track (track)
  "Get the Spotify app to play the TRACK."
  (helm-spotify-plus-play-href (helm-spotify-plus-alist-get '(uri) track)))


(defun helm-spotify-plus-play-album (track)
  "Get the Spotify app to play the album for this TRACK."
  (let ((album-uri (helm-spotify-plus-alist-get '(album uri) track)))
    (helm-spotify-plus-play-href album-uri)))


(defvar helm-spotify-plus-limit 50
  "Magic number to control the limit of candidates that Spotify API allows per request.")


(defun helm-spotify-plus-improved-search-formatted (search-term)
  "Improved version of the out spotify-search formatted using the SEARCH-TERM."
  (let ((final-list '()))
    (dotimes (counter helm-spotify-plus-page-number final-list)
      (setq final-list (append final-list (helm-spotify-plus-search-formatted-helper search-term counter))))))


(defun helm-spotify-plus-search-formatted-helper (search-term counter)
  "Helper function to format the output due to SEARCH-TERM and COUNTER."
  (mapcar (lambda (track)
            (cons (helm-spotify-plus-format-track track) track))
          (helm-spotify-plus-alist-get '(tracks items) (helm-spotify-plus-artist-track-search search-term counter))))

(defun helm-spotify-plus-insert-market-region-url (new-url market-region)
  "Function to insert in the NEW-URL a value passed as MARKET-REGION."
   (cond
    (market-region
     (string-trim (concat new-url (format "&market=%s" market-region))))
    (t
     (string-trim (concat new-url (format "&market=%s" helm-spotify-plus-market-region))))))

(defun helm-spotify-plus-artist-track-search (search-term counter)
  "Function to get the current match between the SEARCH-TERM and amount of requests defined by COUNTER."
  (let ((offset (* helm-spotify-plus-limit counter))
        (market-region (helm-spotify-plus-split-string "m" search-term))
        (url-default "https://api.spotify.com/v1/search?q=%s&type=track&limit=%s&offset=%d"))
    (cond
     
     ((and (string-match "a:" search-term) (string-match "t:" search-term)) ;both the artist and track name are available
      (let* ((artist-name (helm-spotify-plus-split-string "a" search-term))
             (track-name (helm-spotify-plus-split-string "t" search-term))
             (new-url (format "https://api.spotify.com/v1/search?q=%s artist:%s&type=track&limit=%s&offset=%d"
                              track-name artist-name helm-spotify-plus-limit offset)))
        (if helm-spotify-plus-market-region
            (helm-spotify-plus-request (helm-spotify-plus-insert-market-region-url new-url market-region))
          (helm-spotify-plus-request new-url))))
          
     ((string-match "a:" search-term)	;only the artist name was given
      (let* ((artist-name (helm-spotify-plus-split-string "a" search-term))
             (new-url (format "https://api.spotify.com/v1/search?q=artist:%s&type=track&limit=%s&offset=%d" artist-name
                              helm-spotify-plus-limit offset)))
        (if helm-spotify-plus-market-region
            (helm-spotify-plus-request (helm-spotify-plus-insert-market-region-url new-url market-region))
          (helm-spotify-plus-request new-url))))
     
     ((string-match "t:" search-term)	; only the track name was given
      (let* ((track-name (helm-spotify-plus-split-string "t" search-term))
            (new-url (format "https://api.spotify.com/v1/search?q=%s&type=track&limit=%s&offset=%d" track-name
                             helm-spotify-plus-limit offset)))
        (if helm-spotify-plus-market-region
            (helm-spotify-plus-request (helm-spotify-plus-insert-market-region-url new-url market-region))
          (helm-spotify-plus-request new-url))))
     
     (t					;Else case... do a regular search for the track name
      (if helm-spotify-plus-market-region
          (if (string-match "m:" search-term)
              (let* ((search-term-filtered (string-trim (car (split-string search-term "m:"))))
                     (new-url (format url-default search-term-filtered helm-spotify-plus-limit offset))
                     (new-url-market (helm-spotify-plus-insert-market-region-url new-url market-region)))
                (helm-spotify-plus-request new-url-market))
            (helm-spotify-plus-request (helm-spotify-plus-insert-market-region-url (format url-default search-term
                                                                                           helm-spotify-plus-limit offset) market-region)))
        (helm-spotify-plus-request (format url-default search-term helm-spotify-plus-limit offset)))))))
            

(defun helm-spotify-plus-split-string (letter search-term)
  "Function to split based in the LETTER using the SEARCH-TERM."
  (if (string-match (format "%s:" letter) search-term)
      (let* ((delimiter (format ".*%s:" letter))
             (name-tmp (car (cdr (split-string search-term delimiter))))
             (name (car (split-string name-tmp " [a-z]:"))))
        (string-trim name))
    nil))

(defun helm-spotify-plus-request (a-url)
  "Function to request an json given a correct A-URL."
  (let* ((token (helm-spotify-plus-get-token))
         (access-token (cdr token))
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " access-token)))))
    (with-current-buffer
        (url-retrieve-synchronously a-url)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun helm-spotify-plus-decode-utf8 (string)
  "Function to decode the STRING due to the errors in some symbols."
  (decode-coding-string (string-make-unibyte string) 'utf-8))


(defun helm-spotify-plus-format-track (track)
  "Given a TRACK, return a a formatted string suitable for display."
  (let ((track-name   (helm-spotify-plus-decode-utf8 (helm-spotify-plus-alist-get '(name) track)))
	(track-length (/ (helm-spotify-plus-alist-get '(duration_ms) track) 1000))
	(album-name  (helm-spotify-plus-decode-utf8 (helm-spotify-plus-alist-get '(album name) track)))
	(artist-names (mapcar (lambda (artist)
				(helm-spotify-plus-decode-utf8 (helm-spotify-plus-alist-get '(name) artist)))
			      (helm-spotify-plus-alist-get '(artists) track))))
    (format "%s (%dm%0.2ds)\n%s - %s"
	    track-name
	    (/ track-length 60) (mod track-length 60)
	    (mapconcat 'identity artist-names "/")
	    album-name)))


(defun helm-spotify-plus-search (search-term)
  "Improved SEARCH-TERM."
  (helm-spotify-plus-improved-search-formatted search-term))


(defun helm-spotify-plus-actions-for-track (actions track)
  "Return a list of helm ACTIONS available for this TRACK."
  `((,(format "Play Track - %s" (helm-spotify-plus-decode-utf8 (helm-spotify-plus-alist-get '(name) track)))       . helm-spotify-plus-play-track)
    (,(format "Play Album - %s" (helm-spotify-plus-decode-utf8 (helm-spotify-plus-alist-get '(album name) track))) . helm-spotify-plus-play-album)
    ("Show Track Metadata" . pp)))


(defun helm-spotify-plus-get-search-string ()
  "Function to require an input string for the user."
  (read-string "Enter the (partial/full) name of an Track: "))

;;;###autoload
(defun helm-spotify-plus ()
  "Brind up a custom PROMPT asking for the name of the Artist to perform the search and them all the candidates ready to be narrowed."
  (interactive)
  (helm :sources (helm-build-sync-source "Spotify"
		   :init (setq search-string (helm-spotify-plus-get-search-string))
		   :candidates (helm-spotify-plus-search search-string)
		   :multiline t
		   :action-transformer
		   (lambda (actions track)
		     (helm-spotify-plus-actions-for-track actions track)))
        :buffer "*helm-spotify*"))

(provide 'helm-spotify-plus)


;;; helm-spotify-plus.el ends here
