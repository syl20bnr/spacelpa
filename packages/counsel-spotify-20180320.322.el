;;; counsel-spotify.el --- Control Spotify search and select music with Ivy.

;; Copyright (C)
;; Author: Lautaro García <https://github.com/Lautaro-Garcia>
;; Package: counsel-spotify
;; Package-Requires: ((emacs "25") (ivy "0.9.0"))
;; Package-Version: 20180320.322
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

(require 'url)
(require 'json)
(require 'ivy)

(defgroup  counsel-spotify nil
  "Customs for `counsel-spotify'"
  :group 'applications)

(defcustom counsel-spotify-spotify-api-url "https://api.spotify.com/v1"
  "Variable to define spotify API url."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-spotify-api-authentication-url "https://accounts.spotify.com/api/token"
  "Variable to define spotify API url for getting the access token."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-client-id ""
  "Spotify application client ID."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-client-secret ""
  "Spotify application client secret."
  :type 'string :group 'counsel-spotify)


;;;;;;;;;;;;;
;; Helpers ;;
;;;;;;;;;;;;;

(defun counsel-spotify-verify-credentials ()
  "Tell the user that the credentials are not set."
  (when (or (string= counsel-spotify-client-id "") (string= counsel-spotify-client-secret ""))
    (error "The variables counsel-spotify-client-id or counsel-spotify-client-secret are undefined and both are required to authenticate to the Spotify API.  See https://developer.spotify.com/my-applications")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spotify API integration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass counsel-spotify-playable ()
  ((uri :initarg :uri :initform "" :reader uri)))

(defclass counsel-spotify-album (counsel-spotify-playable)
  ((name :initarg :name :initform "" :reader name)
   (artist-name :initarg :artist-name :initform "" :reader artist-name)))

(defclass counsel-spotify-track (counsel-spotify-playable)
  ((name :initarg :name :initform "" :reader name)
   (artist :initarg :artist :initform "" :reader artist)
   (album :initarg :album :initform "" :reader album)
   (duration-in-ms :initarg :duration :initform 0 :reader duration-in-ms)))

(defclass counsel-spotify-artist (counsel-spotify-playable)
  ((name :initarg :name :initform "" :reader name)))

(defun counsel-spotify-request (url)
  "Make a request to the Spotify API to the given URL, handling authentication."
  (let ((url-request-extra-headers `(("Authorization" . ,(concat "Bearer " (counsel-spotify-get-token url))))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun counsel-spotify-get-token (url)
  "Make a spotify request to the given URL with proper authentication."
  (let ((url-request-method "POST")
        (url-request-data "&grant_type=client_credentials")
        (url-request-extra-headers
         `(("Content-Type" . "application/x-www-form-urlencoded")
           ("Authorization" . ,(concat "Basic " (base64-encode-string (concat counsel-spotify-client-id ":" counsel-spotify-client-secret) t))))))
    (with-current-buffer
        (url-retrieve-synchronously counsel-spotify-spotify-api-authentication-url)
      (goto-char url-http-end-of-headers)
      (let* ((response (json-read))
             (token (alist-get 'access_token response)))
        token))))

(cl-defun counsel-spotify-query (&key album artist track (type 'track))
  (if (or artist album track)
      (concat counsel-spotify-spotify-api-url
              "/search?q="
              (when artist (format "artist:%s" artist))
              (when album (format " album:%s" album))
              (when track (format " track:%s" track))
              (when type (format "&type=%s" (symbol-name type))))
    (error "Must supply at least an artist or an album or a track to search for")))

(cl-defun counsel-spotify-search-query (&rest rest)
  (counsel-spotify-builder (or (plist-get rest :type) 'track) (counsel-spotify-request (apply #'counsel-spotify-query rest))))

(cl-defgeneric counsel-spotify-builder (type spotify-alist-response)
  "Builds the TYPE object from the SPOTIFY-ALIST-RESPONSE")

(cl-defmethod counsel-spotify-builder ((type (eql track)) spotify-alist-response)
  (mapcar
   (lambda (tr)
     (let ((artist (elt (alist-get 'artists tr) 0))
           (album (alist-get 'album tr)))
       (make-instance 'counsel-spotify-track
                      :name (alist-get 'name tr)
                      :uri (alist-get 'uri tr)
                      :artist (make-instance 'counsel-spotify-artist :name (alist-get 'name artist) :uri (alist-get 'uri artist))
                      :album (make-instance 'counsel-spotify-album :name (alist-get 'name album) :artist-name (alist-get 'name artist) :uri (alist-get 'uri album))
                      :duration (alist-get 'duration_ms tr))))
   (alist-get 'items (alist-get 'tracks spotify-alist-response))))

(cl-defmethod counsel-spotify-builder ((type (eql album)) spotify-alist-response)
  (mapcar
   (lambda (a) (make-instance 'counsel-spotify-album
                         :name (alist-get 'name a)
                         :artist-name (alist-get 'name (elt (alist-get 'artists a) 0))
                         :uri (alist-get 'uri a)))
   (alist-get 'items (alist-get 'albums spotify-alist-response))))

(cl-defmethod counsel-spotify-builder ((type (eql artist)) spotify-alist-response)
  (mapcar
   (lambda (a) (make-instance 'counsel-spotify-artist :name (alist-get 'name a) :uri (alist-get 'uri a)))
   (alist-get 'items (alist-get 'artists spotify-alist-response))))


;;;;;;;;;;;;;;;;;
;; OS backends ;;
;;;;;;;;;;;;;;;;;

(defclass counsel-spotify-backend-commands ()
  ((play-command :initarg :play :initform "" :reader play)
   (playpause-command :initarg :playpause :initform "" :reader playpause)
   (next-command :initarg :next :initform "" :reader next)
   (previous-command :initarg :previous :initform "" :reader previous)))

(defclass counsel-spotify-backend ()
  ((commands :initarg :commands :reader commands)))

(defclass counsel-spotify-darwin-backend (counsel-spotify-backend)
  ((commands :initform (make-instance 'counsel-spotify-backend-commands :play "play track" :playpause "playpause" :next "next track" :previous "previous track"))))

(defclass counsel-spotify-linux-backend (counsel-spotify-backend)
  ((commands :initform (make-instance 'counsel-spotify-backend-commands :play "Play" :playpause "PlayPause" :next "Next" :previous "Previous"))))

(defvar counsel-spotify-current-backend
  (pcase system-type
    ('gnu/linux (make-instance 'counsel-spotify-linux-backend))
    ('darwin    (make-instance 'counsel-spotify-darwin-backend))))

(cl-defgeneric counsel-spotify-tell-backend-to (backend action)
  "Tells the given BACKEND to execute the given ACTION")

(cl-defmethod counsel-spotify-tell-backend-to ((backend counsel-spotify-darwin-backend) action)
  (shell-command (concat "osascript -e 'tell application \"Spotify\" to '" (shell-quote-argument (funcall action (commands backend))))))

(defconst counsel-spotify-dbus-call "dbus-send  --session --type=method_call --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 "
  "Variable to hold the dbus call string.")

(cl-defmethod counsel-spotify-tell-backend-to ((backend counsel-spotify-linux-backend) action)
  (shell-command (concat counsel-spotify-dbus-call "org.mpris.MediaPlayer2.Player." (shell-quote-argument (funcall action (commands backend))))))

(cl-defgeneric counsel-spotify-do-play (backend playable)
  "Tells the BACKEND to play the PLAYABLE object")

(cl-defmethod counsel-spotify-do-play ((backend counsel-spotify-darwin-backend) (playable counsel-spotify-playable))
  (shell-command (concat "osascript -e 'tell application \"Spotify\" to play track \"" (uri playable) "\"'")))

(cl-defmethod counsel-spotify-do-play ((backend counsel-spotify-linux-backend) (playable counsel-spotify-playable))
  (shell-command (concat counsel-spotify-dbus-call "org.mpris.MediaPlayer2.Player.OpenUri \"string:" (uri playable) "\"")))

(defun counsel-spotify-unwrap-property (elem)
  "Unwrap the property of ELEM."
  (get-text-property 0 'property elem))

(defun counsel-spotify-play-property (elem)
  "Call play on an unwrapped ELEM."
  (counsel-spotify-do-play counsel-spotify-current-backend (counsel-spotify-unwrap-property elem)))


;;;;;;;;;;;;;;;;;;;;;;;
;; Formatting output ;;
;;;;;;;;;;;;;;;;;;;;;;;

(cl-defgeneric counsel-spotify-format (element)
  "Format an ELEMENT to be shown in the minibuffer")

(cl-defmethod counsel-spotify-format :around (element)
  (decode-coding-string (string-make-unibyte (cl-call-next-method)) 'utf-8))

(cl-defmethod counsel-spotify-format ((track counsel-spotify-track))
  (let* ((seconds-of-song (/ (duration-in-ms track) 1000.0))
         (second-left-in-song (% (round seconds-of-song) 60))
         (minutes-in-song (truncate (/ seconds-of-song 60))))
    (format "(%d:%0.2d) %s - %s [%s]"
            minutes-in-song
            second-left-in-song
            (name (artist track))
            (name track)
            (name (album track)))))

(cl-defmethod counsel-spotify-format ((artist counsel-spotify-artist))
  (name artist))

(cl-defmethod counsel-spotify-format ((album counsel-spotify-album))
  (concat (artist-name album) " - " (name album)))

(defun counsel-spotify-get-formatted-object (element)
  "Return an string representation and it's corresponding ELEMENT as a property."
  (propertize (counsel-spotify-format element) 'property element))


;;;;;;;;;;;;;;;;;
;; Controllers ;;
;;;;;;;;;;;;;;;;;

(defun counsel-spotify-play ()
  "Start playing current track."
  (interactive)
  (counsel-spotify-tell-backend-to counsel-spotify-current-backend #'play))

(defun counsel-spotify-toggle-play-pause ()
  "Toggle play or pause of the current track."
  (interactive)
  (counsel-spotify-tell-backend-to counsel-spotify-current-backend #'playpause))

(defun counsel-spotify-previous ()
  "Start playing previous song."
  (interactive)
  (counsel-spotify-tell-backend-to counsel-spotify-current-backend #'previous))

(defun counsel-spotify-next ()
  "Start playing next song."
  (interactive)
  (counsel-spotify-tell-backend-to counsel-spotify-current-backend #'next))


;;;;;;;;;;;;;;;;;;;
;; Ivy interface ;;
;;;;;;;;;;;;;;;;;;;

(defmacro counsel-spotify-search-by-term (search-keyword &rest search-args)
  "Macro that create the function to search by SEARCH-KEYWORD and other SEARCH-ARGS."
  `(lambda (search-term) (mapcar #'counsel-spotify-get-formatted-object (counsel-spotify-search-query ,search-keyword search-term ,@search-args))))

(defun counsel-spotify-search-track ()
  "Bring Ivy frontend to choose and play a track."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search track: " (counsel-spotify-search-by-term :track)
            :dynamic-collection t
            :action '(1
                      ("p" counsel-spotify-play-property "Play track")
                      ("a" (lambda (elem) (counsel-spotify-do-play counsel-spotify-current-backend (album (counsel-spotify-unwrap-property elem)))) "Play album")
                      ("A" (lambda (elem) (counsel-spotify-do-play counsel-spotify-current-backend (artist (counsel-spotify-unwrap-property elem)))) "Play artist"))))

(defun counsel-spotify-search-artist ()
  "Bring Ivy frontend to choose and play an artist."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Seach artist: " (counsel-spotify-search-by-term :artist :type 'artist) :dynamic-collection t :action #'counsel-spotify-play-property))

(defun counsel-spotify-search-album ()
  "Bring Ivy frontend to choose and play an album."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search album: " (counsel-spotify-search-by-term :album :type 'album) :dynamic-collection t :action #'counsel-spotify-play-property))

(defun counsel-spotify-search-tracks-by-artist ()
  "Bring Ivy frontend to search for all tracks for a given artist."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search tracks by artist: " (counsel-spotify-search-by-term :artist :type 'track) :dynamic-collection t :action #'counsel-spotify-play-property))

(defun counsel-spotify-search-tracks-by-album ()
  "Bring Ivy frontend to search for all track on a given album."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search tracks by album: " (counsel-spotify-search-by-term :album :type 'track) :dynamic-collection t :action #'counsel-spotify-play-property))

(provide 'counsel-spotify)
;;; counsel-spotify.el ends here
