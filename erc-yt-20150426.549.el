;;; erc-yt.el --- An erc module to display youtube links nicely

;; Copyright (C) 2015 William Stevenson

;; Author: William Stevenson <yhvh2000@gmail.com>
;; Version: 0.1
;; Package-Version: 20150426.549
;; Keywords: multimedia
;; Package-Requires: ((dash "2.10.0"))

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; An erc module to display youtube links with clickable thumbnails,
;; description and title. Uses api v3.
;; Inspired by, and containing code from:
;; https://github.com/kidd/erc-youtube.el

;;; Usage:

;; (require 'erc-yt)
;; (add-to-list 'erc-modules 'youtube)
;; (erc-update-modules)

;; Or (require 'erc-yt) and M-x customize-option erc-modules RET
;; and insert module name 'youtube' in 'Others'

;;; Code:
(require 'erc)
(require 'json)
(require 'dash)

(defgroup erc-yt nil
  "Top level group for erc-yt customization. "
  :group 'erc)


(defcustom erc-yt-cache-dir "/tmp/erc-yt/"
  "Directory to cache image file into."
  :group 'erc-yt
  :type 'directory)


(defcustom erc-yt-browse-function 'browse-url
  "Function to call when thumbnail is clicked."
  :group 'erc-yt
  :type 'function)

(defcustom erc-yt-max-description-lines 5
  "Maximum amount of lines of description to display."
  :group 'erc-yt
  :type 'integer)


(defconst erc-yt-api-key "AIzaSyCLaxXAE5oArXxF017M3jdilusOeDjbLfY"
  "Api key.")

(defconst erc-yt-regex-extract-videoid
  (concat
   ;; Slurp the possible youtube domains and any other parameters
   ;; before the Video ID, to ignore it.
   "^\\(?:https?:\\/\\/\\)?\\(?:www\\.\\)?"
   "\\(?:youtu\\.be\\/\\|youtube\\.com\\/\\(?:embed\\/\\|v\\/\\|watch\\?v=\\|watch\\?.+&v=\\)\\)"
   ;; Match the Video ID which is currently 11 characters of [-_A-Za-z0-9]
   ;; and save it as the first match group.
   "\\(?1:[-_A-Za-z0-9]\\{11\\}\\)"
   ;; Slurp up the rest of the url to ignore it
   "\\(?:[^-_A-Za-z0-9]?.*\\)$"
   )
  "Emacs 24.3 style regexp to extract the Video ID of a Youtube URL.

This regexp is used internally to check and extract the url from
a IRC buffer and to make API request URLs.

A Youtube URL has many patterns, including http://youtu.be/<video:id> and
https://....youtube.com/...?v=<video:id>.

The Video ID is currently defined as a 11 digit string of
alphanumeric characters, hyphens and underscores. The matched
Video ID can be referenced as the first regexp group result.

This regexp is based on the javascript regexp provided by user
eyecatchup from Stackoverflow.com. Greetz and howdy.
http://stackoverflow.com/a/10315969/28524
http://stackoverflow.com/users/624466/eyecatchup")


(defun assoc-rec (alist &rest keys)
  "Recursively search ALIST for KEYS."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)


(defun erc-yt-id (url)
  "Extract and return the Youtube Video ID from the string URL."
  (replace-regexp-in-string erc-yt-regex-extract-videoid "\\1" url))


(defun erc-yt-api-format (video-id)
  "Format api string using VIDEO-ID."
  (format "https://www.googleapis.com/youtube/v3/videos?&id=%s&key=%s&part=snippet"
          video-id erc-yt-api-key))


(defun erc-yt-fetch-json (url)
  "Fetch json data from URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (set-buffer-multibyte t)
    (goto-char url-http-end-of-headers)
    (json-read)))


(defun erc-yt-request (url)
  "Do youtube api call and extract relevant data for video at URL."
  (let* ((video-id (erc-yt-id url))
         (data (erc-yt-fetch-json (erc-yt-api-format (erc-yt-id url))))
         (items (elt (assoc-default 'items data) 0))
         (title (assoc-rec items 'snippet 'title))
         (description (assoc-rec items 'snippet 'description))
         (thumb (assoc-rec items 'snippet 'thumbnails 'high 'url)))
    `((title . ,title)
      (description . ,(mapconcat
                       'identity
                       (-slice (split-string description "\n")
                               0 erc-yt-max-description-lines) "\n"))
      (thumb . ,thumb)
      (id . ,video-id))))


(defun erc-yt-thumb-download (thumb video-id)
  "Download THUMB for VIDEO-ID unless already cached."
 (unless (file-exists-p erc-yt-cache-dir)
   (make-directory erc-yt-cache-dir))
 (let ((name (concat erc-yt-cache-dir video-id ".jpg")))
   (if (file-exists-p name)
       name
     (url-copy-file thumb name)
     name)))

;;;###autoload
(defun erc-yt-show-info ()
  "Replace youtube links in erc buffers with title, description and thumbnail."
  (interactive)
  (goto-char (point-min))
  (search-forward "http" nil t)
  (let ((url (thing-at-point 'url)))
    (when (and url (string-match erc-yt-regex-extract-videoid url))
      (goto-char (point-max))
      (save-excursion
        (let* ((inhibit-read-only t)
               (data (erc-yt-request url))
               (yt-image (erc-yt-thumb-download
                          (assoc-default 'thumb data)
                          (assoc-default 'id data))))
          (insert "\n")
          ;; Insert image and make clickable
          (let* ((pt (point))
                 (pt-2 (save-excursion
                         (insert-image
                          (create-image yt-image))
                         (point)))
                 (yt-map (make-sparse-keymap))
                 (ov (make-overlay pt pt-2)))
            (define-key yt-map [mouse-1]
              `(lambda () (interactive) (,erc-yt-browse-function ,url)))
            (overlay-put ov 'keymap yt-map)
            (overlay-put ov 'mouse-face 'highlight)
            (forward-char))
          (insert "\n"
                  (assoc-default 'title data)
                  "\n"
                  (assoc-default 'description data)
                  "\n"))))))

;;;###autoload
(eval-after-load 'erc
  '(define-erc-module youtube nil
     "Display clickable youtube thumbnails inline. "
     ((add-hook 'erc-insert-modify-hook 'erc-yt-show-info t)
      (add-hook 'erc-send-modify-hook 'erc-yt-show-info t))
     ((remove-hook 'erc-insert-modify-hook 'erc-yt-show-info)
      (remove-hook 'erc-send-modify-hook 'erc-yt-show-info))))


(provide 'erc-yt)

;;; erc-yt.el ends here
