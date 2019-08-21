;;; erc-tweet.el --- shows text of a tweet when an url is posted in erc buffers

;; Copyright (C) 2012  Raimon Grau

;; Author: Raimon Grau <raimonster@gmail.com>
;; Version: 0.9
;; Package-Version: 20150920.1258
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
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
;;
;; Show inlined tweets in erc buffers.
;;
;;; Installation:
;;
;; usage:
;;
;; (require 'erc-tweet)
;; (add-to-list 'erc-modules 'tweet)
;; (erc-update-modules)
;;
;; Or `(require 'erc-tweet)` and  `M-x customize-option erc-modules RET`
;;
;; This plugin subscribes to hooks `erc-insert-modify-hook` and
;; `erc-send-modify-hook` to download and show tweets.

;;; Code:

(require 'erc)
(require 'url-queue)

(defgroup erc-tweet nil
  "Enable tweet."
  :group 'erc)

(defcustom erc-tweet-regex "https?://\\(?:[^/]*\\)?twitter.com/.+/status/[0-9]+"
  "Regex to mach URLs to be downloaded"
  :group 'erc-tweet
  :type '(regexp :tag "Regex"))

(defun erc-tweet-strip-tags (str)
  "Strip tags in a regex. Naive, I know."
  (replace-regexp-in-string "<.+?>" "" str))

(defun erc-tweet-text ()
  "Extract the tweet text from the retrieved HTML"
  (goto-char (point-min))
  (search-forward-regexp "js-tweet-text tweet-text[^>]*>")
  (let ((pt-before (point)))
    (search-forward "

")
    (backward-char)
    (string-as-multibyte (buffer-substring-no-properties pt-before (point)))))

(defvar erc-tweet-cleanup-text 'erc-tweet-strip-tags)


(defun erc-tweet-decode-entities (html)
  (with-temp-buffer
    (save-excursion (insert html))
    (xml-parse-string)))

(defun erc-tweet-insert (msg marker)
  "Insert MSG before MARKER."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (marker-position marker))
        (let ((pt-before (point)))
          (insert-before-markers (erc-tweet-decode-entities msg))
          (put-text-property pt-before (point) 'read-only t))))))

(defun erc-tweet-error (error-info marker)
  "Insert error text from ERROR-INFO before MARKER."
  (let* ((name (car error-info))
         (data (cadr error-info))
         (msg (format "[tweet/%s] - %s\n" name data)))
    (erc-tweet-insert msg marker)))

(defun erc-tweet-callback (status marker)
  "Callback function for url-queue-retrieve."
  (interactive)
  (let ((error-info (plist-get status :error)))
    (cond (error-info
           (erc-tweet-error error-info marker))
          (t (erc-tweet marker)))))

(defun erc-tweet (marker)
  "Extract the tweet text and insert before MARKER."
  (let* ((tweet-text (erc-tweet-text))
         (msg (concat "[tweet] - "
                      (funcall erc-tweet-cleanup-text tweet-text))))
    (erc-tweet-insert msg marker)))

(defun erc-tweet-correct-url (url)
  "Change the url to go to the non-mobile site."
  (when (and url (string-match erc-tweet-regex url))
    ;; go to the non-mobile tweet
    (replace-regexp-in-string "mobile\." "" url)))

(defun erc-tweet-show-tweet ()
  (interactive)
  (goto-char (point-min))
  (search-forward "http" nil t)
  (let ((url (erc-tweet-correct-url (thing-at-point 'url))))
    (when url
      (url-queue-retrieve url
                          'erc-tweet-callback
                          (list
                           (point-max-marker))
                          t))))

;;;###autoload
(eval-after-load 'erc
  '(define-erc-module tweet nil
     "Display inlined twits in ERC buffer"
     ((add-hook 'erc-insert-modify-hook 'erc-tweet-show-tweet t)
      (add-hook 'erc-send-modify-hook 'erc-tweet-show-tweet t))
     ((remove-hook 'erc-insert-modify-hook 'erc-tweet-show-tweet)
      (remove-hook 'erc-send-modify-hook 'erc-tweet-show-tweet))
     t))

;;; Code:

(provide 'erc-tweet)
;;; erc-tweet.el ends here
