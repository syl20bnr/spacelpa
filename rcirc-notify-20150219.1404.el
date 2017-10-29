;;; rcirc-notify.el --- libnotify popups

;; Copyright (c) 2008 Will Farrington
;; Copyright (c) 2009, 2011 Alex Schroeder <alex@gnu.org>

;; Author: Will Farrington, Alex Schroeder <alex@gnu.org>, Nic Ferrier <nferrier@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Created: 13th October 2011
;; Version: 1.0.0
;; Package-Version: 20150219.1404
;; Keywords: lisp, rcirc, irc, notify, growl

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Changelog:
;; * 2014/11/18 - Improved notify-send support by passing correct flags.
;;
;; * 2014/10/08 - Added support for terminal-notifier,
;;                added rcirc-notify-page-me-hooks,
;;                removed broken notification-check-frame feature,
;;                improved README and fixed "cl package required
;;                at runtime" warning.
;;
;; * 2013/09/04 - Add support for terminal-notifier.
;;
;; * 2011/10/13 - Clean up the namespace, add customization, prevent
;;                notifys if you have the rcirc buffer open in a frame
;;
;; * 2011/04/13 - Support for Growl on Windows; support for
;;                rcirc-keywords.
;;
;; * 2011/01/31 - Fix two warnings -Stefan Kangas
;;
;; * 2009/10/17 - Added support for osascript which is a Mac OS X
;;                what Mac OS 10.4 and Growl 1.1.6 require.
;;
;; * 2009/02/23 - Added support for growlnotify which is a Mac OS X
;;                notification tool.  http://growl.info -Shane Celis
;;
;; * 2008/12/29 - Fix annoying bug where the user gets notified
;;                for every PRIVMSG and added new variable specifying
;;                format of message when a PRIVMSG is received
;;
;; * 2008/02/11 - Fix an annoying bug where the user got
;;                notified every message
;;
;; * 2008/02/11 - First release


;;; Commentary:
;;
;; This code is inspired in part by erc-page-me.el and offers
;; the same functionality as it, but for rcirc.
;;
;; * `rcirc-notify-message` contains the message contents for
;;    the notification
;;
;; * `rcirc-notify-message-private` contains the message
;;    contents for a private message notification
;;
;; * `rcirc-notify-nick-alist` is a list containing the last
;;    folks who notified you, and what times they did it at
;;
;; * `rcirc-notify-timeout` controls the number of seconds
;;    in between notifications from the same nick.

;; Grow For Windows
;; Run something like this from eshell before you use rcirc-notify:
;; /Programme/Growl\ for\ Windows/growlnotify.com /t:IRC \
;; /ai:http://www.emacswiki.org/pics/static/CarbonEmacsPackageIcon.png \
;; /a:Emacs /r:IRC /n:IRC foo

;;; Code:

(require 'rcirc)

(defgroup rcirc-notify nil
  "Notifications for the rcirc IRC client."
  :group 'rcirc
  )

(defcustom rcirc-notify-message "%s mentioned you: %s"
  "Format of the message to display in the popup.
The first %s will expand to the nick that notified you,
the second %s (if any) will expand to the message text itself."
  :type '(string)
  :group 'rcirc-notify
  )

(defcustom rcirc-notify-keywords t
  "Non-nil means matches of `rcirc-keywords' will result in notification.
See `rcirc-notify-keyword' for the message format to use."
  :type '(boolean)
  :group 'rcirc-notify
  )

(defcustom rcirc-notify-keyword "%s mentioned the keyword '%s': %s"
  "Format of the message to display in the popup.
The first %s will expand to the nick that mentioned the keyword,
the second %s (if any) will expand to the keyword used,
the third %s (if any) will expand to the message text itself.
This only happens if `rcirc-notify-keywords' is non-nil."
  :type '(string)
  :group 'rcirc-notify
  )

(defcustom rcirc-notify-message-private "%s sent a private message: %s"
  "Format of the message to display in the popup.
The first %s will expand to the nick that notified you,
the second %s (if any) will expand to the message text itself."
  :type '(string)
  :group 'rcirc-notify
  )

(defcustom rcirc-notify-popup-timeout 8640000
  "Number of seconds to show the notifcation popup, if relevant.
If the notification is done via an operating system popup message
then this controls the timeout of that popup."
  :type '(integer)
  :group 'rcirc-notify
  )

(defcustom rcirc-notify-timeout 60
  "Seconds between notifications from the same person."
  :type '(integer)
  :group 'rcirc-notify
  )

(defvar rcirc-notify--nick-alist nil
  "An alist of nicks and the last time they tried to trigger a notification."
  )

(defvar rcirc-notify-page-me-hooks nil
  "Hook functions called when rcirc-notify sends a notification.")

(defun rcirc-notify-page-me (msg)
  (run-hook-with-args 'rcirc-notify-page-me-hooks msg)
  (cond
    ((executable-find "notify-send")
     (start-process "page-me" nil
                    ;; 8640000 ms = 1 day
                    "notify-send" "-u" "normal" "-i" "emacs" "-a" "emacs"
                    "-c" "im.received"
                    "-t" (format "%s" rcirc-notify-popup-timeout) "rcirc"
                    msg))
    ((executable-find "terminal-notify")
     (start-process "page-me" "*debug*" "terminal-notify" "-activate" "org.gnu.Emacs" "-message" msg))
    ((executable-find "terminal-notifier")
     (start-process "page-me" "*debug*" "terminal-notifier" "-title" "rcirc" "-sender" "org.gnu.Emacs" "-activate" "org.gnu.Emacs" "-message" msg))
    ((executable-find "growlnotify.exe")
     (start-process "page-me" "*debug*" "growlnotify.exe" "/a:Emacs" "/n:IRC" msg))
    ((executable-find "growlnotify")
     (start-process "page-me" "*debug*" "growlnotify" "-a" "Emacs" "-m" msg))
    ((executable-find "osascript")
     (apply 'start-process `("page-me" nil
                             "osascript"
                             "-e" "tell application \"GrowlHelperApp\""
                             "-e" "register as application \"Emacs\" all notifications {\"rcirc\"} default notifications {\"rcirc\"}"
                             "-e" ,(concat "notify with name \"rcirc\" title \"rcirc\" description \""
                                           msg "\" application name \"Emacs\"")
                             "-e" "end tell")))
    (t (error "No method available to page you."))))


(defun rcirc-notify-page-test ()
  "Test the notify system."
  (interactive)
  (rcirc-notify-page-me (format "Hi %s, it's %s"
                                user-full-name (current-time-string))))


(defun rcirc-notify (sender &optional text)
  (when window-system
    ;; Set default dir to appease the notification gods
    (let ((default-directory "~/"))
      (rcirc-notify-page-me (format rcirc-notify-message sender text)))))

(defun rcirc-notify-keyword (sender &optional keyword text)
  (when window-system
    ;; Set default dir to appease the notification gods
    (let ((default-directory "~/"))
      (rcirc-notify-page-me (format rcirc-notify-keyword sender keyword text)))))

(defun rcirc-notify-private (sender &optional text)
  (when window-system
    ;; Set default dir to appease the notification gods
    (let ((default-directory "~/"))
      (rcirc-notify-page-me (format rcirc-notify-message-private sender text)))))

(defun rcirc-notify-allowed (nick &optional delay)
  "Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`rcirc-notify-timeout'."
  (unless delay (setq delay rcirc-notify-timeout))
  (let ((cur-time (time-to-seconds (current-time)))
        (cur-assoc (assoc nick rcirc-notify--nick-alist))
        (last-time))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons nick cur-time) rcirc-notify--nick-alist)
      t)))

;;;###autoload
(defun rcirc-notify-me (proc sender response target text)
  "Notify the current user when someone sends a message that
matches the current nick."
  (interactive)
  (when (and (not (string= (rcirc-nick proc) sender))
             (not (string= (rcirc-server-name proc) sender))
             (rcirc-channel-p target)
             (rcirc-notify-allowed sender))
    (cond ((string-match (rcirc-nick proc) text)
           (rcirc-notify sender text))
          (rcirc-notify-keywords
           (let ((keyword (catch 'match
                            (dolist (key rcirc-keywords)
                              (when (string-match (concat "\\<" key "\\>")
                                                  text)
                                (throw 'match key))))))
             (when keyword
               (rcirc-notify-keyword sender keyword text)))))))

;;;###autoload
(defun rcirc-notify-privmsg (proc sender response target text)
  "Notify the current user when someone sends a private message
to them."
  (interactive)
  (when (and (string= response "PRIVMSG")
             (not (string= sender (rcirc-nick proc)))
             (not (rcirc-channel-p target))
             (rcirc-notify-allowed sender))
    (rcirc-notify-private sender text)))

;;;###autoload
(defun rcirc-notify-add-hooks ()
  "Initialize rcirc-notify into rcirc with hooks."
  (interactive)
  (add-hook 'rcirc-print-hooks 'rcirc-notify-privmsg)
  (add-hook 'rcirc-print-hooks 'rcirc-notify-me))

(provide 'rcirc-notify)

;;; rcirc-notify.el ends here
