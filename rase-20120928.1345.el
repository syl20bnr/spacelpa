;;; rase.el --- Run At Sun Event daemon
;;; -*- lexical-bind: t -*-

;; Author   : Andrey Kotlarski <m00naticus@gmail.com>
;; URL      : https://github.com/m00natic/rase/
;; Package-Version: 20120928.1345
;; Version  : 1.1
;; Keywords : solar, sunrise, sunset, midday, midnight

;; rase.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; rase.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This is an Emacs extension that allows user provided set of
;; functions to be run at some sun events.

;;; Usage:
;; ;; the `solar' built-in package is used, these variables must be set:
;; (custom-set-variables
;;  '(calendar-latitude 42.7)
;;  '(calendar-longitude 23.3))

;; ;; create a two-argument function to be invoked at sun events, like
;; (defun switch-themes (sun-event &optional first-run)
;;   (cond ((eq sun-event 'sunrise)
;;	 ;; ...set lightish theme...
;;	 )
;;	((eq sun-event 'sunset)
;;	 ;; ...set darkish theme...
;;	 )
;;	((eq sun-event 'midday)
;;	 (when first-run
;;	     ;; ...set lightish theme...
;;	   )
;;	 ;; ...lunch...
;;	 )
;;	((eq sun-event 'midnight)
;;	 (when first-run
;;	     ;; ...set darkish theme...
;;	   )
;;	 ;; ...howl...
;;	 )))

;; ;; sign this function to be invoked on sun events
;; (add-hook 'rase-functions 'switch-themes)

;; ;; start the run-at-sun-event daemon, invoking hooks immediately
;; (rase-start t)

;;; Code:

(require 'solar)

;;;###autoload
(defcustom rase-functions nil
  "List of two-argument functions to run at sun event.
Possible values for the first argument are the symbols
`sunrise', `midday', `sunrise' and `midnight'.
The second argument is non-nil only the very start of rase daemon.
If it's a list, it holds the previous events for the day."
  :group 'rase :type 'hook)

(defvar *rase-timer* nil
  "Timer for the next sun event.")

(defun rase-calendar-current-date (&optional offset)
  "Return the current date in a list (month day year).
Optional integer OFFSET is a number of days from the current date.
This is just a backport of the GNU/Emacs 24 version."
  (let* ((now (decode-time))
	 (now (list (nth 4 now) (nth 3 now) (nth 5 now))))
    (if (zerop (or offset 0))
	now
      (calendar-gregorian-from-absolute
       (+ offset (calendar-absolute-from-gregorian now))))))

(defun rase-set-timer (event time &optional event-list offset)
  "Set timer for sun EVENT at TIME.
EVENT-LIST holds the next events for the current day + OFFSET."
  (let ((time-normalize (round (* 3600 time)))
	(date (rase-calendar-current-date offset)))
    (setq *rase-timer*
	  (run-at-time (encode-time (% time-normalize 60)
				    (/ (% time-normalize 3600) 60)
				    (/ time-normalize 3600)
				    (cadr date) (car date)
				    (nth 2 date))
		       nil 'rase-daemon event event-list))))

(defun rase-insert-event (event time event-list)
  "Insert EVENT with TIME in EVENT-LIST keeping it sorted on time."
  (catch 'inserted
    (let ((result-list nil))
      (while event-list
	(let ((event-time (cdar event-list)))
	  (if (< time event-time)
	      (throw 'inserted
		     (nconc result-list
			    (cons (cons event time) event-list)))
	    (setq result-list (nconc result-list
				     (list (car event-list)))
		  event-list (cdr event-list)))))
      (nconc result-list (list (cons event time))))))

(defun rase-build-event-list (&optional offset)
  "Build ordered list of sun events for current day + OFFSET."
  (let* ((solar-info (solar-sunrise-sunset
		      (rase-calendar-current-date offset)))
	 (sunrise (car solar-info))
	 (sunset (cadr solar-info)))
    (cond ((not (or sunrise sunset))
	   (if (char-equal (string-to-char (nth 2 solar-info)) ?2)
	       '((sunrise . 0) (midday . 12))  ; polar day
	     '((sunset . 0) (midnight . 12)))) ; polar night
	  ((not sunset)
	   (let ((midnight (/ sunrise 2.0)))
	     (list (cons 'midnight midnight) (cons 'sunrise sunrise)
		   (cons 'midday (+ midnight 12)))))
	  ((not sunrise)
	   (let ((midday (/ sunset 2.0)))
	     (list (cons 'midday midday) (cons 'sunset sunset)
		   (cons 'midnight (+ midday 12)))))
	  (t (let* ((sunrise (car sunrise))
		    (sunset (car sunset))
		    (early-sunset (< sunset sunrise))
		    (mid (+ (min sunrise sunset)
			    (/ (abs (- sunset sunrise)) 2.0)))
		    (anti-mid (+ mid (if (< mid 12) 12 -12))))
	       (rase-insert-event 'sunrise sunrise
				  (rase-insert-event
				   'sunset sunset
				   (rase-insert-event
				    (if early-sunset 'midnight
				      'midday)
				    mid
				    (list (cons (if early-sunset
						    'midday
						  'midnight)
						anti-mid))))))))))

(defun rase-daemon (event &optional event-list no-hooks)
  "Execute `rase-functions' for EVENT and set timer for the next.
EVENT-LIST holds the next events for the current day.
If NO-HOOKS is given, don't run hooks for current event."
  (or no-hooks (run-hook-with-args 'rase-functions event))
  (if event-list
      (rase-set-timer (caar event-list) (cdar event-list)
		      (cdr event-list))
    (setq event-list (rase-build-event-list 1))	; get next day events
    (rase-set-timer (caar event-list) (cdar event-list)
		    (cdr event-list) 1)))

;;;###autoload
(defun rase-start (&optional immediately)
  "Start run-at-sun-event daemon.  If IMMEDIATELY is non-nil, \
execute hooks for the previous event."
  (rase-stop)
  (let ((event-list (rase-build-event-list))
	(current-time (decode-time (current-time))))
    (let ((current-time (/ (+ (* 3600 (nth 2 current-time))
			      (* 60 (cadr current-time))
			      (car current-time))
			   3600.0))
	  (last-event nil)
	  (past-events nil))
      (while (and event-list (< (cdar event-list) current-time))
	(setq last-event (caar event-list)
	      event-list (cdr event-list))
	(push last-event past-events))
      (or last-event
	  (setq past-events (nreverse
			     (mapcar 'car (rase-build-event-list -1)))
		last-event (car past-events)))
      (if immediately
	  (run-hook-with-args 'rase-functions last-event
			      (or (cdr past-events) t)))
      (if event-list
	  (rase-set-timer (caar event-list) (cdar event-list)
			  (cdr event-list))
	(rase-daemon last-event nil t)))))

(defun rase-stop ()
  "Stop the run-at-sun-event daemon."
  (when *rase-timer*
    (if (timerp *rase-timer*)
	(cancel-timer *rase-timer*))
    (setq *rase-timer* nil)))

(provide 'rase)

;;; rase.el ends here
