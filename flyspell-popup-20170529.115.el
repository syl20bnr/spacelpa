;;; flyspell-popup.el --- Correcting words with Flyspell in popup menus

;; Copyright (C) 2015, 2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
;; Keywords: convenience
;; Package-Version: 20170529.115
;; URL: https://github.com/xuchunyang/flyspell-popup
;; Version: 0.3
;; Package-Requires: ((popup "0.5.0"))
;; Created: Sun Jun 28 15:23:05 CST 2015

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
;; Correct the misspelled word with `flyspell' in popup menu.
;;
;; Usage:
;;
;; Call `flyspell-popup-correct' to correct misspelled word at point with a
;; Popup Menu. You might want to bind it to a key, for example:
;;
;;   (define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct)
;;
;; You can also enable `flyspell-popup-auto-correct-mode' to display that Popup
;; Menu automatically with a delay (default 1.6 seconds):
;;
;;   (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)

;;; Code:

(require 'flyspell)
(require 'popup)

;;;###autoload
(defun flyspell-popup-correct ()
  "Use popup for flyspell correction.
Adapted from `flyspell-correct-word-before-point'."
  (interactive)
  (with-local-quit                      ; http://lists.gnu.org/archive/html/emacs-devel/2013-06/msg00872.html
    ;; use the correct dictionary
    (flyspell-accept-buffer-local-defs)
    (let ((cursor-location (point))
          (word (flyspell-get-word))
          (opoint (point)))
      (if (consp word)
          (let ((word  (car word))
                (start (nth 1 word))
                (end   (nth 2 word))
                poss ispell-filter)
            ;; now check spelling of word.
            (ispell-send-string "%\n")	;put in verbose mode
            (ispell-send-string (concat "^" word "\n"))
            ;; wait until Aspell has processed word
            (while (progn
                     (accept-process-output ispell-process)
                     (not (string= "" (car ispell-filter)))))
            ;; Remove leading empty element
            (setq ispell-filter (cdr ispell-filter))
            ;; ispell process should return something after word is sent.
            ;; Tag word as valid (i.e., skip) otherwise
            (or ispell-filter
                (setq ispell-filter '(*)))
            (if (consp ispell-filter)
                (setq poss (ispell-parse-output (car ispell-filter))))
            (cond
             ((or (eq poss t) (stringp poss))
              ;; don't correct word
              t)
             ((null poss)
              ;; ispell error
              (error "Ispell: error in Ispell process"))
             (t
              ;; The word is incorrect, we have to propose a replacement.
              (let ((res
                     (popup-menu*
                      (append
                       (nth 2 poss)
                       (list
                        (popup-make-item (format "Save \"%s\"" word)
                                         :value (cons 'save word))
                        (popup-make-item (format "Accept (session) \"%s\"" word)
                                         :value (cons 'session word))
                        (popup-make-item (format "Accept (buffer) \"%s\"" word)
                                         :value (cons 'buffer word))))
                      :margin t
                      :fallback (lambda (_event _default)
                                  (keyboard-quit)))))
                (cond ((stringp res)
                       (flyspell-do-correct
                        (substring-no-properties res)
                        poss word cursor-location start end opoint))
                      (t
                       (let ((cmd (car res))
                             (wrd (cdr res)))
                         (flyspell-do-correct
                          cmd poss wrd cursor-location start end opoint)))))))
            (ispell-pdict-save t))))))


;;; Automatically Popup

(defcustom flyspell-popup-correct-delay 1.6
  "Delay in seconds before popup flyspell-popup correct menu.

Use floating point numbers to express fractions of seconds."
  :group 'flyspell
  :type 'number
  :safe #'numberp)

(defvar flyspell-popup-correct-timer nil
  "Timer to automatically call `flyspell-popup-correct'.")
(make-variable-buffer-local 'flyspell-popup-correct-timer)

(defun flyspell-popup-cancel-correct-timer ()
  (when flyspell-popup-correct-timer
    (cancel-timer flyspell-popup-correct-timer)
    (setq flyspell-popup-correct-timer nil)))

(defun flyspell-popup-popup-overlay-p ()
  (catch 'popup
    (dolist (ov (overlays-in (point-min) (point-max)) nil)
      (when (overlay-get ov 'popup)
        (throw 'popup t)))))

(defun flyspell-popup-flyspell-overlay-at-point-p ()
  (catch 'popup
    (dolist (ov (overlays-at (point)) nil)
      (when (flyspell-overlay-p ov)
        (throw 'popup t)))))

(defun flyspell-popup-correct-soon ()
  "Call `flyspell-popup-correct' delayed."
  ;; FIXME: It is probably redundant, but it looks quite complex to me due to:
  ;;
  ;; - Popup is long running
  ;; - `post-command-hook' and `flyspell-correct-word' will non-interactively move point
  ;; - Timer
  ;;
  ;; I want to give it up since I don't want to waste more time on it
  (flyspell-popup-cancel-correct-timer)
  (when (and flyspell-mode
             (not (flyspell-popup-popup-overlay-p))
             (flyspell-popup-flyspell-overlay-at-point-p))
    (setq flyspell-popup-correct-timer
          (run-at-time flyspell-popup-correct-delay nil
                       (lambda ()
                         (flyspell-popup-cancel-correct-timer)
                         (when (and flyspell-mode
                                    (not (flyspell-popup-popup-overlay-p))
                                    (flyspell-popup-flyspell-overlay-at-point-p))
                           (flyspell-popup-correct)))))))

;;;###autoload
(define-minor-mode flyspell-popup-auto-correct-mode
  "Minor mode for automatically correcting word at point."
  :group 'flyspell
  (if flyspell-popup-auto-correct-mode
      (progn
        (add-hook 'post-command-hook 'flyspell-popup-correct-soon nil 'local))
    (remove-hook 'post-command-hook 'flyspell-popup-correct-soon 'local)))

(provide 'flyspell-popup)
;;; flyspell-popup.el ends here
