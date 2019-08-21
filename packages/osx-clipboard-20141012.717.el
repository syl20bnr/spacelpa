;;; osx-clipboard.el --- Use the OS X clipboard from terminal Emacs

;; Copyright (C) 2014 Jon Oddie <jonxfield@gmail.com>

;; Author:			Jon Oddie <jonxfield at gmail.com>
;; Created:			11 October 2014
;; Version:                     0.1
;; Package-Version: 20141012.717
;; Url:                         https://github.com/joddie/osx-clipboard-mode

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;;; Commentary:

;; Sometimes it is useful to run Emacs in a plain terminal window, even
;; when a graphical display is available, but it's a nuisance if you need
;; to copy and paste from the text-mode Emacs to another program.  This is
;; a tiny minor mode which lets Emacs on Mac OS X use the system clipboard
;; even when running in a text terminal, via the external `pbpaste' and
;; `pbcopy' programs.

;; To enable it, either customize the variable `osx-clipboard-mode' to `t',
;; or add the following line to your init file:

;; ,----
;; | (osx-clipboard-mode +1)
;; `----

;; Attempting to enable this mode an a non-OS-X system or in a graphical
;; Emacs will do nothing, so it should be safe to enable it unconditionally
;; even if you share your configuration between multiple machines.

;;;; Code:

;;;###autoload
(defgroup osx-clipboard nil
  "Enable the OS X clipboard when running in a text terminal."
  :group 'environment
  :tag "OS X Clipboard"
  :link '(url-link "http://github.com/joddie/osx-clipboard-mode"))

;;;###autoload
(define-minor-mode osx-clipboard-mode
    "Kill and yank using the OS X clipboard when running in a text terminal.

This mode allows Emacs to use the OS X system clipboard when
running in the terminal, making killing and yanking behave
similarly to a graphical Emacs.  It is not needed in a graphical
Emacs, where NS clipboard integration is built in.

It sets the variables `interprogram-cut-function' and
`interprogram-paste-function' to thin wrappers around the
\"pbcopy\" and \"pbpaste\" command-line programs.

Consider also customizing the variable
  `save-interprogram-paste-before-kill' to `t' for best results."
  :global t
  :lighter " OSX-Clipboard" :tag "OS X Clipboard Mode"
  :group 'osx-clipboard
  (if (not (and (eq system-type 'darwin) (not window-system)))
      (progn
        (when (called-interactively-p 'any)
          (message "`osx-clipboard-mode' only works in text terminals under OS X"))
        (setq osx-clipboard-mode nil))
    (if osx-clipboard-mode
          ;; Turn on
          (setq interprogram-cut-function #'osx-clipboard-cut-function
                interprogram-paste-function #'osx-clipboard-paste-function)
        ;; Turn off
        (setq interprogram-cut-function nil
              interprogram-paste-function nil))))

(defun osx-clipboard-cut-function (text &rest ignore)
  "Copy TEXT to the OS X clipboard using \"pbpaste\".

This is set as the value of `interprogram-cut-function' by
`osx-clipboard-mode'.  It should only be used when Emacs is running in a
text terminal."
  (with-temp-buffer
    (insert text)
    (with-demoted-errors "Error calling pbcopy: %S"
      (call-process-region (point-min) (point-max) "pbcopy"))))

(defvar osx-clipboard-last-selected-text nil)

(defun osx-clipboard-paste-function ()
  "Return the value of the OS X clipboard using \"pbcopy\".

This is set as the value of `interprogram-paste-function' by
`osx-clipboard-mode'.  It should only be used when Emacs is running in a
text terminal."
  (with-temp-buffer
    (with-demoted-errors "Error calling pbpaste: %S"
      (call-process "pbpaste" nil t)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        ;; The following logic is adapted from `x-selection-value'
        ;; in `ns-win.el.gz'
        (cond
          ((or
            ;; Avoid copying an empty clipboard, or copying the same
            ;; text twice
            (not text)
            (eq text osx-clipboard-last-selected-text)
            (string= text "")
            (string= text (car kill-ring))) nil)
          ((string= text osx-clipboard-last-selected-text)
           ;; Record the newer string, so subsequent calls can use the `eq' test.
           (setq osx-clipboard-last-selected-text text)
           nil)
          (t
           (setq osx-clipboard-last-selected-text text)))))))

(provide 'osx-clipboard)

;;; osx-clipboard.el ends here
