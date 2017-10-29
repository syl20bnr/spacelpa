;;; phpcbf.el --- Format PHP code in Emacs using PHP_CodeSniffer's phpcbf

;;; Copyright (c) 2015 nishimaki10

;; Author: nishimaki10
;; URL: https://github.com/nishimaki10/emacs-phpcbf
;; Package-Version: 20150302.528
;; Version: 0.9.2
;; Package-Requires: ((s "1.9.0"))
;; Keywords: tools, php

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Use the command `phpcbf' to reformat the current buffer using
;; the "phpcbf" program.  To do this automatically on save, add
;; the following to your Emacs startup file:

;;     (add-hook 'php-mode-hook 'phpcbf-enable-on-save)

;; Please check the GitHub
;; (https://github.com/nishimaki10/emacs-phpcbf)
;; for more information.

;;; Code:

(require 's)

(defgroup phpcbf nil
  "Format PHP code using PHP_CodeSniffer's phpcbf"
  :group 'tools)

(defcustom phpcbf-executable (executable-find "phpcbf")
  "Location of the phpcbf executable."
  :group 'phpcbf
  :type 'string)

(defcustom phpcbf-standard "PEAR"
  "The name or path of the coding standard to use."
  :group 'phpcbf
  :type 'string)

;;;###autoload
(defun phpcbf ()
  "Format the current buffer according to the phpcbf."
  (interactive)
  (let ((temp-file (make-temp-file "phpcbf"))
        (now-point (point)))
    (unwind-protect
        (let ((status)
              (stderr)
              (standard (format "--standard=%s" phpcbf-standard))
              (encoding (s-chop-suffixes
                         '("-unix" "-dos" "-mac")
                         (format "--encoding=%s" buffer-file-coding-system)))
              (keep-stderr (list t temp-file)))

          (setq status
                (call-process-region
                 (point-min) (point-max) phpcbf-executable
                 t keep-stderr t
                 "-d" "error_reporting=0"
                 standard
                 encoding
                 ))

          (setq stderr
                (with-temp-buffer
                  (insert-file-contents temp-file)
                  (when (> (point-max) (point-min))
                    (insert ": "))
                  (buffer-substring-no-properties
                   (point-min) (line-end-position))))

          (cond
           ((stringp status)
            (error "`phpcbf` killed by signal %s%s" status stderr))
           ((not (equal 1 status))
            (error "`phpcbf` failed with code %d%s" status stderr))
           (t (message (format "Formatted to standard '%s'" phpcbf-standard))))
          ))
    (delete-file temp-file)
    (goto-char now-point)))

;;;###autoload
(defun phpcbf-enable-on-save ()
  "Run pbpcbf when this buffer is saved."
  (add-hook 'before-save-hook 'phpcbf nil t))

(provide 'phpcbf)

;;; phpcbf.el ends here
