;;; phpcbf.el --- Format PHP code in Emacs using PHP_CodeSniffer's phpcbf

;;; Copyright (c) 2015 nishimaki10

;; Author: nishimaki10
;; URL: https://github.com/nishimaki10/emacs-phpcbf
;; Package-Version: 20180519.838
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

(defcustom phpcbf-executable "phpcbf"
  "Location of the phpcbf executable."
  :group 'phpcbf
  :type 'string)

(defcustom phpcbf-standard nil
  "The name or path of the coding standard to use, e.g. “PEAR”.

If nil, “phpcbf” will look for a dominating “phpcs.xml” file,
falling back to PEAR if none is found."
  :group 'phpcbf
  :type '(choice string (const nil)))

(defun phpcbf-executable ()
  "Find the “phpcbf” executable or signal an error."
  (or (executable-find phpcbf-executable)
      (error "%s: executable not found" phpcbf-executable)))

(defun phpcbf--options ()
  "Generate the options list for running “phpcbf”."
  (append (when phpcbf-standard
            (list (format "--standard=%s" phpcbf-standard)))
          (list (s-chop-suffixes
                 '("-unix" "-dos" "-mac")
                 (format "--encoding=%s" buffer-file-coding-system))
		"-d" "error_reporting=0")))

;;;###autoload
(defun phpcbf ()
  "Format the current buffer according to the phpcbf."
  (interactive)
  (let (;; Make sure to look up the executable before switching to the
        ;; temp buffer so a buffer-local ‘exec-path’ can be used.
        (phpcbf (phpcbf-executable))
        (point (point))
        (source (current-buffer))
        (status) (output))
    (with-temp-buffer
      (insert-buffer-substring-no-properties source)
      (setq status (apply #'call-process-region
                          (point-min) (point-max)
                          phpcbf
                          t t nil
                          (phpcbf--options)))
      (setq output (buffer-substring-no-properties (point-min) (point-max))))
    (cond
     ((equal 1 status)
      (erase-buffer)
      (insert output)
      (goto-char point))
     ((stringp status)
      (error "‘phpcbf’ killed by signal %s" status))
     (t (unless output
          (setq output "no output"))
        ;; Strip trailing whitespace
        (when (string-match "[ \t\n\r]+\\'" output)
          (setq output (replace-match "" t t output)))
        (error "‘phpcbf’ failed with code %s: %s" status output)))))

(defun phpcbf-warn-on-error ()
  "Run phpcbf but reduce errors to warnings."
  (condition-case err
      (phpcbf)
    (error err (display-warning 'phpcbf (error-message-string err)))))

;;;###autoload
(defun phpcbf-enable-on-save ()
  "Run pbpcbf when this buffer is saved."
  (add-hook 'before-save-hook 'phpcbf-warn-on-error nil t))

(provide 'phpcbf)

;;; phpcbf.el ends here
