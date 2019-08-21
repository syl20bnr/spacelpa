;;; ietf-docs.el --- Fetch, Cache and Load IETF documents

;; Copyright (c) 2015 by Christian E. Hopps
;; All rights reserved.

;; Author: Christian E. Hopps <chopps@gmail.com>
;; Package-Version: 20190420.851
;; Package-X-Original-Version: 1.0.0
;; Keywords: ietf, rfc
;; URL: https://github.com/choppsv1/ietf-docs

;; This file is NOT part of GNU Emacs.

;;; License:

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:
;;
;; Functions supporting fetching, caching and opening the IETF
;; document referenced by the text at the point.
;;
;;; Code:

;; Suggested binding: C-c i o
;; (global-set-key (kbd "C-c i o") 'ietf-docs-open-at-point)

(require 'thingatpt)

(defgroup  ietf-docs nil
  "Customizable variables for ietf-docs functions"
  :group 'communications)

(defcustom ietf-docs-cache-directory (expand-file-name "~/ietf-docs-cache/")
  "Local directory to store downloaded IETF documents. Created if necessary."
  :type 'directory
  :group 'ietf-docs)

(defcustom ietf-docs-draft-url-directory "http://tools.ietf.org/id/"
  "The base URL to fetch IETF drafts from."
  :type 'string
  :group 'ietf-docs)

(defcustom ietf-docs-rfc-url-directory "http://tools.ietf.org/rfc/"
  "The base URL to fetch IETF RFCs from."
  :type 'string
  :group 'ietf-docs)

;--------------------------------------------------
; Define a thing-at-point for draft and RFC names.
;--------------------------------------------------

(defvar ietf-draft-or-rfc-regexp "\\(\\(RFC\\|rfc\\)\\(-\\| \\)?[0-9]+\\)\\|\\(draft-[-[:alnum:]]+\\(.txt\\|.html\\|.xml\\)?\\)")

(put 'ietf-docs-name 'bounds-of-thing-at-point 'thing-at-point-bounds-of-ietf-name-at-point)
(defun thing-at-point-bounds-of-ietf-name-at-point ()
  (if (thing-at-point-looking-at ietf-draft-or-rfc-regexp)
      (let ((beginning (match-beginning 0))
            (end (match-end 0)))
        (cons beginning end))))

(put 'ietf-docs-name 'thing-at-point 'thing-at-point-ietf-name-at-point)
(defun thing-at-point-ietf-name-at-point ()
  "Return the ietf document name around or before point."
  (let ((name ""))
    (if (thing-at-point-looking-at ietf-draft-or-rfc-regexp)
        (progn
          (setq name (buffer-substring-no-properties (match-beginning 0)
                                                     (match-end 0)))
          (if (string-match "\\(?:RFC\\|rfc\\)\\(?:[ ]+\\|-\\)?\\([0-9]+\\)" name)
              (setq name (concat "rfc" (match-string 1 name))))
          (if (string-match "draft-\\([-[:alnum:]]+\\)\\(?:.txt\\|.html\\|.xml\\)" name)
              (setq name (concat "draft-" (match-string 1 name))))
          name))))

(put 'ietf-docs-name 'end-op
     (function (lambda ()
		 (let ((bounds (thing-at-point-bounds-of-ietf-name-at-point)))
		   (if bounds
		       (goto-char (cdr bounds))
		     (error "No IETF document name here"))))))

(put 'ietf-docs-name 'beginning-op
     (function (lambda ()
		 (let ((bounds (thing-at-point-bounds-of-ietf-name-at-point)))
		   (if bounds
		       (goto-char (car bounds))
		     (error "No IETF document name here"))))))

(defun ietf-docs-starts-with (string prefix)
  "Return t if STRING starts with PREFIX."
  (let* ((l (length prefix)))
    (string= (substring string 0 l) prefix)))

(defun ietf-docs-at-point ()
  "Return the ietf document name at the point with .txt extension."
  (let ((docname (thing-at-point 'ietf-docs-name)))
    (if docname
        (concat (file-name-sans-extension docname) ".txt"))))

(defun ietf-docs-fetch-to-cache (filename &optional reload)
  (let* ((pathname (concat (file-name-as-directory ietf-docs-cache-directory) (downcase filename)))
         url)
    (if (and (file-exists-p pathname) (not reload))
        (message "Cached path %s" pathname)
      (setq filename (downcase filename))
      (make-directory ietf-docs-cache-directory t)
      (if (ietf-docs-starts-with filename "rfc")
          (setq url (concat ietf-docs-rfc-url-directory filename))
        (setq url (concat ietf-docs-draft-url-directory filename)))
      (message url)
      (url-copy-file url pathname t)
      (message "Downloading %s to %s" url pathname)
      pathname)
    pathname))

;;;###autoload
(defun ietf-docs-at-point-fetch-to-cache (&optional reload)
  "Load the document around the point into the cache if not
  present. If prefix argument is given force cache RELOAD."
  (interactive "P")
  (let ((docname (ietf-docs-at-point)))
    (if docname
        (ietf-docs-fetch-to-cache docname reload))))

;;;###autoload
(defun ietf-docs-open-at-point (&optional reload)
  "Open the IETF internet-draft or RFC indicated by the point in a new
  buffer. RELOAD the cache if PREFIX argument is specified"
  (interactive "P")
  (let ((pathname (ietf-docs-at-point-fetch-to-cache reload)))
    (if pathname
        (find-file-read-only pathname)
      (error "No IETF document name around point"))))

(provide 'ietf-docs)

;;; ietf-docs.el ends here
