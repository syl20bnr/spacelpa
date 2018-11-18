;;; helm-xref.el --- Helm interface for xref results -*- lexical-binding: t -*-

;; Copyright (C) 2017  Fritz Stelzer <brotzeitmacher@gmail.com>

;; Author: Fritz Stelzer <brotzeitmacher@gmail.com>
;; URL: https://github.com/brotzeitmacher/helm-xref
;; Package-Version: 20180528.1516
;; Version: 0.2
;; Package-Requires: ((emacs "25.1") (helm "1.9.4"))

;;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Code:

(require 'helm)
(require 'xref)
(require 'cl-seq)

(defvar helm-xref-alist nil
  "Holds helm candidates.")

(defgroup helm-xref nil
  "Xref with helm."
  :prefix "helm-xref-" :group 'helm)

(defface helm-xref-file-name
  '((t (:foreground "cyan")))
  "Face for xref file name"
  :group 'helm-xref)

(defface helm-xref-line-number
  '((t (:inherit 'compilation-line-number)))
  "Face for xref line number"
  :group 'helm-xref)

(defcustom  helm-xref-candidate-formatting-function 'helm-xref-format-candidate-short
  "Select the function for candidate formatting."
  :type '(radio (function-item helm-xref-format-candidate-short)
		(function-item helm-xref-format-candidate-long)
		function)
  :group 'helm-xref)

(defun helm-xref-candidates (xrefs)
  "Convert XREF-ALIST items to helm candidates and add them to `helm-xref-alist'."
  (dolist (xref xrefs)
    (with-slots (summary location) xref
      (let* ((line (xref-location-line location))
             (file (xref-location-group location))
             candidate)
        (setq candidate
              (funcall helm-xref-candidate-formatting-function file line summary))
        (push (cons candidate xref) helm-xref-alist))))
  (setq helm-xref-alist (reverse helm-xref-alist)))

(defun helm-xref-format-candidate-short (file line summary)
  "Build short form of candidate format with FILE, LINE, and SUMMARY."
  (concat
   (propertize (car (reverse (split-string file "\\/")))
	       'font-lock-face 'helm-xref-file-name)
   (when (string= "integer" (type-of line))
     (concat
      ":"
      (propertize (int-to-string line)
		  'font-lock-face 'helm-xref-line-number)))
   ":"
   summary))

(defun helm-xref-format-candidate-long (file line summary)
  "Build long form of candidate format with FILE, LINE, and SUMMARY."
  (concat
   (propertize file 'font-lock-face 'helm-xref-file-name)
   (when (string= "integer" (type-of line))
     (concat
      "\n:"
      (propertize (int-to-string line)
		  'font-lock-face 'helm-xref-line-number)))
   ":"
   summary))

(defun helm-xref-goto-xref-item (xref-item func)
  "Set buffer and point according to xref-item XREF-ITEM.

Use FUNC to display buffer."
  (with-slots (summary location) xref-item
    (let* ((marker (xref-location-marker location))
           (buf (marker-buffer marker))
           (offset (marker-position marker)))
      (with-current-buffer buf
        (goto-char offset)
        (funcall func buf)))))

(defun helm-xref-source ()
  "Return a `helm' source for xref results."
  (helm-build-sync-source "Helm Xref"
    :candidates (lambda ()
                  helm-xref-alist)
    :persistent-action (lambda (xref-item)
                         (helm-xref-goto-xref-item xref-item 'display-buffer))
    :action (lambda (xref-item)
              (helm-xref-goto-xref-item xref-item 'switch-to-buffer))
    :candidate-number-limit 9999))

(defun helm-xref-show-xrefs (xrefs _alist)
  "Function to display XREFS.

Needs to be set the value of `xref-show-xrefs-function'."
  (setq helm-xref-alist nil)
  (helm-xref-candidates xrefs)
  (helm :sources (helm-xref-source)
        :truncate-lines t
        :buffer "*helm-xref*"))

(provide 'helm-xref)
;;; helm-xref.el ends here
