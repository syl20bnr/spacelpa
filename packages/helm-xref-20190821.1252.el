;;; helm-xref.el --- Helm interface for xref results -*- lexical-binding: t -*-

;; Copyright (C) 2017  Fritz Stelzer <brotzeitmacher@gmail.com>

;; Author: Fritz Stelzer <brotzeitmacher@gmail.com>
;; URL: https://github.com/brotzeit/helm-xref
;; Package-Version: 20190821.1252
;; Version: 0.4
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

;;; Contributors:
;; Sanjeev Sivasankaran <kalasalatemp at gmail.com> in 2019 changed font-face.

;;; Code:

(require 'helm)
(require 'helm-utils)
(require 'xref)
(require 'cl-seq)

(defvar helm-xref-alist nil
  "Holds helm candidates.")

(defgroup helm-xref nil
  "Xref with helm."
  :prefix "helm-xref-" :group 'helm)

(defface helm-xref-file-name
  '((t (:inherit 'font-lock-builtin-face)))
  "Face for xref file name"
  :group 'helm-xref)

(defface helm-xref-line-number
  '((t (:inherit 'helm-grep-lineno)))
  "Face for xref line number"
  :group 'helm-xref)

(defcustom helm-xref-candidate-formatting-function 'helm-xref-format-candidate-short
  "Select the function for candidate formatting."
  :type '(radio
          (function-item helm-xref-format-candidate-short)
          (function-item helm-xref-format-candidate-full-path)
		  (function-item helm-xref-format-candidate-long)
		  function)
  :group 'helm-xref)

(defun helm-xref-candidates-26 (xrefs)
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

(defun helm-xref-candidates-27 (fetcher alist)
  "Convert XREF-ALIST items to helm candidates and add them to `helm-xref-alist'."
  (cl-assert (functionp fetcher))
  (let* ((xrefs
          (or
           (assoc-default 'fetched-xrefs alist)
           (funcall fetcher))))
	(dolist (xref xrefs)
	  (with-slots (summary location) xref
	    (let* ((line (xref-location-line location))
		       (file (xref-location-group location))
		       candidate)
          (setq candidate
		        (funcall helm-xref-candidate-formatting-function file line summary))
          (push (cons candidate xref) helm-xref-alist)))))
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

(defun helm-xref-format-candidate-full-path (file line summary)
  "Same as `helm-xref-format-candidate-short', but display entire path."
  (concat
   (propertize file 'font-lock-face 'helm-xref-file-name)
   (when (string= "integer" (type-of line))
     (concat
      ":"
      (propertize (int-to-string line)
		  'font-lock-face 'helm-xref-line-number)))
   ":"
   summary))

(defun helm-xref-format-candidate-long (file line summary)
  "Use two lines for each candidate. One contains the path and the other the actual candidate."
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
      (switch-to-buffer buf)
      (goto-char offset)
      (funcall func buf))))

(defun helm-xref-source ()
  "Return a `helm' source for xref results."
  (helm-build-sync-source "Helm Xref"
    :candidates (lambda ()
                  helm-xref-alist)
    :persistent-action (lambda (xref-item)
                         (helm-xref-goto-xref-item
                          xref-item '(lambda (buf) (helm-highlight-current-line))))
    :action '(("Switch to buffer" . (lambda (xref-item) (helm-xref-goto-xref-item xref-item 'switch-to-buffer)))
              ("Other window" . (lambda (xref-item) (helm-xref-goto-xref-item xref-item 'switch-to-buffer-other-window))))
    :candidate-number-limit 9999))

(defun helm-xref-show-xrefs (xrefs _alist)
  "Function to display XREFS.

Needs to be set the value of `xref-show-xrefs-function'."
  (setq helm-xref-alist nil)
  (helm-xref-candidates-26 xrefs)
  (helm :sources (helm-xref-source)
        :truncate-lines t
        :buffer "*helm-xref*"))

(defun helm-xref-show-xrefs-27 (fetcher alist)
  "Function to display XREFS.

Needs to be set the value of `xref-show-xrefs-function'."
  (setq helm-xref-alist nil)
  (helm-xref-candidates-27 fetcher alist)
  (helm :sources (helm-xref-source)
        :truncate-lines t
        :buffer "*helm-xref*"))

(if (< emacs-major-version 27)
    (setq xref-show-xrefs-function 'helm-xref-show-xrefs)
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs-27))

(provide 'helm-xref)
;;; helm-xref.el ends here
