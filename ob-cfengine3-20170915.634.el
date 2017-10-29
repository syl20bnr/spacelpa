;;; ob-cfengine3.el --- Org Babel functions for CFEngine 3

;; Copyright (C) 2017  Nick Anderson

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;; Author: Nick Anderson <nick@cmdln.org>
;; Keywords: tools, convenience
;; Package-Version: 20170915.634
;; URL: https://github.com/nickanderson/ob-cfengine3
;; Version: 0.0.1


;;; Commentary:
;; Execute CFEngine 3 policy inside org-mode src blocks.

;;; Code:

(defvar ob-cfengine3-command "cf-agent"
  "Name of command to use for executing cfengine policy.")

(defvar ob-cfengine3-command-options ""
  "Option string that should be passed to the agent.
Note that --file will be appended to the options.")

(defvar ob-cfengine3-file-control-stdlib "body file control{ inputs => { '$(sys.libdir)/stdlib.cf' };}\n"
  "File control body to include the standard libriary from $(sys.libdir).
It is useful to inject into an example source block before execution.")

(defconst ob-cfengine3-header-args-cfengine3
  '(
    (no-lock . :any)
    (include-stdlib . :any)
    (define . :any)
    (bundlesequence . :any))
  "CFEngine specific header arguments.")

(defun org-babel-execute:cfengine3 (body params)
  "Actuate a block of CFEngine 3 policy.
This function is called by `org-babel-execute-src-block'.

  A temporary file is constructed containing
  `ob-cfengine3-file-control-stdlib and the BODY of the src
  block. `ob-cfengine3-command' is used to execute the
  temporary file."

    (let* ((temporary-file-directory ".")
           (use-locks (cdr (assoc :use-locks params)))
           (include-stdlib (not (string= "no" (cdr (assoc :include-stdlib params)))))
           (define (cdr (assoc :define params)))
           (bundlesequence (cdr (assoc :bundlesequence params)))
    (tempfile (make-temp-file "cfengine3-")))
      (with-temp-file tempfile
        (when include-stdlib (insert ob-cfengine3-file-control-stdlib))
        (insert body))
      (unwind-protect
         (shell-command-to-string
           (concat
            ob-cfengine3-command
            " "
            (when bundlesequence (concat "--bundlesequence "  bundlesequence ))
            " "
            (when define (concat "--define "  define ))
            " "
            (unless use-locks "--no-lock")
            ob-cfengine3-command-options
            " "
            (format " --file %s" (shell-quote-argument tempfile))))
        (delete-file tempfile))))

(add-to-list 'org-src-lang-modes '("cfengine3" . cfengine3))
(add-to-list 'org-babel-tangle-lang-exts '("cfengine3" . "cf"))

(provide 'ob-cfengine3)
;;; ob-cfengine3.el ends here
