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
;; Package-Version: 20190520.1929
;; URL: https://github.com/nickanderson/ob-cfengine3
;; Version: 0.0.6

;;; Commentary:
;; Execute CFEngine 3 policy inside org-mode src blocks.

;;; Code:

(defvar ob-cfengine3-command "cf-agent"
  "Name of command to use for executing cfengine policy.")

(defvar ob-cfengine3-command-options nil
  "Option string that should be passed to the agent.
Note that --file will be appended to the options.")

(defvar ob-cfengine3-file-control-stdlib "body file control{ inputs => { '$(sys.libdir)/stdlib.cf' };}\n"
  "File control body to include the standard libriary from $(sys.libdir).
It is useful to inject into an example source block before execution.")

(defvar ob-cfengine3-wrap-with-main-template "bundle agent __main__\n{\n%s\n}\n"
  "Template to use to wrap the contents of the source block in a
  'main' bundle. Must contain exactly one '%s', where the body
  will be inserted.")

(defconst ob-cfengine3-header-args-cfengine3
  '(
    (debug . :any)
    (info . :any)
    (verbose . :any)
    (use-locks . :any)
    (include-stdlib . :any)
    (define . :any)
    (bundlesequence . :any)
    (log-level . :any)
    (command . :any)
    (command-in-result . :any)
    (command-in-result-command . :any)
    (command-in-result-prompt . :any)
    (command-in-result-filename . :any)
    (run-with-main . :any))
  "CFEngine specific header arguments.")

(defun ob-cfengine3-header-arg (pname params)
  "Returns the value of header argument PNAME, extracted from PARAMS."
  (cdr (assoc pname params)))

(defun ob-cfengine3-bool (str)
  "Convert string to boolean. Strings `yes', `YES', `true',
`TRUE' and `t' are considered true, anything else is false."
  (or (string-equal str "yes")
      (string-equal str "true")
      (string-equal str "t")
      (string-equal str "YES")
      (string-equal str "TRUE")))

(defun ob-cfengine3-bool-arg (pname params)
  "Returns the boolean value of header argument PNAME, extracted from PARAMS."
  (ob-cfengine3-bool (ob-cfengine3-header-arg pname params)))

(defun ob-cfengine3-tangle-file (&optional tangle-value)
  "Return the filename to use for tangling a CFEngine code block.

TANGLE-VALUE must be the value of the `:tangle' header
argument. If `\"yes\"', returns the basename of the current
buffer with the `.cf' extension. If `\"no\"' or `nil', returns
`nil'. If any other string, it is returned as-is."
  (cond
   ((string= "yes" tangle-value)
    (concat (file-name-sans-extension (buffer-file-name)) "."
            (cdr (assoc "cfengine3" org-babel-tangle-lang-exts))))
   ((or (null tangle-value) (string= "no" tangle-value)) nil)
   ((> (length tangle-value) 0) tangle-value)))

(defun org-babel-execute:cfengine3 (body params)
  "Actuate a block of CFEngine 3 policy.
This function is called by `org-babel-execute-src-block'.

  A temporary file is constructed containing
  `ob-cfengine3-file-control-stdlib and the BODY of the src
  block. `ob-cfengine3-command' is used to execute the
  temporary file."
  (let* ((temporary-file-directory ".")
         (debug                      (ob-cfengine3-bool-arg :debug params))
         (info                       (ob-cfengine3-bool-arg :info params))
         (verbose                    (ob-cfengine3-bool-arg :verbose params))
         (use-locks                  (ob-cfengine3-bool-arg :use-locks params))
         (include-stdlib             (ob-cfengine3-bool (or (ob-cfengine3-header-arg :include-stdlib params) "yes")))
         (define                     (ob-cfengine3-header-arg :define params))
         (bundlesequence             (ob-cfengine3-header-arg :bundlesequence params))
         (log-level                  (ob-cfengine3-header-arg :log-level params))
         (command                    (or (ob-cfengine3-header-arg :command params) ob-cfengine3-command))
         (command-in-result          (ob-cfengine3-bool-arg :command-in-result params))
         (command-in-result-command  (or (ob-cfengine3-header-arg :command-in-result-command params) command))
         (command-in-result-prompt   (or (ob-cfengine3-header-arg :command-in-result-prompt params) "# "))
         (tempfile-dir               (or (ob-cfengine3-header-arg :tmpdir params) "."))
         (tempfile                   (make-temp-file (concat tempfile-dir "/cfengine3-")))
         (command-in-result-filename (or (ob-cfengine3-header-arg :command-in-result-filename params) (ob-cfengine3-tangle-file (ob-cfengine3-header-arg :tangle params)) tempfile))
         (auto-main                  (ob-cfengine3-bool-arg :auto-main params))
         (run-with-main              (or (ob-cfengine3-bool-arg :run-with-main params) auto-main)))
    (with-temp-file tempfile
      (when include-stdlib (insert ob-cfengine3-file-control-stdlib))
      (if run-with-main
          (insert (format ob-cfengine3-wrap-with-main-template body))
          (insert body)))
    (unwind-protect
        (let ((command-args
               (concat
                (when bundlesequence (concat "--bundlesequence "  bundlesequence " "))
                (when define (concat "--define "  define " "))
                (unless use-locks "--no-lock ")
                (when info "--inform ")
                (when verbose "--verbose ")
                ;; When debug header arg is given, add --debug with
                ;; all log modules enabled to the command string and
                ;; throw away the args
                (when debug (concat "--debug --log-modules=all "))
                (when log-level (concat "--log-level " log-level " "))
                (when ob-cfengine3-command-options (concat ob-cfengine3-command-options " ")))))
          (concat
           ;; When the :command-in-result header arg is specified,
           ;; include the command line in the output. The prompt,
           ;; command and filename to use (instead of the real ones)
           ;; can be specified with the :command-in-result-prompt,
           ;; :command-in-result-command and
           ;; :command-in-result-filename args.
           (when command-in-result
             (concat command-in-result-prompt
                     command-in-result-command " "
                     command-args
                     (format "--file %s" (shell-quote-argument command-in-result-filename))
                     "\n"))
           ;; Execute command and return output
           (shell-command-to-string
            (concat command " "
                    command-args
                    (format "--file %s" (shell-quote-argument tempfile))))))
      (delete-file tempfile))))

(add-to-list 'org-src-lang-modes '("cfengine3" . cfengine3))
(add-to-list 'org-babel-tangle-lang-exts '("cfengine3" . "cf"))

(setq org-babel-default-header-args:cfengine3
      '((:exports  . "both")
        ;; I don't know how to make it use the identity format directly
        ;;(:tangle-mode  . 384) ;; (identity #o600) The standard default
        (:tangle-mode  . 448)   ;; (identity #o700) Since we write the shebang, lets see how useful execution bit is
        (:shebang . "#!/var/cfengine/bin/cf-agent -f-")
        ;; By default, includ the standard library for exported files. To disable this set the :prologue header arg
        (:prologue . "body file control\n{\n      inputs => { '$(sys.libdir)/stdlib.cf' };\n}\n")
        (:results  . "output")))

(defun org-babel-expand-body:cfengine3 (body params)
  "Expand a block of CFEngine 3 policy before tangling.
This function is called by `org-babel-tangle-single-block'.

  If the `:tangle-with-main'' or `:auto-main' header arguments
  are `yes', `true' or `t', the BODY is formatted according to
  the template in `ob-cfengine3-wrap-with-main-template`,
  otherwise it is returned as-is."
  (let* ((auto-main (ob-cfengine3-bool-arg :auto-main params))
         (tangle-with-main (or (ob-cfengine3-bool-arg :tangle-with-main params) auto-main)))
    (if tangle-with-main
        (format ob-cfengine3-wrap-with-main-template body)
      body)))

(provide 'ob-cfengine3)
;;; ob-cfengine3.el ends here
