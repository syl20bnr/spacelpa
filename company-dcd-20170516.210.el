;;; company-dcd.el --- Company backend for Dlang using DCD.

;; Author: tsukimizake <shomasd_at_gmail.com>
;; Version: 0.1
;; Package-Version: 20170516.210
;; Package-Requires: ((company "0.9") (flycheck-dmd-dub "0.7") (yasnippet "0.8") (popwin "0.7") (cl-lib "0.5") (ivy "20160804.326"))
;; Keywords: languages
;; URL: http://github.com/tsukimizake/company-dcd

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

;; Company backend for DCD.

;;; Usage:

;; (add-to-list 'exec-path "/path/to/DCD/bin")
;; (require 'company-dcd)
;; (add-hook 'd-mode-hook 'company-dcd-mode)


;;; Code:

(require 'company)
(require 'rx)
(require 'yasnippet)
(require 'flycheck-dmd-dub)
(require 'ring)
(require 'cl-lib)
(require 'popwin)
(require 'ivy)
(defgroup company-dcd nil "company-mode backend for DCD." :group 'company)

(defcustom company-dcd-client-executable
  "dcd-client"
  "Location of dcd-client executable."
  :group 'company-dcd
  :type 'file)

(defcustom company-dcd-compiler
  "dmd"
  "Name of dlang compiler executable."
  :group 'company-dcd
  :type 'file)

(defcustom company-dcd--flags nil
  "Extra flags to pass to the dcd-server.
This variable will typically contain include paths,
e.g., (\"-I~/MyProject\", \"-I.\").
You can't put port number flag here.  Set `company-dcd--server-port' instead."
  :group 'company-dcd
  :type '(repeat (string :tag "Argument" "")))

(defconst company-dcd--completion-pattern
  (rx bol (submatch (1+ nonl)) "\t" (submatch (any "cisuvmkfgePMaAltT")) eol)
  "Regex to parse dcd output.
\\1 is candidate itself, \\2 is kind of candidate.")

(defconst company-dcd--server-buffer-name "*dcd-server*")
(defconst company-dcd--error-buffer-name "*dcd-error*")
(defconst company-dcd--output-buffer-name "*dcd-output*")
(defconst company-dcd--documentation-buffer-name "*dcd-document*")

(defcustom company-dcd-server-executable
  "dcd-server"
  "Location of dcd-server executable."
  :group 'company-dcd
  :type 'file)

(defcustom company-dcd--server-port 9166
  "Port number of dcd-server.  The default is 9166."
  :group 'company-dcd)

(defvar company-dcd--delay-after-kill-process 200
  "Duration to wait after killing the server process, in milliseconds.
If `company-dcd-restart-server' does not work correctly, please set this variable to a bigger number.")

(defvar company-dcd--version nil
  "Version of dcd-server.  This variable is automatically set when company-dcd--get-version is called.")

(defcustom company-dcd--ignore-template-argument nil
  "If non-nil, ignore template argument of calltip candidate."
  :group 'company-dcd)

;; Server management functions

(defun company-dcd-stop-server ()
  "Stop dcd-server manually.  You shouldn't need to call this function directly.

If you need to restart the server, use `company-dcd-restart-server' instead."
  (interactive)
  (interrupt-process "dcd-server"))

(defun company-dcd--start-server ()
  "Start dcd-server."

  (unless (executable-find company-dcd-server-executable)
    (error "company-dcd error: dcd-server not found"))
  
  (let (buf args proc)
    (setq buf (get-buffer-create company-dcd--server-buffer-name))
    (setq args (nconc (list company-dcd-server-executable
			    "-p"
			    (format "%s" company-dcd--server-port))
		      company-dcd--flags))
    (setq proc
	  (with-current-buffer buf (apply 'start-process "dcd-server" (current-buffer) args)))
    (set-process-query-on-exit-flag proc nil)))

(defun company-dcd--server-is-alive-p ()
  "If dcd-server is alive, return t.  Otherwise, return nil."
  (if (or (get-process "dcd-server") (not (zerop (string-to-number (shell-command-to-string "pidof dcd-server")))))
      t
    nil))

(defun company-dcd-maybe-start-server ()
  "Start dcd-server.  If the server process is already running, do nothing."
  (unless (company-dcd--server-is-alive-p)
    (company-dcd--start-server)))

(defun company-dcd-restart-server ()
  "Start dcd-server.  If the server process is already running, restart it."
  (interactive)
  (when (company-dcd--server-is-alive-p)
    (company-dcd-stop-server)
    (sleep-for 0 company-dcd--delay-after-kill-process))
  (company-dcd--start-server)
  (setq company-dcd--version nil))

(defun company-dcd--get-version ()
  "Get the version of dcd-server.  Cache the value to `company-dcd--version'."
  (if company-dcd--version
      company-dcd--version
    (progn
      (let ((str (company-dcd--call-process '("--version")))
	    verstr)
	(unless str
	  (error "company-dcd error: Error obtaining dcd-server version"))
	(string-match (rx "v" (submatch (* nonl)) (or "-" "\n")) str)
	(setq verstr (match-string 1 str))
	(setq company-dcd--version (string-to-number verstr))
	))))

;; Output parsing functions

(defun company-dcd--parse-output-for-completion ()
  "Parse dcd output from a completion query.

Return a list of matches, where each match is a string,
optionally with an attached `company-dcd--help' property
containing the completion kind."
  (with-current-buffer company-dcd--output-buffer-name
    (goto-char (point-min))
    (let ((pattern company-dcd--completion-pattern)
	  lines match detailed-info
	  )
      (while (re-search-forward pattern nil t)
	(setq match (match-string-no-properties 1))
	
	(setq detailed-info (match-string-no-properties 2))
	(when detailed-info
	  (setq match (propertize match 'company-dcd--help detailed-info)))
	(push match lines))
      lines)))

(defun company-dcd--get-help (cand)
  "Get symbol kind of completion candidate CAND."
  (get-text-property 0 'company-dcd--help cand))

(defvar company-dcd--error-message-regexp
  (rx (and (submatch (* nonl))  ": " (submatch (* nonl)) ": " (submatch (* nonl) eol)))
  "If this regexp matches the first line of dcd-client output, it indicates an error message.")

(defun company-dcd--handle-error (res args)
  "Display error message from a failed dcd-client invocation with exit code RES and arguments ARGS."
  (let* ((errbuf (get-buffer-create company-dcd--error-buffer-name))
         (outbuf (get-buffer company-dcd--output-buffer-name))
         (cmd (concat company-dcd-client-executable " " (mapconcat 'identity args " ")))
         (errstr
          (with-current-buffer outbuf
            (goto-char (point-min))
            (re-search-forward company-dcd--error-message-regexp)
            (concat
             (match-string 2) " : " (match-string 3)))
          ))
    (with-current-buffer errbuf
      (erase-buffer)
      (insert (current-time-string)
              "\n\"" cmd "\" failed."
              (format "\nError type is: %s\n" errstr)
              )
      (goto-char (point-min)))
    (display-buffer errbuf)))

(defun company-dcd--output-buf-string ()
  "Return contents of dcd-output buffer."
  (with-current-buffer company-dcd--output-buffer-name
    (buffer-string)))

;; Utility functions for process invocation

(defun company-dcd--call-process (args)
  "Call dcd-client with ARGS and return output string.

The current buffer's contents is passed to dcd-client via stdin.
\(The entire buffer is sent, even if narrowed.\)

Returns the output from dcd-client, or nil if an error occurred."
  (let ((buf (get-buffer-create company-dcd--output-buffer-name)))
    (with-current-buffer buf (erase-buffer))
    (if (executable-find company-dcd-client-executable)
	;; Execute dcd-client, get error code
	(let ((res (apply 'call-process-region 1 (1+ (buffer-size))
			  company-dcd-client-executable nil buf nil args)))
	  (with-current-buffer buf
	    (if (eq 0 res)
		(buffer-string)		; Return output
	      (company-dcd--handle-error res args)
	      nil)))

      (message "company-dcd error: could not find dcd-client executable")
      nil)))

(defsubst company-dcd--cursor-position ()
  "Get the current cursor position to pass to dcd-client."
  (1- (position-bytes (point))))

(defun company-dcd--build-args (&optional pos)
  "Build the argument list to pass to dcd-client.

Optionally, pass POS as the --cursorPos argument if non-nil."
  (nconc
   (list
    "--port"
    (format "%s" company-dcd--server-port))
   (when pos
     (list
      (concat "-I" default-directory)
      "--cursorPos"
      (format "%s" pos)))))

(defsubst company-dcd--in-string/comment ()
  "Return non-nil if point is in a literal (a comment or string)."
  (nth 8 (syntax-ppss)))

;; Interface functions to company-mode.

(defun company-dcd--get-candidates ()
  "Retrieve ordinary auto-completion candidates."
  (unless (company-dcd--in-string/comment)
    (when (company-dcd--call-process
	   (company-dcd--build-args (company-dcd--cursor-position)))
      (company-dcd--parse-output-for-completion))))

(defun company-dcd--documentation (item)
  "Return a short documentation string of ITEM.

This will be displayed in the mini-buffer when a completion candidate is
highlighted.  Currently this returns a string describing the item's kind."
  (when (stringp item)
    (let ((s (company-dcd--get-help item)))
      (cond
       ;; https://github.com/Hackerpilot/DCD#completion-kinds
       ((equal s "c") "class name")
       ((equal s "i") "interface name")
       ((equal s "s") "struct name")
       ((equal s "u") "union name")
       ((equal s "v") "variable name")
       ((equal s "m") "member variable name")
       ((equal s "k") "keyword / built-in version / scope statement")
       ((equal s "f") "function or method")
       ((equal s "g") "enum name")
       ((equal s "e") "enum member")
       ((equal s "P") "package name")
       ((equal s "M") "module name")
       ((equal s "a") "array")
       ((equal s "A") "associative array")
       ((equal s "l") "alias name")
       ((equal s "t") "template name")
       ((equal s "T") "mixin template name")
       (t (format "Unknown candidate kind: %s" s))
       ))))

(defun company-dcd--action (lastcompl)
  "Post-completion action callback.

Used to display the argument list (calltips)."
  ;; Q: Why run-with-timer?
  ;; A: See https://github.com/company-mode/company-mode/issues/320
  (let ((candidate-type (company-dcd--get-help lastcompl)))
    (cond
     ((string= "f" candidate-type) ; when it was a function
      (run-with-idle-timer 0 nil (lambda ()
				   (company-begin-backend 'company-dcd--calltips)
				   (let ((this-command 'company-idle-begin))
				     (company-post-command)))))
     ((string= "s" candidate-type) ; when it was a struct
      (run-with-idle-timer 0 nil (lambda ()
				   (company-begin-backend 'company-dcd--calltips-for-struct-constructor)
				   (let ((this-command 'company-idle-begin))
				     (company-post-command)))))
     ((string= "c" candidate-type) ; when it was a class
      (run-with-idle-timer 0 nil (lambda ()
				   (company-begin-backend 'company-dcd--calltips-for-struct-constructor)
				   (let ((this-command 'company-idle-begin))
				     (company-post-command)))))
     )))

(defvar company-dcd-mode nil)

(defun company-dcd (command &optional arg &rest ignored)
  "The `company-mode' backend callback for DCD."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-dcd))
    (prefix (and company-dcd-mode (company-grab-symbol)))
    (candidates (company-dcd--get-candidates))
    (annotation (format " %s" (company-dcd--get-help arg)))
    (meta (company-dcd--documentation arg))
    (post-completion (company-dcd--action arg))
    (doc-buffer (company-dcd--get-completion-documentation arg))
    (location (company-dcd--get-completion-location arg))))


;; Function calltip expansion with yasnippet
(defun company-dcd--get-calltip-candidates ()
  "Return calltip completion candidates for the D symbol at point.

The cursor must be at the end of a D symbol.
When the symbol is not a function, return nil."
  (let ((buf (get-buffer-create company-dcd--output-buffer-name)))
    (when (company-dcd--call-process-for-calltips)
      (with-current-buffer buf (company-dcd--parse-calltips)))))

(defun company-dcd--call-process-for-calltips ()
  "Call process to get calltips of the function at point."
  (let ((src (buffer-string))
	(pt (point)))
    (with-temp-buffer
      (insert src)
      (goto-char pt)

      (insert "( ;")
      (backward-char 2)

      (company-dcd--call-process
       (company-dcd--build-args (company-dcd--cursor-position))))))


(defconst company-dcd--normal-calltip-pattern
  (rx bol (submatch (* nonl)) (submatch "(" (* nonl) ")") eol)
  "Regexp to parse calltip completion.
\\1 is function return type (if exists) and name, and \\2 is args.")
(defconst company-dcd--template-pattern (rx (submatch (* nonl)) (submatch "(" (*? nonl) ")") (submatch "(" (* nonl)")"))
  "Regexp to parse template calltips.  
\\1 is function return type (if exists) and name, \\2 is template args, and \\3 is args.")
(defconst company-dcd--calltip-pattern
  (rx  (or (and bol (* nonl) "(" (* nonl) ")" eol)
	   (and bol (* nonl) "(" (*? nonl) ")" "(" (* nonl)")" eol))))
(defcustom company-dcd--ignore-template-argument t
  "If non-nil, ignore template argument on calltip expansion."
  :group 'company-dcd)

(defsubst company-dcd--cleanup-function-candidate (s)
  "Helper function for parsing calltips.

Remove the return type from the beginning of the function signature.
S is the candidate string."
  (let (res)
    (with-temp-buffer
      (insert s)

      ;;goto beggining of function name
      (progn
        (end-of-line)
        (backward-sexp)
        (re-search-backward (rx (or bol " "))))

      (setq res (buffer-substring
                 (point)
                 (progn
                   (end-of-line)
                   (point))))
      (when (equal " " (substring res 0 1))
        (setq res (substring res 1)))
      res
      )))

(defsubst company-dcd--cleanup-template-candidate (s)
  "Helper function for parsing calltips.

Remove return type of the head of the template function.
S is the candidate string."
  (let (res)
    (with-temp-buffer
      (insert s)

      ;; Go to beginning of function name
      (progn
        (end-of-line)
        (backward-sexp)
	(backward-sexp)
        (re-search-backward (rx (or bol " "))))

      (setq res (buffer-substring
                 (point)
                 (progn
                   (end-of-line)
                   (point))))
      (when (equal " " (substring res 0 1))
        (setq res (substring res 1)))
      res
      )))

(defsubst company-dcd--candidate-is-template-p (s)
  "Helper function for parsing calltips.

If candidate string S is a template, return t."
  (with-temp-buffer
    (insert s)
    (backward-sexp)
    (equal ")" (char-to-string (char-before)))))

(defun company-dcd--parse-calltips ()
  "Parse dcd output for calltip completion.

Returns a list of calltip candidates."
  (goto-char (point-min))
  (let ((pattern company-dcd--calltip-pattern)
        lines
        match
	)
    (while (re-search-forward pattern nil t)
      (setq match (match-string 0))
      (if (company-dcd--candidate-is-template-p match)
	  (progn
	    (string-match company-dcd--template-pattern match)
	    (add-to-list 'lines (company-dcd--cleanup-function-candidate (format "%s%s" (match-string 1 match) (match-string 3 match)))) ;remove template argument
	    (unless company-dcd--ignore-template-argument
	      (string-match company-dcd--template-pattern match)
	      (add-to-list 'lines (company-dcd--cleanup-template-candidate (format "%s!%s%s" (match-string 1 match) (match-string 2 match) (match-string 3 match))))) ; candidate with template argument
	    )
	(progn
	  (string-match company-dcd--normal-calltip-pattern match)
	  (add-to-list 'lines (company-dcd--cleanup-function-candidate (format "%s%s" (match-string 1 match) (match-string 2 match))))) ; when it was not template argument
	))
    lines
    ))

(defsubst company-dcd--format-calltips (str)
  "Format calltips STR in parenthesis to yasnippet style."
  (let (yasstr)
    
    ;;remove parenthesis
    (setq str (substring str 1 (1- (length str))))

    (setq yasstr
	  (mapconcat
	   (lambda (s) "format each args to yasnippet style" (concat "${" s "}"))
	   (split-string str ", ")
	   ", "))
    (setq yasstr (concat "(" yasstr ")"))
    ))

(defun company-dcd--calltip-action (lastcompl)
  "Post-completion callback: format and insert the calltip using yasnippet.
This function should be called at *dcd-output* buf."
  (let* ((end (point))
	 (arg-beg (save-excursion
		    (backward-sexp)
		    (point)))
	 (template-beg
	  (if (company-dcd--candidate-is-template-p lastcompl)
	      (save-excursion
		(backward-sexp 2)
		(point))
	    nil))
	 (args (buffer-substring arg-beg end))
	 res)
    (delete-region arg-beg end)
    (setq res (company-dcd--format-calltips args))
    
    (when template-beg
      (let ((template-args (buffer-substring template-beg arg-beg)))
	(delete-region template-beg arg-beg)
	(setq res (format "%s%s" (company-dcd--format-calltips template-args) res))))
    (yas-expand-snippet res)))

(defun company-dcd--calltip-completion-available ()
  (if (company-dcd--get-calltip-candidates)
      (company-grab-symbol)
    nil))

(defun company-dcd--calltips (command &optional arg &rest ignored)
  "Company \"backend\" for DCD calltip completion."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-dcd--calltips))
    (prefix (company-dcd--calltip-completion-available))
    (candidates
     (company-dcd--get-calltip-candidates)
     )
    (post-completion (company-dcd--calltip-action arg))
    (doc-buffer (company-dcd--get-completion-documentation arg))
    (location (company-dcd--get-completion-location arg))))

;; Struct constructor calltip expansion

(defsubst company-dcd--replace-this-to-struct-name (struct-name)
  "Replace \"this\" with STRUCT-NAME.
dcd-client outputs candidates which begin with \"this\" when completing struct constructor calltips."
  (goto-char (point-min))
  (while (search-forward "this" nil t)
    (replace-match struct-name)))

(defun company-dcd--get-calltip-candidate-for-struct-constructor (lastcompl)
  "Almost the same as `company-dcd--get-calltip-candidates', but call `company-dcd--replace-this-to-struct-name' before parsing."
  (let ((buf (get-buffer-create company-dcd--output-buffer-name)))
    (company-dcd--call-process-for-calltips)
    (with-current-buffer buf
      (company-dcd--replace-this-to-struct-name lastcompl)
      (company-dcd--parse-calltips))
    ))

(defun company-dcd--calltips-for-struct-constructor (command &optional arg &rest ignored)
  "Company \"backend\" for DCD struct/class constructor calltip completion."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-dcd--calltips))
    (prefix (company-dcd--calltip-completion-available))
    (candidates
     (company-dcd--get-calltip-candidate-for-struct-constructor arg))
    (post-completion (company-dcd--calltip-action arg))
    (doc-buffer (company-dcd--get-completion-documentation arg))
    (location (company-dcd--get-completion-location arg))))


;; Documentation display

(defun company-dcd--reformat-documentation ()
  "Prepare a documentation string for display.
Currently, it simply unescapes `\\n' unless it's in $(D ...) closure."
  (with-current-buffer (get-buffer company-dcd--documentation-buffer-name)
    (goto-char (point-min))
    (while (not (equal (point) (point-max)))
      (cond
       ((looking-at (rx "\\n"))
	(delete-char 2)
	(insert "\n"))
       ((looking-at (rx "$(")) ; skip D expr.
	(forward-sexp 2))
       )
      (forward-char))
    ))

(defun company-dcd--get-ddoc ()
  "Retrieve symbol documentation using \"dcd-client --doc\".

Return nil on error or if the symbol is not documented."
  (let ((args
         (append
          (company-dcd--build-args (company-dcd--cursor-position))
          '("--doc"))))

    (let ((result (company-dcd--call-process args)))
      (when (and
	     result			; invocation succeeded
	     (string-match (rx (not (syntax whitespace)))
			   result))	; result not empty (contains non-whitespace)
	result))))

(defun company-dcd-show-ddoc-with-buffer ()
  "Display Ddoc of symbol at point using `display-buffer'."
  (interactive)
  (let ((raw-doc (company-dcd--get-ddoc)))
    (if raw-doc
	(progn
	  (with-current-buffer (get-buffer-create company-dcd--documentation-buffer-name)
	    (erase-buffer)
	    (insert raw-doc))
	  (company-dcd--reformat-documentation)
	  (display-buffer (get-buffer-create company-dcd--documentation-buffer-name)))
      (message "No documentation for the symbol at point."))))


(defun company-dcd--call-process-with-compl (lastcompl switch)
  "Call dcd-client with a hypothetically-expanded completion candidate.

Create a temporary buffer, which is a copy of the current buffer but with
LASTCOMPL expanded.  Execute DCD with the additional parameter SWITCH.
Return the result."
  (let ((src (buffer-string))
	(pt (point)))
    (with-temp-buffer
      (insert src)
      (goto-char pt)

      (delete-char (- 0 (length (company-grab-symbol))))
      (save-excursion
	(insert lastcompl))
      (forward-char)

      (company-dcd--call-process
       (append
	(company-dcd--build-args (company-dcd--cursor-position))
	(list switch))))))

(defun company-dcd--get-completion-documentation (lastcompl)
  "Company callback for displaying the documentation for a completion candidate."
  (let ((raw-doc (company-dcd--call-process-with-compl lastcompl "--doc")))
    (when raw-doc
      (company-doc-buffer
       (with-current-buffer (get-buffer-create company-dcd--documentation-buffer-name)
	 (erase-buffer)
	 (insert raw-doc)
	 (company-dcd--reformat-documentation)
	 (buffer-string))))))


;; Go to definition
;; Thanks to jedi.el by Takafumi Arakaki

(defcustom company-dcd--goto-definition-marker-ring-length 16
  "Length of marker ring to store `company-dcd-goto-definition' call positions."
  :group 'company-dcd)

(defvar company-dcd--goto-definition-marker-ring
  (make-ring company-dcd--goto-definition-marker-ring-length)
  "Ring that stores company-dcd--goto-symbol-declaration.")

(defsubst company-dcd--goto-def-push-marker ()
  "Push marker at point to goto-def ring."
  (ring-insert company-dcd--goto-definition-marker-ring (point-marker)))

(defun company-dcd-goto-def-pop-marker ()
  "Goto the point where `company-dcd-goto-definition' was last called."
  (interactive)
  (if (ring-empty-p company-dcd--goto-definition-marker-ring)
      (error "Marker ring is empty, can't pop")
    (let ((marker (ring-remove company-dcd--goto-definition-marker-ring 0)))
      (switch-to-buffer (or (marker-buffer marker)
                            (error "Buffer has been deleted")))
      (goto-char (marker-position marker))
      ;; Cleanup the marker so as to avoid them piling up.
      (set-marker marker nil nil))))

(cl-defstruct company-dcd--position-data file type offset)

(defun company-dcd-goto-definition ()
  "Goto declaration of symbol at point."
  (interactive)
  (when (company-dcd--call-process-for-symbol-declaration)
    (let ((data (company-dcd--parse-output-for-get-symbol-declaration)))
      (if data
	  (let ((file (company-dcd--position-data-file data))
		(offset (company-dcd--position-data-offset data)))
	    (company-dcd--goto-def-push-marker)
	    (unless (string=  file "stdin") ; the declaration is in the current file
	      (find-file file))
	    (goto-char (byte-to-position offset)))
	(message "Not found")))))

(defun company-dcd--get-completion-location (lastcompl)
  "Company callback for opening the definition for a completion candidate."
  (when (company-dcd--call-process-with-compl lastcompl "--symbolLocation")
    (let ((data (company-dcd--parse-output-for-get-symbol-declaration)))
      (when data
	(let* ((file (company-dcd--position-data-file data))
	       (offset (company-dcd--position-data-offset data))
	       (buffer (if (string=  file "stdin")
			   (current-buffer)
			 (find-file-noselect file)))
	       (position (with-current-buffer buffer (byte-to-position offset))))
	  (cons buffer position))))))

;; Utilities for goto-definition

(defun company-dcd--word-char-p (char)
  "Return t if CHAR is a D word char (part of an identifier), nil otherwise."
  (member (char-syntax char) '(?w ?_)))

(defun company-dcd--call-process-for-symbol-declaration ()
  "Call process for `dcd-client --symbolLocation'."
  (let ((pos (company-dcd--cursor-position)))

    ;; Work around https://github.com/Hackerpilot/DCD/issues/98
    (when (and
	   (not (company-dcd--word-char-p (char-before (point))))
	   (company-dcd--word-char-p (char-after (point))))
      (setq pos (1+ pos)))

    (company-dcd--call-process
     (append (company-dcd--build-args pos) '("--symbolLocation")))))

(defun company-dcd--parse-output-for-get-symbol-declaration ()
  "Parse output of `company-dcd--get-symbol-declaration'.

Output is a `company-dcd--position-data', whose `type' is nil."
  (let ((buf (get-buffer-create company-dcd--output-buffer-name)))
    (with-current-buffer buf
      (goto-char (point-min))
      (if (not (string= "Not found\n" (buffer-string)))
          (progn (re-search-forward (rx (submatch (* nonl)) "\t" (submatch (* nonl)) "\n"))
                 (make-company-dcd--position-data
		  :file (match-string 1)
		  :offset (1+ (string-to-number (match-string 2)))))
        nil))
    ))

;;; Symbol search.

(defvar company-dcd--symbol-search-pattern
  (rx (and bol (submatch (* nonl)) "\t" (submatch char) "\t" (submatch (* digit)) eol))
  "Regex pattern to parse dcd output for symbol location.")

(defun company-dcd--parse-output-for-symbol-search ()
  "Return a list of company-dcd--position-data."
  (with-current-buffer company-dcd--output-buffer-name
    (goto-char (point-min))
    (let (res)
      (while (re-search-forward company-dcd--symbol-search-pattern nil t)
	(add-to-list 'res
		     (make-company-dcd--position-data
		      :file (match-string 1)
		      :type (match-string 2)
		      :offset (string-to-number (match-string 3)))
		     ))
      res)))

(defun company-dcd--call-process-for-symbol-search (str)
  "Invoke dcd-client to find symbol STR."
  (let ((args
         (append
          (company-dcd--build-args)
          '("--search")
	  (list str))))
    (company-dcd--call-process args)))

(defun company-dcd--symbol-search (str)
  "Search symbol using DCD with query STR.

Return a list of `company-dcd--position-data' structs."
  (when (company-dcd--call-process-for-symbol-search str)
    (company-dcd--parse-output-for-symbol-search)))

(defun company-dcd--pos-data-to-ivy-candidate-string (pos-data)
  (with-current-buffer (company-dcd--find-file-of-pos-data pos-data)
    (company-dcd--goto-char-of-pos-data pos-data)
    (let ((line-string (company-dcd--line-string-at-pos)))
      (format "%s:%s:%s\n%s"
	      (company-dcd--position-data-file pos-data)
	      (company-dcd--position-data-type pos-data)
	      (company-dcd--position-data-offset pos-data)
	      line-string)
      )))

(defun company-dcd--ivy-candidate-string-to-pos-data (str)
  (string-match (rx (submatch (* nonl)) ":" (submatch (* nonl)) ":" (submatch (* nonl)) "\n" (* nonl)) str)
  (let ((file (match-string 1 str))
	(type (match-string 2 str))
	(offset (string-to-number (match-string 3 str))))
    (make-company-dcd--position-data :file file :type type :offset offset)))

(defun company-dcd--find-file-of-pos-data (pos-data)
  (find-file-noselect (company-dcd--position-data-file pos-data)))

(defun company-dcd--goto-char-of-pos-data (pos-data)
  (goto-char (byte-to-position (company-dcd--position-data-offset pos-data))))

(defun company-dcd--line-string-at-pos ()
  (let ((beg (point-at-bol))
	(end (point-at-eol)))
    (buffer-substring-no-properties beg end)))


(defun company-dcd--read-query-or-region-str ()
  "If region is active, return the region string.
Else, read query."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (read-string "Query: ")))

(defun company-dcd-ivy-search-symbol ()
  (interactive)
  (let* ((ivy-format-function 'ivy-format-function-arrow)
	 (query (company-dcd--read-query-or-region-str))
	 (candidates (company-dcd--symbol-search query))
	 (candidates-strlist (mapcar 'company-dcd--pos-data-to-ivy-candidate-string candidates))
	 (res (company-dcd--ivy-candidate-string-to-pos-data (ivy-read "Search: " candidates-strlist))))
    (company-dcd--goto-def-push-marker)
    (switch-to-buffer (company-dcd--find-file-of-pos-data res))
    (company-dcd--goto-char-of-pos-data res)
    ))

;;; Automatic import path detection.
(defvar company-dcd--imports-cache (make-hash-table :test #'equal)
  "A cache variable to store import paths from dub.")

(defun company-dcd--get-project-dir ()
  "Get current dub project dir"
  (fldd--get-project-dir))

(defun company-dcd--get-include-dirs ()
  "Get include dir using dub."
  (fldd--get-dub-package-dirs))

(defun company-dcd--initialize-imports-cache ()
  (setq company-dcd--imports-cache (make-hash-table :test #'equal)))

(defun company-dcd--delete-imports-cache ()
  (interactive)
  (company-dcd--initialize-imports-cache))

(defun company-dcd--put-imports-cache (project-root-dir import-dirs)
  (puthash project-root-dir import-dirs company-dcd--imports-cache))

(defun company-dcd--get-imports-cache (project-root-dir)
  (gethash project-root-dir company-dcd--imports-cache))

(defun company-dcd--parent-directory (dir)
  "Return parent directory of DIR."
  (when dir
    (file-name-directory (directory-file-name (expand-file-name dir)))))

(defun company-dcd--search-file-up (name &optional path)
  "Search for file NAME in parent directories recursively, starting with PATH (or `default-directory')."
  (let ((tags-file-name (concat path name))
	(parent (company-dcd--parent-directory path))
	(path (or path default-directory)))
    (cond
     ((file-exists-p tags-file-name) tags-file-name)
     ((string= parent path) nil)
     (t (company-dcd--search-file-up name parent)))))

(defun company-dcd--find-imports-dub ()
  "Extract import flags from \"dub describe\" output."
  (let ((basedir (company-dcd--get-project-dir)))
    (if basedir
	(mapcar (lambda (x) (concat "-I" x)) (company-dcd--get-include-dirs))
      nil)))

(defconst company-dcd--dmd-import-path-pattern
  (rx bol "import path[" (one-or-more digit) "] = " (submatch (one-or-more not-newline)) eol))

(defun company-dcd--find-imports-dmd ()
  "Extract import flags from dmd configuration.

This runs \"dmd company-dcd-nonexisting-file-test\", and reads DMD's search
paths from stderr.

This method avoids needing to find the correct dmd.conf and parsing it correctly."
  (with-temp-buffer
    (call-process company-dcd-compiler nil t nil "company-dcd-nonexisting-file-test")
    (goto-char (point-min))
    (let (lines)
      (while (re-search-forward company-dcd--dmd-import-path-pattern nil t)
        (push (concat "-I" (match-string-no-properties 1)) lines))
      (nreverse lines))))

(defun company-dcd--add-imports ()
  "Send import flags of the current DUB project to dcd-server.

The root of the project is determined by the \"closest\" dub.json
or package.json file.
If cache was found, use it instead of calling dub."
  (interactive)
  (let* ((proj-dir (company-dcd--get-project-dir))
	 (cached-imports (and proj-dir (company-dcd--get-imports-cache proj-dir)))
	 (cached-or-dub-imports (or cached-imports (company-dcd--find-imports-dub))))

    ;; if cached-imports is not available, put dub-imports to cache.
    (when (not cached-imports)
      (company-dcd--put-imports-cache proj-dir cached-or-dub-imports))

    (company-dcd--call-process
     (append
      (company-dcd--build-args)
      (company-dcd--find-imports-dmd)
      cached-or-dub-imports
      ))))

(defvar company-dcd-mode-map (make-keymap))
(define-key company-dcd-mode-map (kbd "C-c ?") 'company-dcd-show-ddoc-with-buffer)
(define-key company-dcd-mode-map (kbd "C-c .") 'company-dcd-goto-definition)
(define-key company-dcd-mode-map (kbd "C-c ,") 'company-dcd-goto-def-pop-marker)
(define-key company-dcd-mode-map (kbd "C-c s") 'company-dcd-ivy-search-symbol)

;;;###autoload
(define-minor-mode company-dcd-mode "company-backend for Dlang Completion Demon, aka DCD."
  :init-value nil
  :lighter " DCD"
  :keymap company-dcd-mode-map
  (if company-dcd-mode
      (progn (company-mode-on)
	     (yas-minor-mode-on)
	     (company-dcd-maybe-start-server)
	     (company-dcd--add-imports)
	     (add-to-list 'company-backends 'company-dcd)
	     (add-to-list 'popwin:special-display-config
			  `(,company-dcd--error-buffer-name :noselect t))
	     (add-to-list 'popwin:special-display-config
			  `(,company-dcd--documentation-buffer-name :position right :width 80)))
    ))

(provide 'company-dcd)
;;; company-dcd.el ends here
