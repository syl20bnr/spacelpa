;;; counsel-gtags.el --- ivy for GNU global -*- lexical-binding: t; -*-

;; Copyright (C) 2016 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-counsel-gtags
;; Package-Version: 20190422.1501
;; Version: 0.01
;; Package-Requires: ((emacs "25.1") (counsel "0.8.0") (seq "1.0"))

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

;; `counsel-gtags.el' provides `ivy' interface of GNU GLOBAL.

;;; Code:

(require 'counsel)
(require 'cl-lib)
(require 'rx)
(require 'seq)

(declare-function cygwin-convert-file-name-from-windows "cygw32.c")
(declare-function cygwin-convert-file-name-to-windows "cygw32.c")
(declare-function tramp-file-name-localname "tramp")
(declare-function tramp-dissect-file-name "tramp")

(defgroup counsel-gtags nil
  "`counsel' for GNU Global"
  :group 'counsel)

(defcustom counsel-gtags-ignore-case nil
  "Whether to ignore case in search pattern."
  :type 'boolean)

(defcustom counsel-gtags-path-style 'root
  "Path style of candidates.
The following values are supported:
- `root'     Show path from root of current project.
- `relative' Show path from current directory.
- `absolute' Show absolute path."
  :type '(choice (const :tag "Root of the current project" root)
                 (const :tag "Relative from the current directory" relative)
                 (const :tag "Absolute path" absolute)))

(defcustom counsel-gtags-auto-update nil
  "Whether to update the tag database when a buffer is saved to file."
  :type 'boolean)

(defcustom counsel-gtags-update-interval-second 60
  "Update tag database after this many seconds have passed.
If nil, the tags are updated every time a buffer is saved to file."
  :type '(choice (integer :tag "Update after this many seconds")
                 (boolean :tag "Update every time" nil)))

(defcustom counsel-gtags-use-input-at-point t
  "Whether to use input at point.
If non-nil, the symbol at point is used as default value when
searching for a tag."
  :type 'boolean)

(defcustom counsel-gtags-global-extra-update-options-list nil
  "List of extra arguments passed to global when updating database."
  :type 'list)

(defcustom counsel-gtags-gtags-extra-update-options-list nil
  "List of extra arguments passed to gtags when updating database."
  :type 'list)

(defcustom counsel-gtags-prefix-key "\C-c"
  "Key binding used for `counsel-gtags-mode-map'.
This variable does not have any effect unless
`counsel-gtags-use-suggested-key-map' is non-nil."
  :type 'string)

(defvaralias 'counsel-gtags-suggested-key-mapping 'counsel-gtags-use-suggested-key-map)
(defcustom counsel-gtags-use-suggested-key-map nil
  "Whether to use the suggested key bindings."
  :type 'boolean)
(make-obsolete-variable 'counsel-gtags-suggested-key-mapping 'counsel-gtags-use-suggested-key-map "0.01")

(defconst counsel-gtags--prompts
  '((definition . "Find Definition: ")
    (reference  . "Find Reference: ")
    (pattern    . "Find Pattern: ")
    (symbol     . "Find Symbol: ")))

(defconst counsel-gtags--complete-options
  '((file      . "-P")
    (pattern   . "-g")
    (reference . "-r")
    (symbol    . "-s")))

(defvar counsel-gtags--last-update-time 0)
(defvar counsel-gtags--context nil)
(defvar counsel-gtags--context-position 0)
(defvar counsel-gtags--original-default-directory nil
  "Last `default-directory' where command is invoked.")

(defun counsel-gtags--select-gtags-label ()
  "Get label from user to be used to generate tags."
  (let ((labels '("default" "native" "ctags" "pygments")))
    (ivy-read "GTAGSLABEL(Default: default): " labels)))

(defun counsel-gtags--generate-tags ()
  "Query user for tag generation and do so if accepted."
  (if (not (yes-or-no-p "File GTAGS not found. Run 'gtags'? "))
      (error "Abort generating tag files")
    (let* ((root (read-directory-name "Root Directory: "))
           (label (counsel-gtags--select-gtags-label))
           (default-directory root))
      (message "gtags is generating tags....")
      (unless (zerop (process-file "gtags" nil nil nil "-q"
                                   (concat "--gtagslabel=" label)))
        (error "Failed: 'gtags -q'"))
      root)))

(defun counsel-gtags--root ()
  "Get gtags root by looking at env vars or looking for GTAGS.

Will trigger tags generation if not found."
  (or (getenv "GTAGSROOT")
      (locate-dominating-file default-directory "GTAGS")
      (counsel-gtags--generate-tags)))

(defsubst counsel-gtags--windows-p ()
  "Whether we're inside non-free Gates OS."
  (memq system-type '(windows-nt ms-dos)))

(defun counsel-gtags--file-path-style ()
  "Return current `counsel-gtags-path-style' option as argument to global cmd.

Kept free of whitespaces."
  (format "--path-style=%s"
     (pcase counsel-gtags-path-style
       ((or 'relative 'absolute)
	(symbol-name counsel-gtags-path-style))
       ('root "through")
       (_
	(error "Unexpected counsel-gtags-path-style: %s"
	       (symbol-name counsel-gtags-path-style))))))

(defun counsel-gtags--command-options (type &optional extra-options)
  "Get list with options for global command according to TYPE.

Prepend EXTRA-OPTIONS.  If \"--result=.\" is in EXTRA-OPTIONS, it will have
precedence over default \"--result=grep\"."
  (let* ((options extra-options)
	 (has-result (seq-filter (lambda (opt)
				   (and (stringp opt)
					(string-prefix-p "--result=" opt)))
				 options)))
    (unless has-result
      (setq options (append '("--result=grep") options)))
    (let ((opt (assoc-default type counsel-gtags--complete-options)))
      (when opt
        (push opt options)))
    (push (counsel-gtags--file-path-style) options)
    (when counsel-gtags-ignore-case
      (push "-i" options))
    (when current-prefix-arg ;; XXX
      (push "-l" options))
    (when (getenv "GTAGSLIBPATH")
      (push "-T" options))
    options))

(defun counsel-gtags--string-looks-like-regex (s)
  "Return non-nil if S has special regex characters."
  (and s
       (save-match-data
	 (string-match (rx (any "." "^" "*" "+" "?" "{" "}" "[" "]"
				"$" "(" ")"))
		       s))))

(defun counsel-gtags--get-grep-command ()
  "Get a grep command to be used to filter candidates.

Returns a command without arguments.

Otherwise, returns nil if couldn't find any."
  (cl-loop
   for command in (list grep-command "rg" "ag" "grep")
   for actual-command = (and command
			     (let ((command-no-args (car
						     (split-string command))))
			       (executable-find command-no-args)))
   while (not actual-command)
   finally return actual-command))

(defun counsel-gtags--build-command-to-collect-candidates (query &optional extra-args)
  "Build command to collect condidates filtering by QUERY.

Used in `counsel-gtags--async-tag-query'.  Forward QUERY and EXTRA-ARGS to
`counsel-gtags--command-options'.
Since it's a tag query, we use definition as type when getting options"
  (mapconcat #'shell-quote-argument
	     (append
	      `("global")
	      (counsel-gtags--command-options 'definition extra-args)
	      `(,(counsel--elisp-to-pcre (ivy--regex query))))
	     " "))
(defun counsel-gtags--filter-tags (s)
  "Filter function receving S.

Extract the first part of each line, containing the tag."
  (replace-regexp-in-string (rx (char space) (* any) line-end)
			    ""
			    s))

(defun counsel-gtags--async-tag-query-process (query)
  "Add filter to tag query command.

Input for searching is QUERY.

Since we can't look for tags by regex, we look for their definition and filter
the location, giving us a list of tags with no locations."
  (counsel--async-command
   (counsel-gtags--build-command-to-collect-candidates query '("--result=ctags"))
   nil ;; default sentinel
   (lambda (p s)
     (counsel--async-filter p (counsel-gtags--filter-tags s)))))

(defun counsel-gtags--async-tag-query (query)
  "Gather the object names asynchronously for `ivy-read'.

Use global flags according to TYPE.

Forward QUERY to global command to be treated as regex.

Because «global -c» only accepts letters-and-numbers, we actually search for
tags matching QUERY, but filter the list.

Inspired on ivy.org's `counsel-locate-function'."
  (or
   (ivy-more-chars)
   (progn
     (counsel-gtags--async-tag-query-process query)
     '("" "Filtering …"))))

(defun counsel-gtags--file-and-line (candidate)
  "Return list with file and position per CANDIDATE.

Candidates are supposed to be strings of the form \"file:line\" as returned by
global. Line number is returned as number (and not string)."
  (if (and (counsel-gtags--windows-p)
           (string-match-p "\\`[a-zA-Z]:" candidate)) ;; Windows Driver letter
      (when (string-match "\\`\\([^:]+:[^:]+:\\):\\([^:]+\\)" candidate)
        (list (match-string-no-properties 1)
              (string-to-number (match-string-no-properties 2))))
    (let ((fields (split-string candidate ":")))
      (list (cl-first fields) (string-to-number (or (cl-second fields) "1"))))))

(defun counsel-gtags--resolve-actual-file-from (file-candidate)
  "Resolve actual file path from CANDIDATE taken from a global cmd query.

Note: candidates are handled as ⎡file:location⎦ and ⎡(file . location)⎦.
     FILE-CANDIDATE is supposed to be *only* the file part of a candidate."
  (let ((file-path-per-style
	 (concat
	  (pcase counsel-gtags-path-style
	    ((or 'relative 'absolute)
	     "")
	    ('root
	     (file-name-as-directory
	      (counsel-gtags--default-directory)))
	    (_
	     (error
	      "Unexpected counsel-gtags-path-style: %s"
	      (symbol-name counsel-gtags-path-style))))
	  file-candidate)))
    (file-truename file-path-per-style)))

(defun counsel-gtags--jump-to (candidate &optional push)
  "Call `find-file' and `forward-line' on file location from CANDIDATE .

Calls `counsel-gtags--push' at the end if PUSH is non-nil.
Returns (buffer line)"
  (cl-multiple-value-bind (file-path line)
      (counsel-gtags--file-and-line candidate)
    (let* ((default-directory (file-name-as-directory
			       (or counsel-gtags--original-default-directory
				   default-directory)))
	   (file (counsel-gtags--resolve-actual-file-from file-path))
	   (opened-buffer (find-file file)))
      ;; position correctly within the file
      (goto-char (point-min))
      (forward-line (1- line))
      (back-to-indentation)
      (if push
	  (counsel-gtags--push 'to))
      `(,opened-buffer ,line))))

(defun counsel-gtags--find-file (candidate)
  "Open file-at-position per CANDIDATE using `find-file'.

This is the `:action' callback for `ivy-read' calls."
  (with-ivy-window
    (swiper--cleanup)
    (counsel-gtags--push 'from)
    (counsel-gtags--jump-to candidate 'push)))

(defun counsel-gtags--read-tag-ivy-parameters (type)
  "Get `counsel-gtags--read-tag' the parameters from TYPE to call `ivy-read'."
  `(,(assoc-default type counsel-gtags--prompts)
    counsel-gtags--async-tag-query
    :initial-input ,(and counsel-gtags-use-input-at-point
			 (thing-at-point 'symbol))
    :unwind ,(lambda ()
	       (counsel-delete-process)
	       (swiper--cleanup))
    :dynamic-collection t))

(defun counsel-gtags--read-tag (type)
  "Prompt the user for selecting a tag using `ivy-read'.

Returns selected tag

Use TYPE ∈ '(definition reference symbol) for defining global parameters.
If `counsel-gtags-use-input-at-point' is non-nil, will use symbol at point as
initial input for `ivy-read'.

TYPE ∈ `counsel-gtags--prompts'

See `counsel-gtags--async-tag-query' for more info."
  (apply 'ivy-read
	 (plist-put
	  (counsel-gtags--read-tag-ivy-parameters type)
	  :caller 'counsel-gtags--read-tag)))



(defun counsel-gtags--tag-directory ()
  "Get directory from either GTAGSROOT env var or by running global."
  (with-temp-buffer
    (or (getenv "GTAGSROOT")
        (progn
          (unless (zerop (process-file "global" nil t nil "-p"))
            (error "GTAGS not found"))
          (goto-char (point-min))
          (let ((dir (buffer-substring-no-properties (point) (line-end-position))))
            (file-name-as-directory (if (eq system-type 'cygwin)
                                        (cygwin-convert-file-name-from-windows dir)
                                      dir)))))))

(defun counsel-gtags--process-lines (command &rest args)
  "Like `process-lines' on COMMAND and ARGS, but using `process-file'.

`process-lines' does not support Tramp because it uses `call-process'.  Using
`process-file' makes Tramp support auto-magical."
  ;; Space before buffer name to make it "invisible"
  (let ((global-run-buffer (get-buffer-create (format " *global @ %s*" default-directory))))
    ;; The buffer needs to be cleared, this can be done after split-string,
    ;; but for now it is better to keep it like this for debugging purposed
    ;; between calls
    (with-current-buffer global-run-buffer
      (erase-buffer))
    (apply #'process-file command
	   nil ;; no input file
	   global-run-buffer;;BUFFER
	   nil ;;DISPLAY
	   args)
    (with-current-buffer global-run-buffer
      (split-string
       (buffer-string) "\n" t))))

(defun counsel-gtags--collect-candidates (type tagname encoding extra-options)
  "Collect lines for ⎡global …⎦ using TAGNAME as query.

TAGNAME may be nil, suggesting a match-any query.
Use TYPE to specify query type (tag, file).
Use ENCODING to specify encoding.
Use EXTRA-OPTIONS to specify encoding.

This is for internal use and not for final user."
  (let* ((options (counsel-gtags--command-options type extra-options))
         (default-directory default-directory)
         (coding-system-for-read encoding)
         (coding-system-for-write encoding)
	 (query-as-list (pcase tagname
			  ((pred null) '())
			  ("" '())
			  (`definition '())
			  (_ (list tagname))))
	 (global-args (append (reverse options) query-as-list)))
    (apply #'counsel-gtags--process-lines "global" global-args)))

(defun counsel-gtags--select-file-ivy-parameters (type tagname extra-options auto-select-only-candidate)
  "Get `counsel-gtags--select-file' the parameters from TYPE to call `ivy-read'."
  (if (string-empty-p tagname)
      (message "No candidate tags")
    (let* ((root (counsel-gtags--default-directory))
           (encoding buffer-file-coding-system)
           (default-directory root)
           (collection (counsel-gtags--collect-candidates
			type tagname encoding extra-options))
           (ivy-auto-select-single-candidate t) ;; see issue #7
           )
      `("Pattern: " ,collection
        :action counsel-gtags--find-file))))

(defun counsel-gtags--select-file (type tagname &optional extra-options auto-select-only-candidate)
  "Prompt the user to select a file_path:position according to query.

Use TYPE ∈ '(definition reference symbol) for defining global parameters.
Use TAGNAME for global query.
Use AUTO-SELECT-ONLY-CANDIDATE to skip `ivy-read' if have a single candidate.
Extra command line parameters to global are forwarded through EXTRA-OPTIONS."
  (let* ((the-ivy-arguments
	  (counsel-gtags--select-file-ivy-parameters type
						     tagname
						     extra-options
						     auto-select-only-candidate))
	 (collection (cadr the-ivy-arguments)))
    (if (and auto-select-only-candidate (= (length collection) 1))
        (counsel-gtags--find-file (car collection))
      ;; else
      (apply 'ivy-read (plist-put
			the-ivy-arguments
			:caller 'counsel-gtags--select-file)))))

;;;###autoload
(defun counsel-gtags-find-definition (tagname)
  "Search for TAGNAME definition in tag database.
Prompt for TAGNAME if not given."
  (interactive
   (list (counsel-gtags--read-tag 'definition)))
  (counsel-gtags--select-file 'definition tagname))

;;;###autoload
(defun counsel-gtags-find-reference (tagname)
  "Search for TAGNAME reference in tag database.
Prompt for TAGNAME if not given."
  (interactive
   (list (counsel-gtags--read-tag 'reference)))
  (counsel-gtags--select-file 'reference tagname))

;;;###autoload
(defun counsel-gtags-find-symbol (tagname)
  "Search for TAGNAME symbol in tag database.
Prompt for TAGNAME if not given."
  (interactive
   (list (counsel-gtags--read-tag 'symbol)))
  (counsel-gtags--select-file 'symbol tagname))

(defconst counsel-gtags--include-regexp
  "\\`\\s-*#\\(?:include\\|import\\)\\s-*[\"<]\\(?:[./]*\\)?\\(.*?\\)[\">]")

(defun counsel-gtags--include-file ()
  "Get ⎡#include …⎦ from first line."
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (when (string-match counsel-gtags--include-regexp line)
      (match-string-no-properties 1 line))))

(defun counsel-gtags--default-directory ()
  "Return default directory per `counsel-gtags-path-style'.

Useful for jumping from a location when using global commands (like with
\"--from-here\")."
  (setq counsel-gtags--original-default-directory
        (cl-case counsel-gtags-path-style
          ((relative absolute) default-directory)
          (root (counsel-gtags--root)))))

(defun counsel-gtags--get-files ()
  "Get a list of all files from global."
  (let* ((encoding buffer-file-coding-system)
	 (candidates (counsel-gtags--collect-candidates
		      'file
		      nil ;; match any
		      encoding
		      nil))
	 (files (mapcar (lambda (candidate)
			  (cl-multiple-value-bind (file-path _)
			      (counsel-gtags--file-and-line candidate)
			    file-path))
			candidates
			)))
    (cl-remove-duplicates files
			  :test #'string-equal)))

(defun counsel-gtags--find-file-ivy-parameters (filename)
  "Get `counsel-gtags-find-file' the parameters from FILENAME to call `ivy-read'."

  (let* ((initial-input (or filename
			    (counsel-gtags--include-file)))
         (collection (counsel-gtags--get-files)))
    `("Find File: "
      ,collection
      :initial-input ,initial-input
      :action counsel-gtags--find-file)))

;;;###autoload
(defun counsel-gtags-find-file (&optional filename)
  "Search/narrow for FILENAME among tagged files."
  (interactive)
  (apply 'ivy-read
	 (plist-put
	  (counsel-gtags--find-file-ivy-parameters filename)
	  :caller 'counsel-gtags-find-file-name)))

;;;###autoload
(defun counsel-gtags-go-backward ()
  "Go to previous position in context stack."
  (interactive)
  (unless counsel-gtags--context
    (user-error "Context stack is empty"))
  (catch 'exit
    (let ((position counsel-gtags--context-position)
          (num-entries (length counsel-gtags--context)))
      (while (< (cl-incf position) num-entries)
        (when (counsel-gtags--goto position)
          (setq counsel-gtags--context-position position)
          (throw 'exit t))))))
(defalias 'counsel-gtags-pop 'counsel-gtags-go-backward)
(make-obsolete 'counsel-gtags-pop 'counsel-gtags-go-backward "0.01")

;;;###autoload
(defun counsel-gtags-go-forward ()
  "Go to next position in context stack."
  (interactive)
  (unless counsel-gtags--context
    (user-error "Context stack is empty"))
  (catch 'exit
    (let ((position counsel-gtags--context-position))
      (while (>= (cl-decf position) 0)
        (when (counsel-gtags--goto position)
          (setq counsel-gtags--context-position position)
          (throw 'exit t))))))

(defun counsel-gtags--goto (position)
  "Go to POSITION in context stack.
  Return t on success, nil otherwise."
  (let ((context (nth position counsel-gtags--context)))
    (when (and context
               (cond
                ((plist-get context :file)
                 (find-file (plist-get context :file)))
                ((and (plist-get context :buffer)
                      (buffer-live-p (plist-get context :buffer)))
                 (switch-to-buffer (plist-get context :buffer)))
                (t
                 nil)))
      (goto-char (point-min))
      (forward-line (1- (plist-get context :line)))
      t)))

(defun counsel-gtags--push (direction)
  "Add new entry to context stack.

  DIRECTION ∈ '(from, to)."
  (let ((new-context (list :file (and (buffer-file-name)
                                      (file-truename (buffer-file-name)))
                           :buffer (current-buffer)
                           :line (line-number-at-pos)
                           :direction direction)))
    (setq counsel-gtags--context
          (nthcdr counsel-gtags--context-position counsel-gtags--context))
    ;; We do not want successive entries with from-direction,
    ;; so we remove the old one.
    (let ((prev-context (car counsel-gtags--context)))
      (if (and (eq direction 'from)
               (eq (plist-get prev-context :direction) 'from))
          (pop counsel-gtags--context)))
    (push new-context counsel-gtags--context)
    (setq counsel-gtags--context-position 0)))

(defun counsel-gtags--make-gtags-sentinel (action)
  "Return default sentinel that messages success/failed exit.

  Message printed has ACTION as detail."
  (lambda (process _event)
    (when (eq (process-status process) 'exit)
      (if (zerop (process-exit-status process))
          (message "Success: %s TAGS" action)
        (message "Failed: %s TAGS(%d)" action (process-exit-status process))))))

;;;###autoload
(defun counsel-gtags-create-tags (rootdir label)
  "Create tag database in ROOTDIR.
  LABEL is passed as the value for the environment variable GTAGSLABEL.
  Prompt for ROOTDIR and LABEL if not given.  This command is asynchronous."
  (interactive
   (list (read-directory-name "Directory: " nil nil t)
         (counsel-gtags--select-gtags-label)))
  (let* ((default-directory rootdir)
         (proc-buf (get-buffer-create " *counsel-gtags-tag-create*"))
         (proc (start-file-process
                "counsel-gtags-tag-create" proc-buf
                "gtags" "-q" (concat "--gtagslabel=" label))))
    (set-process-sentinel
     proc
     (counsel-gtags--make-gtags-sentinel 'create))))

(defun counsel-gtags--remote-truename (&optional file-path)
  "Return real file name for file path FILE-PATH in remote machine.

  If file is local, return its `file-truename'

  FILE-PATH defaults to current buffer's file if it was not provided."
  (let ((filename (or file-path
                      (buffer-file-name)
                      (error "This buffer is not related to any file")))
	(default-directory (file-name-as-directory default-directory)))
    (if (file-remote-p filename)
        (tramp-file-name-localname (tramp-dissect-file-name filename))
      (file-truename filename))))

(defun counsel-gtags--read-tag-directory ()
  "Get directory for tag generation from user."
  (let ((dir (read-directory-name "Directory tag generated: " nil nil t)))
    ;; On Windows, "gtags d:/tmp" work, but "gtags d:/tmp/" doesn't
    (directory-file-name (expand-file-name dir))))

(defsubst counsel-gtags--how-to-update-tags ()
  "Read prefix input from user and return corresponding type of tag update."
  (cl-case (prefix-numeric-value current-prefix-arg)
    (4 'entire-update)
    (16 'generate-other-directory)
    (otherwise 'single-update)))

(defun counsel-gtags--update-tags-command (how-to)
  "Build global command line to update commands.
  HOW-TO ∈ '(entire-update generate-other-directory single-update) per
  `counsel-gtags--how-to-update-tags' (user prefix)."
  ;; note: mayble use `-flatten' here
  (cl-case how-to
    (entire-update
     (append '("global" "-u")
	     counsel-gtags-global-extra-update-options-list))
    (generate-other-directory
     (append '("gtags")
	     counsel-gtags-global-extra-update-options-list
	     (list (counsel-gtags--read-tag-directory))))
    (single-update
     (append '("global" "--single-update")
	     counsel-gtags-global-extra-update-options-list
	     (list (counsel-gtags--remote-truename))))))

(defun counsel-gtags--update-tags-p (how-to interactive-p current-time)
  "Should we update tags now?.

  Will update if being called interactively per INTERACTIVE-P.
  If HOW-TO equals 'single-update, will update only if
  `counsel-gtags-update-interval-second' seconds have passed up to CURRENT-TIME."
  (or interactive-p
      (and (eq how-to 'single-update)
           (buffer-file-name)
           (or (not counsel-gtags-update-interval-second)
               (>= (- current-time counsel-gtags--last-update-time)
                   counsel-gtags-update-interval-second)))))

;;;###autoload
(defun counsel-gtags-update-tags ()
  "Update tag database for current file.
  Changes in other files are ignored.  With a prefix argument, update
  tags for all files.  With two prefix arguments, generate new tag
  database in prompted directory."
  (interactive)
  (let ((how-to (counsel-gtags--how-to-update-tags))
        (interactive-p (called-interactively-p 'interactive))
        (current-time (float-time (current-time))))
    (when (counsel-gtags--update-tags-p how-to interactive-p current-time)
      (let* ((cmds (counsel-gtags--update-tags-command how-to))
             (proc (apply #'start-file-process "counsel-gtags-update-tag" nil cmds)))
        (if (not proc)
            (message "Failed: %s" (mapconcat 'identity cmds " "))
          (set-process-sentinel proc (counsel-gtags--make-gtags-sentinel 'update))
          (setq counsel-gtags--last-update-time current-time))))))

(defun counsel-gtags--from-here (tagname)
  "Try to open file by querying TAGNAME and \"--from-here\"."
  (let* ((line (line-number-at-pos))
         (root (counsel-gtags--remote-truename (counsel-gtags--default-directory)))
         (file (counsel-gtags--remote-truename))
         (from-here-opt (format "--from-here=%d:%s" line (file-relative-name file root))))
    (counsel-gtags--select-file 'from-here tagname (list from-here-opt) t)))

;;;###autoload
(defun counsel-gtags-dwim ()
  "Find definition or reference of thing at point (Do What I Mean).
  If point is at a definition, find its references, otherwise, find
  its definition."
  (interactive)
  (let ((cursor-symbol (thing-at-point 'symbol)))
    (if (and (buffer-file-name) cursor-symbol)
        (counsel-gtags--from-here (substring-no-properties cursor-symbol))
      (call-interactively 'counsel-gtags-find-definition))))

(defvar counsel-gtags-mode-name " CounselGtags")
(defvar counsel-gtags-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode counsel-gtags-mode ()
  "Minor mode of counsel-gtags.
  If `counsel-gtags-update-tags' is non-nil, the tag files are updated
  after saving buffer."
  :init-value nil
  :global     nil
  :keymap     counsel-gtags-mode-map
  :lighter    counsel-gtags-mode-name
  (if counsel-gtags-mode
      (when counsel-gtags-auto-update
        (add-hook 'after-save-hook 'counsel-gtags-update-tags nil t))
    (when counsel-gtags-auto-update
      (remove-hook 'after-save-hook 'counsel-gtags-update-tags t))))

;; Key mapping of gtags-mode.
(when counsel-gtags-use-suggested-key-map
  ;; Current key mapping.
  (let ((command-table '(("s" . counsel-gtags-find-symbol)
                         ("r" . counsel-gtags-find-reference)
                         ("t" . counsel-gtags-find-definition)
                         ("d" . counsel-gtags-find-definition)))
        (key-func (if (string-prefix-p "\\" counsel-gtags-prefix-key)
                      #'concat
                    (lambda (prefix key) (kbd (concat prefix " " key))))))
    (cl-loop for (key . command) in command-table
             do
             (define-key counsel-gtags-mode-map (funcall key-func counsel-gtags-prefix-key key) command))

    ;; common
    (define-key counsel-gtags-mode-map "\C-]" 'counsel-gtags--from-here)
    (define-key counsel-gtags-mode-map "\C-t" 'counsel-gtags-go-backward)
    (define-key counsel-gtags-mode-map "\e*" 'counsel-gtags-go-backward)
    (define-key counsel-gtags-mode-map "\e." 'counsel-gtags-find-definition)))

(provide 'counsel-gtags)

;;; counsel-gtags.el ends here
