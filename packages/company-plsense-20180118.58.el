;;; company-plsense.el --- Company backend for Perl -*- lexical-binding:t -*-

;; Copyright (C) 2017 by Troy Hinckley

;; Author: Troy Hinckley <troy.hinckley@gmail.com>
;; URL: https://github.com/CeleritasCelery/company-plsense
;; Package-Version: 20180118.58
;; Version: 0.2.0
;; Package-Requires: ((company "0.9.3") (cl-lib "0.5.0") (dash "2.12.0") (s "1.12") (emacs "24"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Company backend for Perl.
;; Analysis is done through PlSense located at <https://github.com/aki2o/plsense>.
;; Note that aki2o already has an Emacs autocompletion backend for autocomple.el
;; located at <https://github.com/aki2o/emacs-plsense>
;; To use this package you must download PlSense and go through the setup.
;;
;; If there are libraries your Perl code uses that are not part of @INC make
;; sure they are either part of lib-path in `company-plsense-config-path' (Note
;; that this variable can only take one path) or they are part of the $PERL5LIB
;; within the Emacs environment. If you make changes to $PERL5LIB you will need
;; to restart the PlSense server to apply the changes.

;;; Code:

(require 'company) ; company fontend
(require 'cl-lib)  ; `cl-case'
(require 'tq)      ; transcation queues. Note modifications below
(require 'dash)    ; List manipulation library
(require 's)       ; String manipulation library


;;; Customizable variables
(defgroup company-plsense nil
  "company back-end for perl5"
  :prefix "company-plsense-"
  :group 'programming
  :link '(url-link :tag "Github" "https://github.com/CeleritasCelery/company-plsense"))

(defcustom company-plsense-executable "plsense"
  "The location of the PlSense executable. Default is to search for it on $PATH.")
(defcustom company-plsense-ignore-compile-errors t
  "Ignore errors from PlSense related to compiling libraries and imported modules.")
(defcustom company-plsense-config-path "~/.plsense"
  "The location of the plsense config file. Run 'plsense' from the shell to generate this file.")
(defcustom company-plsense-braces-autopaired t
  "Whether or not to assume that braces are auto-paired.")


;;; State variables
(defvar company-plsense--last-error ""
  "Used to keep track of the last error from the plsese server so that we don't spam error messages.")
(defvar company-plsense--queue nil
  "Modified transaction queue to keep all server commands.")
(defvar company-plsense--process nil
  "Lisp object for the PlSense server.")
(defvar company-plsense--server-started-p nil
  "Server started predicate.")
(defvar company-plsense-enabled-modes '(cperl-mode perl-mode)
  "Major modes that will use plsense.")

(defvar company-plsense--current-file ""
  "The file the user is currently working in.")
(defvar company-plsense--opening-file ""
  "The file the server is currently trying to open.")
(defvar company-plsense--current-package ""
  "The package the user is currently working in.")
(defvar company-plsense--current-function ""
  "The function the user is currently working in.")

(defvar company-plsense--function-list '()
  "A list of all the functions in current file that PlSense knows about.
This list has the form:
\((function_foo 588 . 640)
 (function_bar 557 . 587)
 (function_baz 605 . 630))
where the first item is the name of the function and the cons pair
is its location within the buffer. This data is used to determine
the current scope the user is operating in.")
(defvar company-plsense--package-list '()
  "A list of all the packages in the current file that PlSense knows about.
This list has the same form as `company-plsense--function-list' above.")
(defvar company-plsense--changed-regions nil
  "Has the same form as `company-plsense--function-list' but contains changed regions.")
(defvar company-plsense--prev-symbol nil
  "Symbol surrounding previous changed region.")

(defvar company-plsense--opened-files '()
  "A list of all files that have been opened this session.
Every file needs to be opened before it can provide completion candidates.")

(defvar company-plsense--done-re "Done"
  "Regular expression matching a done repsonse from the server.")
(defvar company-plsense--package-re (rx bol (* space) "package" (+ space) (group (+ (any alnum "_:"))) (* space) ";")
  "Regular expression matching a package name.")
(defvar company-plsense--sub-re (rx bol (* space) "sub" (+ space) (group (+ (any alnum "_"))))
  "Regular expression matching a function name.")
(defvar company-plsense--active nil
  "Company PlSense hooks are present in the buffer")

(make-variable-buffer-local 'company-plsense--function-list)
(make-variable-buffer-local 'company-plsense--package-list)
(make-variable-buffer-local 'company-plsense--changed-regions)
(make-variable-buffer-local 'company-plsense--prev-symbol)
(make-variable-buffer-local 'company-plsense--active)


;;; Server interface commands

(defun company-plsense--process-running-p ()
  "Return t if the there is an active PlSense process running, else nil."
  (and (processp company-plsense--process)
       (eq (process-status (process-name company-plsense--process)) 'run)
       company-plsense--queue))

(defun company-plsense--server-request (cmd &optional callback)
  "Post a asynchronous CMD to the PlSense server.
This is higher level wrapper for `company-plsense--async-request'
that does some post processing and returns CALLBACK."
  (unless (company-plsense--process-running-p)
    (company-plsense--start-process))
  (company-plsense--async-request cmd (if callback
                           (lambda (_closure resp)
                             (funcall callback (replace-regexp-in-string "\n?>\\s-\\'" "" resp)))
                         nil)))

(defun company-plsense--server-query (cmd &optional timeout)
  "Post a synchronous CMD to the PlSense server.
This is higher level wrapper for `company-plsense--sync-request'
that does some post processing before TIMEOUT."
  (unless (company-plsense--process-running-p)
    (company-plsense--start-process))
  (company-plsense--sync-request cmd timeout))

(defun company-plsense--async-request (cmd callback)
  "Post a asynchronous CMD to the PlSense server and call CALLBACK."
  (tq-enqueue company-plsense--queue (if cmd (concat cmd "\n") "") ">\\s-\\'" nil callback t))

(defun company-plsense--sync-request (cmd &optional timeout)
  "Post a synchronous CMD to the PlSense server with TIMEOUT."
  (let ((done nil)
        (reply "")
        (limit (if timeout (* 5 timeout) 25))
        (counter 0))
    (tq-enqueue company-plsense--queue
                (if cmd (concat cmd "\n") "")
                ">\\s-\\'"
                nil
                (lambda (_closure resp)
                  (setq reply resp)
                  (setq done t))
                t)
    (while (and (< counter limit) (not done))
      (accept-process-output company-plsense--process 0.2 nil t)
      (cl-incf counter))
    (if done
        (replace-regexp-in-string "\n?>\\s-\\'" "" reply)
      (setcar company-plsense--queue nil))))

(defun company-plsense--start-process ()
  "Create the PlSense process and transaction queue."
  (if (not (file-exists-p company-plsense-config-path))
      (message "PlSense config file '%s' does not exist. run 'plsense' on shell to initalize it."
               (expand-file-name company-plsense-config-path))
    (when (and (processp company-plsense--process)
               (process-status (process-name company-plsense--process)))
      (kill-process company-plsense--process)
      (sleep-for 1))
    (setq company-plsense--process
          (start-process-shell-command "plsense" nil (concat company-plsense-executable " --interactive")))
    (setq company-plsense--queue (company-plsense--tq-create company-plsense--process))
    (company-plsense--sync-request nil)
    (set-process-query-on-exit-flag company-plsense--process nil)
    company-plsense--queue))


;;; Server management commands

(defun company-plsense-executable-version ()
  "Show the PlSense Version."
  (interactive)
  (message (shell-command-to-string (concat company-plsense-executable " --version"))))

(defun company-plsense-server-status ()
  "Show the PlSense server status."
  (interactive)
  (message (company-plsense--server-query "serverstatus" 10)))

(defun company-plsense-buffer-ready ()
  "Return the ready status of the buffer.
Reply 'Yes' i buffer is ready, 'No' if it has not been loaded, and 'Not Found'
 if either the file could not be found or the file failed to compile cleanly."
  (interactive)
  (message (company-plsense--server-query
            (concat "ready " (buffer-file-name (current-buffer))) 10)))

(defun company-plsense-server-command (cmd)
  "Run an arbitrary CMD on the PlSense server synchronously."
  (interactive "Mcommand: ")
  (message (company-plsense--server-query cmd)))

(defun company-plsense--open-file (file)
  "Opening causes PlSense to load FILE and use it for completion candidates.
Every file must be loaded once per session."
  (when (and
         (file-exists-p file)
         (not (file-directory-p file))
         (company-plsense--server-request
          (concat "open " (expand-file-name file))
          (lambda (resp)
            (when (string-match company-plsense--done-re resp)
              (add-to-list 'company-plsense--opened-files file)
              (setq company-plsense--current-file file)
              (setq company-plsense--current-package "main")
              (setq company-plsense--current-function ""))
            (setq company-plsense--opening-file ""))))))

(defun company-plsense--kill-process ()
  "Force kill the PlSense server."
  (company-plsense--reset-location)
  (setq company-plsense--opened-files '())
  (setq company-plsense--last-error "")
  (when (company-plsense--process-running-p)
    (tq-close company-plsense--queue)))

(defun company-plsense-start-server ()
  "Start the PlSense server if it is not already running."
  (interactive)
  (message "Starting plsense server...")
  (company-plsense--server-request
   "serverstatus"
   (lambda (resp)
     (if (< 0 (s-count-matches "Not" resp))
         (company-plsense--server-request
          "serverstart"
          (lambda (resp)
            (setq company-plsense--opened-files '())
            (setq company-plsense--changed-regions nil)
            (company-plsense--reset-location)
            (when (string-match company-plsense--done-re resp)
              (message "plsense server started"))))
       (message "plsense server already running")))))

(defun company-plsense-stop-server ()
  "Attempt to stop the PlSense server.
If it takes longer then 5 seconds, force kill it."
  (interactive)
  (message "Stopping plsense server...")
  (when (company-plsense--process-running-p)
    (company-plsense--sync-request "serverstop" 5))
  (company-plsense--kill-process)
  (message "plsense server stopped")
  t)

(defun company-plsense-restart-server ()
  "Restart PlSense server.
Use this command when either the server failed
to start or when `company-plsense-server-status'
reveals that not all work servers are running."
  (interactive)
  (company-plsense-stop-server)
  (sleep-for 1)
  (company-plsense-start-server))


;;; Functions to monitor the current file and update the server.

(defun company-plsense--update ()
  "Update causes PlSense to reanalyze the given file."
  (company-plsense--get-function-scopes)
  (company-plsense--get-package-scopes)
  (setq company-plsense--changed-regions nil)
  (let ((file (buffer-file-name (current-buffer))))
    (if (-contains? company-plsense--opened-files file)
        (company-plsense--server-request
         (concat "update " file))
      (company-plsense--open-file file))))

;;;###autoload
(defun company-plsense-setup ()
  "Setup the default ‘company-plsense’ configuration.
This will start the server and enable command `company-mode'
with the appropriate major modes."
  (interactive)
  (--each company-plsense-enabled-modes
    (add-hook (intern-soft (concat (symbol-name it) "-hook")) 'company-mode))
  (add-to-list 'company-backends 'company-plsense))

(defun company-plsense--reset-location ()
  "Rest all location variables so that ‘company-plsense’ will resync with the server on the next query."
  (setq company-plsense--current-file nil)
  (setq company-plsense--current-package nil)
  (setq company-plsense--current-function nil))

(defun company-plsense-update-location ()
  "Set the current file, module, and function for the server."
  (interactive)
  (company-plsense--reset-location)
  (company-plsense--update-package-and-file)
  (company-plsense--update-function))

(defun company-plsense--update-package-and-file ()
  "Sync the current package and file with the PlSense server."
  (let ((file (buffer-file-name (current-buffer))))
    (unless (or (s-equals? company-plsense--current-file file)
                (s-equals? company-plsense--opening-file file))
      (if (-contains? company-plsense--opened-files file)
          (company-plsense--server-request (concat "onfile " file))
        (company-plsense--open-file file))
      (setq company-plsense--current-package "main")
      (setq company-plsense--current-function "")
      (setq company-plsense--opening-file file)))
  (let ((pkg (company-plsense--get-current-scope company-plsense--package-list)))
    (unless (s-equals? pkg company-plsense--current-package)
      (company-plsense--server-request (concat "onmod " pkg))
      (setq company-plsense--current-package pkg))))

(defun company-plsense--update-function ()
  "Sync the current function with the PlSense server."
  (let ((func (company-plsense--get-current-scope company-plsense--function-list)))
    (unless (s-equals? func company-plsense--current-function)
      (company-plsense--server-request (concat "onsub " func))
      (setq company-plsense--current-function func))))

(defun company-plsense--update-scopes (list change-start size)
  "Adjusts the boundaries for a LIST of scopes (packages or functions).
Move all points if they are after CHANGE-START by SIZE."
  (dolist (sub list)
    (let ((start (cadr sub))
          (end (cddr sub)))
      (when (> start change-start)
        (setf (cadr sub) (+ start size)))
      (when (>= end change-start)
        (setf (cddr sub) (+ end size))))))

(defun company-plsense--get-package-scopes ()
  "Generate `company-plsense--package-list' by analyzing the buffer."
  (save-excursion
    (save-restriction
      (widen)
      (setq company-plsense--package-list (list (cons "main" (cons (point-min) (point-max)))))
      (goto-char (point-min))
      (let (done-parsing-p)
        (while (and (not done-parsing-p)
                    (not (eobp)))
          (if (re-search-forward company-plsense--package-re nil t)
              (let ((start (match-beginning 1))
                    (pkg-name (match-string-no-properties 1)))
                (setf (cl-cddar company-plsense--package-list) start)
                (push (cons pkg-name (cons start (+ 1 (point-max)))) company-plsense--package-list))
            (setq done-parsing-p t)))))))

(defun company-plsense--get-function-scopes ()
  "Generate `company-plsense--function-list' by analyzing the buffer."
  (interactive)
  (setq company-plsense--function-list '())
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (done-parsing-p)
        (while (and (not done-parsing-p)
                    (not (eobp)))
          (if (beginning-of-defun -1)
              (let* ((start (point))
                     (sub-name (nth 1 (s-match company-plsense--sub-re
                                               (buffer-substring-no-properties
                                                start
                                                (line-end-position))))))
                (ignore-errors (end-of-defun))
                (backward-char)
                (beginning-of-line-text)
                (forward-char)
                (add-to-list 'company-plsense--function-list (cons sub-name (cons start (point)))))
            (setq done-parsing-p t)))))))

(defun company-plsense--get-current-scope (list &optional point)
  "Return the name of the scope LIST POINT is currently in."
  (unless point (setq point (point)))
  (--if-let
      (--first (and (>= point (cadr it))
                    (<= point (cddr it)))
               list)
      (car it)
    ""))

(defun company-plsense--add-code ()
  "Sends the lines of code found in `company-plsense--changed-regions' to resolver.
Any symbols found in the added lines will be available until buffer save."
  (dolist (range company-plsense--changed-regions)
    (let* ((start (cadr range))
           (end (cddr range))
           (new-code (save-restriction
                       (widen)
                       (buffer-substring (max start (point-min))
                                         (min end (point-max))))))
      (setq new-code (replace-regexp-in-string "#.*$" "" (s-trim new-code))) ;; remove comments
      (setq new-code (replace-regexp-in-string "\n" "" new-code)) ;; put all code one one line.
      (when (and (s-present? new-code)
                 (>= 1000 (length new-code)))
        (company-plsense--server-request
         (concat "codeadd " new-code)))))
  (setq company-plsense--changed-regions nil))

(defun company-plsense--create-code-range (beg end)
  "Create an object holding the position of the most code change.
Contains all lines from BEG to END."
  (cons "range"
        (save-excursion
          (cons (progn (goto-char beg)
                       (line-beginning-position))
                (progn (goto-char end)
                       (line-end-position))))))

(defun company-plsense--get-symbol ()
  "Get the symbol around point.
This is used to handle redundant edits like
regexp replace, iedit, or multiple cursors. We don't
want to send uncompleted symbols to the resolver."
  (cons
   (->> (point)
        (buffer-substring-no-properties (point-at-bol))
        (s-match (rx (0+ (any word "_")) eos))
        (car))
   (->> (point)
        (buffer-substring-no-properties (point-at-eol))
        (s-match (rx bos (0+ (any word "_"))))
        (car))))

(defun company-plsense--handle-change (beg end len)
  "Hanlder for `after-change-functions' which update all scopes.
BEG and END are locations of the change with LEN the size of the
previous TEXT."
  (let ((current-symbol (company-plsense--get-symbol)))
    (let ((size (- end beg len)))
      ;; update all scope lists
      (dolist (scope (list company-plsense--function-list
                           company-plsense--package-list
                           company-plsense--changed-regions))
        (company-plsense--update-scopes scope beg size))
      (unless company-plsense--changed-regions
        (setq company-plsense--changed-regions (list (company-plsense--create-code-range beg end)))))
    (if (s-present? (company-plsense--get-current-scope company-plsense--changed-regions end))
        (when (and (not (nth 3 (syntax-ppss))) ;; not inside string
                   (s-matches? "\n" (buffer-substring beg end)))
          (company-plsense--add-code)) ;; if a newline was added, send the code to the server
      (let ((prev company-plsense--prev-symbol)
            (curr current-symbol))
        (if (and (not (equal curr '("" . ""))) ;; no symbol at point
                 (s-equals? (car prev) (car curr))
                 (s-equals? (cdr prev) (cdr curr)))
            ;; handle multiple edits (regexp replace, iedit, multiple cursors etc.)
            (add-to-list 'company-plsense--changed-regions (company-plsense--create-code-range beg end))
          (company-plsense--add-code)
          (setq company-plsense--changed-regions (list (company-plsense--create-code-range beg end))))))
    (setq company-plsense--prev-symbol current-symbol)))


;;; Hanlders for company-plense backend

(defun company-plsense-init ()
  "Setup the current buffer for use by ‘company-plsense’."
  (interactive)
  (when (-contains? company-plsense-enabled-modes major-mode)
    (unless company-plsense--server-started-p
      (setq company-plsense--server-started-p t)
      (company-plsense-start-server))
    (company-plsense--get-function-scopes)
    (company-plsense--get-package-scopes)
    (company-plsense--open-file (buffer-file-name (current-buffer)))
    (setq company-plsense--active t)
    (add-hook 'company-mode-hook #'company-plsense--teardown nil t)
    (add-hook 'after-save-hook #'company-plsense--update nil t)
    (add-hook 'after-change-functions #'company-plsense--handle-change nil t)))

(defun company-plsense--teardown ()
  "Teardown the current buffer hooks.
This will automatically be called when `company-mode'
is disabled."
  (when (and (null company-mode)
             company-plsense--active)
    (setq company-plsense--active nil)
    (company-plsense--reset-location)
    (remove-hook 'company-mode-hook #'company-plsense--teardown t)
    (remove-hook 'after-save-hook #'company-plsense--update t)
    (remove-hook 'after-change-functions #'company-plsense--handle-change t)))

(defun company-plsense--prefix ()
  "Grab prefix at point.
incudes variable type identifiers like $ @ %."
  (when (-contains? company-plsense-enabled-modes major-mode)
    (company-plsense--update-package-and-file)
    (unless (nth 4 (syntax-ppss)) ;; don't complete in comments
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position)
                   (point))))
        (unless (and (nth 3 (syntax-ppss)) ;; don't complete non symbols in strings
                     (not (s-matches? "\\_<qw[{(]" line))
                     (s-matches? "\\(?:\\s-\\|'\\|\"\\|^\\)[[:alnum:]]+\\'" line))
          (--if-let (when (and (or (looking-back "[$@%&:[:alpha:]_{([]" (- (point) 1))
                                   (looking-back "\\(?:->\\|{^\\|$^\\)" (- (point) 2)))
                               (not (looking-at "[[:alnum:]_^]")))
                      (save-match-data
                        (if (string-match "\\(?:[$@%]{?^?\\|[[:alpha:]_]\\)[[:alnum:]_:]*\\'" line)
                            (match-string 0 line)
                          "")))
              (if (looking-back "[>&:{([].*" (- (point) company-minimum-prefix-length))
                  (cons it t)
                it)
            'stop))))))

(defun company-plsense--candidates (callback prefix)
  "Asyncrounously grab a list for completion candidates from the PlSense server.
Return CALLBACK containing the candidates based on PREFIX."
  (company-plsense--update-function)
  (let ((static-prefix (replace-regexp-in-string
                        "[^$@%]*\\'"
                        ""
                        (if (listp prefix)
                            (car prefix)
                          prefix)))
        (buffer (current-buffer))
        (line (buffer-substring-no-properties
               (line-beginning-position)
               (point))))
    (company-plsense--server-request
     (concat "assist " line)
     (lambda (resp)
       (with-current-buffer buffer
         (funcall
          callback
          (--map (concat static-prefix it)
                 (s-split "\n" resp t))))))))

(defun company-plsense--doc-buffer (candidate)
  "Return the perldoc info for the CANDIDATE."
  (company-doc-buffer
   (company-plsense--server-query
    (concat "assisthelp " (s-chop-prefixes '("$" "@" "%") candidate)))))

;;;###autoload
(defun company-plsense (command &optional arg &rest ignored)
  "Company backend for PlSense server."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-plsense))
    (init (company-plsense-init))
    (prefix (company-plsense--prefix))
    (candidates (cons :async
                      (lambda (callback)
                        (company-plsense--candidates callback arg))))
    (doc-buffer (company-plsense--doc-buffer arg))
    (post-completion (when (and company-plsense-braces-autopaired
                                (s-suffix? "}" arg)
                                (looking-at "}"))
                       (delete-char 1)))))


;;; modified transaction queue

;; the `tq' library will throw an error when input it recieved and there is not
;; outstanding server call. Plsense will raise errors asyncrounously and this will
;; cause the queue to fail. These overrided functions all us to handle spurious
;; input.

(defun company-plsense--tq-create (process)
  "Create and return a transaction queue communicating with PROCESS.
PROCESS should be a subprocess capable of sending and receiving
streams of bytes.  It may be a local process, or it may be connected
to a tcp server on another machine."
  (let ((tq (cons nil (cons process
                            (generate-new-buffer
                             (concat " tq-temp-"
                                     (process-name process)))))))
    (buffer-disable-undo (tq-buffer tq))
    (set-process-filter process
                        (lambda (_proc string) (company-plsense--tq-filter tq string)))
    tq))

(defun company-plsense--tq-filter (tq string)
  "Append STRING to the TQ's buffer; then process the new data."
  (let ((buffer (tq-buffer tq))
        (reply (company-plsense--handle-errors string)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert reply)
        (company-plsense--tq-process-buffer tq)))))

(defun company-plsense--tq-process-buffer (tq)
  "Check TQ's buffer for the regexp at the head of the queue."
  (let ((buffer (tq-buffer tq)))
    (when (buffer-live-p buffer)
      (set-buffer buffer)
      (when (/= 0 (buffer-size))
        (if (tq-queue-empty tq)
            (delete-region (point-min) (point-max))
          (goto-char (point-min))
          (if (re-search-forward (tq-queue-head-regexp tq) nil t)
              (let ((answer (buffer-substring (point-min) (point))))
                (delete-region (point-min) (point))
                (unwind-protect
                    (condition-case nil
                        (funcall (tq-queue-head-fn tq)
                                 (tq-queue-head-closure tq)
                                 answer)
                      (error nil))
                  (tq-queue-pop tq))
                (company-plsense--tq-process-buffer tq))))))))

(defun company-plsense--post-error (error)
  "Echo ERROR in message area and add it to error buffer."
  (setq company-plsense--last-error error)
  (message "company-plsense: server %s" error)
  (with-current-buffer (get-buffer-create "*company-plsense-errors*")
    (goto-char (point-max))
    (insert error)))

(defun company-plsense--handle-errors (msg)
  "Handle errors from the PlSense server in MSG."
  (let ((error-regexp (rx (or (and (or "FATAL" "ERROR") ":") ;; standard PlSense errors
                              (and bol (*? nonl) "Segmentation fault")) ;; analysis failure
                          (* nonl) "\n"))) ;; grab rest of line
    (save-match-data
      (let ((pos 0))
        (while (string-match error-regexp msg pos)
          (let ((error (match-string 0 msg)))
            (cond
             ((s-prefix? "ERROR: Not yet set current" error)
              (company-plsense-update-location))
             ((s-prefix? "ERROR: Not yet exist" error)
              (if (s-equals? company-plsense--current-file (nth 1 (s-match "\\[\\(.*\\)\\]$" error)))
                  (company-plsense--open-file company-plsense--current-file)
                (company-plsense-update-location)))
             ((s-prefix? "ERROR: Failed compile" error)
              (when (or (not company-plsense-ignore-compile-errors)
                        (-contains? company-plsense--opened-files (nth 1 (s-match "'\\(.*\\)'$" error))))
                (company-plsense--post-error error)))
             ((not (s-equals? company-plsense--last-error error))
              (company-plsense--post-error error))))
          (setq pos (match-end 0)))))
    (replace-regexp-in-string error-regexp "" msg)))

(provide 'company-plsense)

;;; company-plsense.el ends here
