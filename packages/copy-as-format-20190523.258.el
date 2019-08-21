;;; copy-as-format.el --- Copy buffer locations as GitHub/Slack/JIRA etc... formatted code -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017 Skye Shaw
;; Author: Skye Shaw <skye.shaw@gmail.com>
;; Package-Version: 20190523.258
;; Package-X-Original-Version: 0.0.8
;; Keywords: github, slack, jira, hipchat, gitlab, bitbucket, org-mode, pod, rst, asciidoc, tools, convenience
;; URL: https://github.com/sshaw/copy-as-format
;; Package-Requires: ((cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;;; License:

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

;; Copy buffer locations as GitHub/Slack/JIRA/HipChat/... formatted code and add them
;; to the kill ring.  The buffer will not be modified.
;;
;; With a prefix argument prompt for the format.  Defaults to `copy-as-format-default'.
;;
;; To add formats see `copy-as-format-format-alist'.
;;
;; For AsciiDoc customization see `copy-as-format-asciidoc-include-file-name' and
;; `copy-as-format-asciidoc-language-alist'.

;;; Change Log:

;; 2019-01-19 - v0.0.8
;; * Add more languages to Jira (thanks Peter Oliver)
;;
;; 2017-12-15 - v0.0.7
;; * Add support for AsciiDoc
;; * Remove use of string-empty-p to support pre 24.4 versions of Emacs
;;
;; 2017-06-03 - v0.0.6
;; * Fix Disqus: don't include a code tag unless we have a mode
;;
;; 2017-03-10 - v0.0.5
;; * Add support for POD
;; * Add support for reStructuredText
;; * Fix MediaWiki function autoload
;;
;; 2017-03-03 - v0.0.4
;; * Add support for MediaWiki
;; * Fix for language when the file has no extension
;; * Fix v0.0.3's release date in Change Log
;;
;; 2017-02-07 - v0.0.3
;; * Add support for Org-mode
;;
;; 2016-12-31 - v0.0.2
;; * Remove leading whitespace when copying regions
;; * Remove leading whitespace when copying JIRA single lines
;; * Use buffer-substring-no-properties instead of buffer-substring

;;; Code:

(require 'cl-lib)
(require 'tabify)
(require 'xml)

(defgroup copy-as-format nil
  "Copy buffer locations as GitHub/Slack/JIRA etc... formatted code"
  :prefix "copy-as-format-"
  :link '(url-link :tag "Report a Bug" "https://github.com/sshaw/copy-as-format/issues")
  :link '(url-link :tag "Homepage" "https://github.com/sshaw/copy-as-format")
  :group 'convenience)

(defcustom copy-as-format-default "markdown"
  "Name of the default formatter, defaults to `markdown'."
  :type 'string
  :group 'copy-as-format)

(defcustom copy-as-format-format-alist
  '(("asciidoc"  copy-as-format--asciidoc)
    ("bitbucket" copy-as-format--github)
    ("disqus"    copy-as-format--disqus)
    ("github"    copy-as-format--github)
    ("gitlab"    copy-as-format--github)
    ("hipchat"   copy-as-format--hipchat)
    ("html"      copy-as-format--html)
    ("jira"      copy-as-format--jira)
    ("markdown"  copy-as-format--markdown)
    ("mediawiki" copy-as-format--mediawiki)
    ("org-mode"  copy-as-format--org-mode)
    ("pod"       copy-as-format--pod)
    ("rst"       copy-as-format--rst)
    ("slack"     copy-as-format--slack))
  "Alist of format names and the function to do the formatting."
  :type '(alist :key-type string :value-type (group function))
  :group 'copy-as-format)

(defcustom copy-as-format-asciidoc-include-file-name nil
  "If non-nil include the buffer's file name."
  :type 'boolean
  :group 'copy-as-format)

(defcustom copy-as-format-asciidoc-language-alist nil
  "Alist of file name patterns to language names used for syntax highlighting.
By default the buffer's file extension is used.  If this does not
work with your processor add the appropriate mapping here."
  :type '(alist :key-type string :value-type string)
  :group 'copy-as-format)

(defconst copy-as-format--jira-supported-languages
  '(("as"  "actionscript")
    ("adb" "ada")
    ("ads" "ada")
    ("cs"  "c#")
    ("erl" "erlang")
    ("hs"  "haskel")
    ("htm" "html")
    ("mm"  "objc")
    ("pl"  "perl")
    ("pm"  "perl")
    ("py"  "python")
    ("rb"  "ruby")
    ("ksh" "sh")
    ("vb"  "visualbasic")
    ("yml" "yaml"))
  "Alist mapping file extensions to Jira language names.
Jira's {code} markup blocks are documented at URL `https://jira.atlassian.com/secure/WikiRendererHelpAction.jspa?section=advanced',
but you can get a complete list from Jira itself by trying to use
a language unknown to it.")

(dolist (lang '("applescript" "bash" "c" "cpp" "css" "go" "groovy" "html" "java" "js" "json" "lua" "php" "r" "scala" "sh" "sql" "swift" "xml" "yaml"))
  (add-to-list 'copy-as-format--jira-supported-languages (list lang lang)))


(defun copy-as-format--extract-text ()
  (if (not (use-region-p))
      (buffer-substring-no-properties (line-beginning-position) (line-end-position))
    ;; Avoid adding an extra blank line to the selection. This happens when point or mark
    ;; is at the start of the next line.
    ;;
    ;; When selection is from bottom to top, exchange point and mark
    ;; so that the `point' and `(region-end)' are the same.
    (when (< (point) (mark))
      (exchange-point-and-mark))

    (let (n min text (end (region-end)))
      (when (= end (line-beginning-position))
	(setq end (1- end)))

      ;; Let's trim unnecessary leading space from the region
      (setq text (buffer-substring-no-properties (region-beginning) end))
      (with-temp-buffer
	(insert text)
	(goto-char (point-min))
	;; The length of the match (see below) determines how much leading space to trim
	;; Without this only one space would be trimmed for each tab
	(untabify (point-min) (point-max))
	(while (search-forward-regexp "^\\([[:space:]]*\\)[^[:space:]]" nil t)
	  (setq n (length (match-string 1)))
	  (when (or (null min) (< n min))
	    (setq min n)))

	(when (and (not (null min)) (> min 0))
	  (indent-rigidly 1 (point-max) (- min)))
	(buffer-string)))))

(defun copy-as-format--asciidoc (text multiline)
  (if multiline
      (let* ((file (and (buffer-file-name) (file-name-nondirectory (buffer-file-name))))
             (lang (or (and file (cadr (cl-find file
                                                copy-as-format-asciidoc-language-alist
                                                :test '(lambda (name pair) (string-match-p (car pair) name)))))
                       (copy-as-format--language))))
        (setq text (format "%s----\n%s\n----\n"
                           (if (> (length lang) 0)
                               (format "[source,%s]\n" lang)
                             "")
                           text))
        (if (and copy-as-format-asciidoc-include-file-name file)
            (format ".%s\n%s" file text)
          text))
    (copy-as-format--slack text nil)))

(defun copy-as-format--disqus (text _multiline)
  (let ((lang (copy-as-format--language))
        (text (xml-escape-string text)))
   (when (> (length lang) 0)
     (setq text (format "<code class='%s'>\n%s\n</code>" lang text)))

   (format "<pre>%s</pre>\n" text)))

(defun copy-as-format--github (text multiline)
  (if multiline
      (format "```%s\n%s\n```\n" (copy-as-format--language) text)
    (copy-as-format--inline-markdown text)))

(defun copy-as-format--hipchat (text _multiline)
  ;; If I recall HipChat treats multiline and single line the same
  ;; TODO: does leading whitspace need to be trimmed?
  (format "/code %s" text))

(defun copy-as-format--html (text multiline)
  (setq text (xml-escape-string text))
  (if multiline
      (format "<pre><code>\n%s\n</code></pre>\n" text)
    (format "<code>%s</code>" text)))

(defun copy-as-format--jira (text multiline)
  (if multiline
      (let ((lang (car (cdr (assoc (copy-as-format--language)
			           copy-as-format--jira-supported-languages)))))
	(format "{code:%s}\n%s\n{code}\n" (or lang "none") text))
    (format "{{%s}}" (copy-as-format--trim text))))

(defun copy-as-format--markdown (text multiline)
  (if multiline
      (with-temp-buffer
        (insert text)
        (indent-rigidly 1 (point-max) 4)
        (buffer-string))
    (copy-as-format--inline-markdown text)))

(defun copy-as-format--mediawiki (text multiline)
  (format "<syntaxhighlight lang='%s'%s>\n%s\n</syntaxhighlight>"
          (copy-as-format--language)
          (if (not multiline) " inline" "")
          text))

(defun copy-as-format--org-mode (text _multiline)
  (format "#+BEGIN_SRC %s\n%s\n#+END_SRC\n"
          (replace-regexp-in-string "-mode\\'" "" (symbol-name major-mode))
          text))

(defun copy-as-format--pod (text multiline)
  (if multiline
      (with-temp-buffer
        (insert text)
        (indent-rigidly 1 (point-max) 2)
        (buffer-string))
    (format "C<< %s >>" text)))

(defun copy-as-format--rst (text multiline)
  (if multiline
      ;; Not sure what highlighting directives are supported and where, so leave it blank
      (format ".. code::\n\n%s\n"
              (with-temp-buffer
                (insert text)
                (indent-rigidly 1 (point-max) 4)
                (buffer-string)))
    (format "``%s``" (copy-as-format--trim text))))

(defun copy-as-format--slack (text multiline)
  (if multiline
      (format "```\n%s\n```\n" text)
    (copy-as-format--inline-markdown
     ;; Slack preserves leading and trailing whitespace
     (copy-as-format--trim text))))

(defun copy-as-format--inline-markdown (text)
  (format "`%s`" text))

(defun copy-as-format--language ()
  (or (and (buffer-file-name)
           (file-name-extension (downcase (buffer-file-name))))
      ""))

(defun copy-as-format--trim (s)
  (replace-regexp-in-string "^[[:space:]]+\\|[[:space:]]+$" "" s))


;;;###autoload
(defun copy-as-format ()
  "Copy the current line or active region and add it to the kill ring as
GitHub/Slack/JIRA/HipChat/... formatted code.  Format defaults to
`copy-as-format-default'.  The buffer will not be modified.

With a prefix argument prompt for the format."
  (interactive)
  (let* ((text (copy-as-format--extract-text))
         (format (if current-prefix-arg
                     (completing-read "Format: "
                                      (mapcar 'car copy-as-format-format-alist)
                                      nil
                                      t
                                      ""
                                      nil
                                      copy-as-format-default)
                   copy-as-format-default))
         (func (cadr (assoc format copy-as-format-format-alist))))

    (when (eq (length text) 0)
      (error "No text selected"))

    (when (not (fboundp func))
      (error "Missing or invalid format function for `%s'" format))

    (kill-new (funcall
               func
               text
               (use-region-p)))

    (setq deactivate-mark t)))

;; Generate format specific functions
(cl-loop for (name) in copy-as-format-format-alist
         do (fset (intern (concat "copy-as-format-" name))
                  `(lambda ()
                     (interactive)
                     (setq copy-as-format-default ,name)
                     (copy-as-format))))

;;;###autoload (autoload 'copy-as-format-asciidoc  "copy-as-format" nil t)
;;;###autoload (autoload 'copy-as-format-bitbucket "copy-as-format" nil t)
;;;###autoload (autoload 'copy-as-format-disqus    "copy-as-format" nil t)
;;;###autoload (autoload 'copy-as-format-github    "copy-as-format" nil t)
;;;###autoload (autoload 'copy-as-format-gitlab    "copy-as-format" nil t)
;;;###autoload (autoload 'copy-as-format-hipchat   "copy-as-format" nil t)
;;;###autoload (autoload 'copy-as-format-html      "copy-as-format" nil t)
;;;###autoload (autoload 'copy-as-format-jira      "copy-as-format" nil t)
;;;###autoload (autoload 'copy-as-format-markdown  "copy-as-format" nil t)
;;;###autoload (autoload 'copy-as-format-mediawiki "copy-as-format" nil t)
;;;###autoload (autoload 'copy-as-format-org-mode  "copy-as-format" nil t)
;;;###autoload (autoload 'copy-as-format-pod       "copy-as-format" nil t)
;;;###autoload (autoload 'copy-as-format-rst       "copy-as-format" nil t)
;;;###autoload (autoload 'copy-as-format-slack     "copy-as-format" nil t)

(provide 'copy-as-format)
;;; copy-as-format.el ends here
