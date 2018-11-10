;;; jsonnet-mode.el --- Major mode for editing jsonnet files

;; Copyright (C) 2017 Nick Lanham

;; Author: Nick Lanham
;; URL: https://github.com/mgyucht/jsonnet-mode
;; Package-Version: 20180822.1619
;; Package-X-Original-Version: 0.0.1
;; Keywords: languages
;; Package-Requires: ((emacs "24"))

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

;; This package provides syntax highlighting, indenting, formatting, and utility
;; methods for jsonnet files. To use it, place it somewhere in your load-path,
;; and then add the following to your init.el:
;; (load "jsonnet-mode")
;;
;; This mode creates the following keybindings:
;;   'C-c C-c' evaluates the current buffer in Jsonnet and put the output in an
;;             output buffer
;;   'C-c C-f' jumps to the definition of the identifier at point
;;   'C-c C-r' reformats the entire buffer using Jsonnet's fmt utility

;;; Code:

(defgroup jsonnet '()
  "Major mode for editing Jsonnet files."
  :group 'languages)

(defcustom jsonnet-command
  "jsonnet"
  "Jsonnet command to run in ‘jsonnet-eval-buffer’."
  :type '(string)
  :group 'jsonnet)

(defcustom jsonnet-library-search-directories
  nil
  "Sequence of Jsonnet library search directories, with later entries shadowing earlier entries."
  :type '(repeat directory)
  :group 'jsonnet)

(defcustom jsonnet-enable-debug-print
  nil
  "If non-nil, enables debug printing in ‘jsonnet-mode’ functions."
  :type '(boolean)
  :group 'jsonnet)

(defconst jsonnet--identifier-regexp
  "[a-zA-Z_][a-zA-Z0-9_]*"
  "Regular expression matching a Jsonnet identifier.")

(defconst jsonnet--function-name-regexp
  (concat "local \\(" jsonnet--identifier-regexp "\\)"
          "\\(([a-zA-Z0-9_, ]*)\s*=\\|\s*=\s*function\\)"))

(defconst jsonnet-font-lock-keywords-1
  (let ((builtin-regex (regexp-opt '("assert" "else" "error" "for" "function" "if" "import" "importstr" "in" "local" "self" "super" "then") 'words))
        (constant-regex (regexp-opt '("false" "null" "true") 'words))
        (function-name-regex jsonnet--function-name-regexp)
        ;; Any other local bindings are variables
        (variable-name-regex (concat "local \\(" jsonnet--identifier-regexp "\\)\s+="))
        ;; All standard library functions (see https://jsonnet.org/docs/stdlib.html)
        (standard-functions-regex (regexp-opt (mapcar (lambda (func-name) (concat "std." func-name))
                                                '("abs" "acos" "asin" "assertEqual" "atan" "base64" "base64Decode" "base64DecodeBytes" "ceil" "char" "codepoint" "cos" "count" "endsWith" "escapeStringBash" "escapeStringDollars" "escapeStringJson" "escapeStringPython" "exp" "exponent" "extVar" "filter" "filterMap" "flattenArrays" "floor" "foldl" "foldr" "format" "join" "length" "lines" "makeArray" "manifestIni" "manifestPython" "manifestPythonVars" "mantissa" "map" "max" "md5" "mergePatch" "min" "mod" "objectFields" "objectFieldsAll" "objectHas" "objectHasAll" "parseInt" "pow" "prune" "range" "set" "setDiff" "setInter" "setUnion" "sin" "sort" "split" "splitLimit" "sqrt" "startsWith" "stringChars" "substr" "substr" "tan" "thisFile" "toString" "type" "uniq")))))
    (list
     `(,builtin-regex . font-lock-builtin-face)
     `(,constant-regex . font-lock-constant-face)
     `(,function-name-regex . (1 font-lock-function-name-face))
     `(,variable-name-regex . (1 font-lock-variable-name-face))
     '("[[:space:]].+:" . font-lock-keyword-face)
     '("\\([[:digit:]]+\\(?:\\.[[:digit:]]+\\)?\\)" . font-lock-constant-face)
     `(,standard-functions-regex . font-lock-function-name-face)
     ))
  "Minimal highlighting for ‘jsonnet-mode’.")

(defvar jsonnet-font-lock-keywords jsonnet-font-lock-keywords-1
  "Default highlighting expressions for jsonnet mode.")

;; Syntax table
(defconst jsonnet-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments. Jsonnet supports /* */ and // as comment delimiters
    (modify-syntax-entry ?/ ". 124" table)
    (modify-syntax-entry ?* ". 23b" table)
    ;; Additionally, Jsonnet supports # as a comment delimiter
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; ", ', and ||| are quotations in Jsonnet.
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?| "\"" table) ; This incidentally also causes triple-quoted strings to be
                                        ; correctly highlighted.
    ;; Our parenthesis, braces and brackets
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Syntax table for `jsonnet-mode'.")

;; Indent rules
(defun jsonnet--debug-print (str)
  "Print out STR if ‘jsonnet-enable-debug-print’ is non-nil."
  (when jsonnet-enable-debug-print
    (message str)))

(defun jsonnet--find-current-block-comment ()
  "Return the position of the comment start if inside a block comment. Otherwise, return nil."
  (let* ((previous-comment-start (save-excursion (re-search-backward "\\/\\*" nil t)))
         (previous-comment-end (save-excursion (re-search-backward "\\*\\/" nil t)))
         (is-in-block-comment (and (integerp previous-comment-start)
                                   (or (not (integerp previous-comment-end))
                                       (> previous-comment-start previous-comment-end)))))
    (when is-in-block-comment previous-comment-start)))

(defun jsonnet--find-current-multiline-string ()
  "Return the position of the beginning of the current multiline string.

If not inside of a multiline string, return nil."
  (save-excursion
    (beginning-of-line)
    (let ((num-triple-pipe 0)
          closest-triple-pipe)
      (while (search-backward "|||" 0 t)
        (setq num-triple-pipe (+ num-triple-pipe 1))
        (when (not closest-triple-pipe)
          (setq closest-triple-pipe (point))))
      (when (eq 1 (% num-triple-pipe 2))
        closest-triple-pipe))))

(defun jsonnet--line-matches-regex-p (regex)
  "Return t if the current line matches REGEX."
  (save-excursion
    (beginning-of-line)
    (integerp (re-search-forward regex (line-beginning-position 2) t))))

;; Experimental algorithm
(defun jsonnet--indent-in-parens ()
  "Compute the indent of the current line, given it is inside parentheses."
  (if (jsonnet--line-matches-regex-p "^\s*)") 0 4))

(defun jsonnet--indent-in-braces ()
  "Compute the indent of the current line, given it is inside braces."
  (cond
   ((jsonnet--line-matches-regex-p "^\s*}") 0)
   ((save-excursion
      (forward-line -1)
      (jsonnet--line-matches-regex-p ":\s*$")) 4)
   (t 2)))

(defun jsonnet--indent-in-brackets ()
  "Compute the indent of the current line, given it is inside braces."
  (if (jsonnet--line-matches-regex-p "^\s*]") 0 2))

(defun jsonnet--indent-toplevel ()
  "Compute the indent of the current line, given it is not inside any delimiter."
  0)

(defun jsonnet-calculate-indent ()
  "Compute the indent of the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond
     ;; At the beginning of the buffer, the indent should be 0.
     ((bobp) 0)
     ;; NOTE: In all of these examples, the 'o' indicates the location of point after
     ;; indenting on that line. If the indent of the line depends on the contents of the line
     ;; itself, a '^' is used to indicate the proper indentation for the last line.
     ;;
     ;; If we are in a block comment, the indent should match the * at the beginning of the
     ;; comment.
     ;; e.g.
     ;; |/*
     ;; | o
     ((jsonnet--find-current-block-comment)
      (goto-char (jsonnet--find-current-block-comment))
      (+ 1 (current-column)))
     ;; If we are inside of a multiline string, the indent should be 2 greater than the beginning
     ;; of the multiline string. However, if the current line ends a multiline string, then the
     ;; indent should match the beginning of the multiline string.
     ((jsonnet--find-current-multiline-string)
      (let ((multiline-string-ends (jsonnet--line-matches-regex-p "^\s*|||")))
        (goto-char (jsonnet--find-current-multiline-string))
        (+ (current-indentation) (if multiline-string-ends 0 2))))
     ;; Otherwise, indent according to the kind of delimiter we are nested in.
     (t
      (let ((state (syntax-ppss)))
        (if (not (eq 0 (car state)))
            (let* ((delimiter-pos (cadr state))
                   (delimiter (when (not (eq 0 (car state)))
                                (char-after delimiter-pos)))
                   (current-indent (save-excursion
                                     (goto-char (cadr state))
                                     (current-indentation)))
                   (additional-indent (pcase delimiter
                                        (`?\( (jsonnet--indent-in-parens))
                                        (`?\{ (jsonnet--indent-in-braces))
                                        (`?\[ (jsonnet--indent-in-brackets))
                                        (_    (error (format "Unrecognized delimiter: %s" delimiter)))))
                   (new-indent (+ current-indent additional-indent)))
              (jsonnet--debug-print (format "Current delimiter: %s, position: %d" delimiter delimiter-pos))
              new-indent)
          (jsonnet--indent-toplevel)))))))

(defun jsonnet-indent ()
  "Indent current line according to Jsonnet syntax."
  (interactive)
  (let ((calculated-indent (jsonnet-calculate-indent)))
    (when (not (eq calculated-indent (current-indentation)))
      (beginning-of-line)
      (delete-char (current-indentation))
      (indent-to calculated-indent))))

;;;###autoload
(define-derived-mode jsonnet-mode prog-mode "Jsonnet"
  "jsonnet-mode is a major mode for editing .jsonnet files."
  :syntax-table jsonnet-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults) '(jsonnet-font-lock-keywords ;; keywords
                                                   nil                        ;; keywords-only
                                                   nil                        ;; case-fold
                                                   nil                        ;; syntax-alist
                                                   nil                        ;; syntax-begin
                                                   ))
  (set (make-local-variable 'indent-line-function) 'jsonnet-indent))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.jsonnet\\'" 'jsonnet-mode))
(add-to-list 'auto-mode-alist (cons "\\.libsonnet\\'" 'jsonnet-mode))

;; Utilities for evaluating and jumping around Jsonnet code.
;;;###autoload
(defun jsonnet-eval-buffer ()
  "Run jsonnet with the path of the current file."
  (interactive)
  (let ((file-to-eval (buffer-file-name))
        (search-dirs jsonnet-library-search-directories))
    (when (buffer-modified-p)
      (when (y-or-n-p
             (format "Save file %s? " file-to-eval))
        (save-buffer)))
    (with-current-buffer (get-buffer-create "*jsonnet output*")
      (erase-buffer)
      (let ((args (nconc (cl-loop for dir in search-dirs
                                  collect "-J"
                                  collect dir)
                         (list file-to-eval))))
        (apply #'call-process jsonnet-command nil t nil args))
      (when (fboundp 'json-mode)
        (json-mode))
      (display-buffer (current-buffer)
                      '((display-buffer-pop-up-window
                         display-buffer-reuse-window
                         display-buffer-at-bottom
                         display-buffer-pop-up-frame))))))

(define-key jsonnet-mode-map (kbd "C-c C-c") 'jsonnet-eval-buffer)

;;;###autoload
(defun jsonnet-jump-to-definition (identifier)
  "Jump to the definition of the jsonnet function IDENTIFIER."
  (interactive "sFind definition with name: ")
  (let* ((local-def (concat "local\s+" identifier "[^[:alnum:]_]"))
         (inner-def (concat identifier "\\:+"))
         (full-regex (concat "\\(" local-def "\\|" inner-def "\\)"))
         (identifier-def (save-excursion
                           (goto-char (point-max))
                           (re-search-backward full-regex nil t))))
    (if identifier-def
        (goto-char identifier-def)
      (message (concat "Unable to find definition for " identifier ".")))))

(defun jsonnet--get-identifier-at-location (&optional location)
  "Return the identifier at LOCATION if over a Jsonnet identifier.
If not provided, current point is used."
  (save-excursion
    (when location
      (goto-char location))
    (let ((curr-point (point))
          (curr-char (char-after)))
      (when (or (eq ?_ curr-char)
                (<= ?a curr-char ?z)
                (<= ?A curr-char ?Z)
                (<= ?0 curr-char ?9))
        (let ((start (save-excursion
                       (skip-chars-backward "[:alnum:]_")
                       (skip-chars-forward "[:digit:]")
                       (point)))
              (end   (save-excursion
                       (skip-chars-forward "[:alnum:]_")
                       (point))))
          (when (<= start curr-point end)
            (buffer-substring start end)))))))

;;;###autoload
(defun jsonnet-jump (point)
  "Jumps to the definition of the Jsonnet expression at POINT."
  (interactive "d")
  (let ((current-identifier (jsonnet--get-identifier-at-location)))
    (if (not current-identifier)
        (message "Point is not over a valid Jsonnet identifier.")
      (jsonnet-jump-to-definition current-identifier))))

(define-key jsonnet-mode-map (kbd "C-c C-f") 'jsonnet-jump)

;;;###autoload
(defun jsonnet-reformat-buffer ()
  "Reformat entire buffer using the Jsonnet format utility."
  (interactive)
  (call-process-region (point-min) (point-max) jsonnet-command t t nil "fmt" "-"))

(define-key jsonnet-mode-map (kbd "C-c C-r") 'jsonnet-reformat-buffer)

(provide 'jsonnet-mode)
;;; jsonnet-mode.el ends here
