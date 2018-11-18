;;; wolfram-mode.el --- Mathematica editing and inferior mode.  -*- lexical-binding: t -*-

;; Filename: wolfram-mode.el
;; Description: Wolfram Language (Mathematica) editing and inferior Mode
;; Package-Requires: ((emacs "24.3"))
;; Package-Version: 20180307.13
;; Author: Daichi Mochihashi <daichi at cslab.kecl.ntt.co.jp>
;; Modified by: Taichi Kawabata <kawabata.taichi_at_gmail.com>
;; Modified by: Tomas Skrivan <skrivantomas_at_seznam.cz.cz>
;; Modified by: Ken Kang <kenkangxgwe_at_gmail.com>
;; Created: 2009-07-08
;; Modified: 2017-02-16
;; Keywords: languages, processes, tools
;; Namespace: wolfram-
;; URL: https://github.com/kawabata/wolfram-mode/

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Commentary:

;; This provides basic editing features for Wolfram Language
;; (http://reference.wolfram.com/language/), based on `math++.el'
;; (http://chasen.org/~daiti-m/dist/math++.el).

;; You should add the followings to `~/.emacs.d/init.el'.

;;  (autoload 'wolfram-mode "wolfram-mode" nil t)
;;  (autoload 'run-wolfram "wolfram-mode" nil t)
;;  (setq wolfram-program "/Applications/Mathematica.app/Contents/MacOS/MathKernel")
;;  (add-to-list 'auto-mode-alist '("\\.m$" . wolfram-mode))
;;  (setq wolfram-path "directory-in-Mathematica-$Path") ;; e.g. on Linux ~/.Mathematica/Applications

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mathematica is (C) Copyright 1988-2013 Wolfram Research, Inc.
;;
;; Protected by copyright law and international treaties.
;;
;; Unauthorized reproduction or distribution subject to severe civil
;; and criminal penalties.
;;
;; Mathematica is a registered trademark of Wolfram Research.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO
;; - wolfram-imenu-generic-expression
;; - sending useful commands to comint buffer.
;; - support for parsing string
;; - support for top level "\n" parsing

;;; Change Log:

;; 2013-10-01  Kawabata Taichi <kawabata.taichi_at_gmail.com>
;;         * Modified to work with Emacs 24.3.
;;         * Remove duplicate functions, undefined function calls and compiler warnings.
;; 2013-10-07
;;         * Change `math-process-string' to `math-program'
;;         * Change `scheme-args-to-list' to `split-string-and-unquote'
;; 2013-10-12
;;         * Change `setq/make-local-variable' to `setq-local'
;;         * Change `(* ... *)' comment to be nestable.
;;         * `math-outline-regexp' : New variable.
;;         * `math-electric' : New function.
;;         * Change syntax of "`" to be word-constituent.
;;         * Change indentation routines to use SMIE.
;; 2013-10-13
;;         * `math-program-arguments' : New variable
;;         * `run-math' : Cleanup
;; 2013-11-23
;;         * Change `math-' prefix to `wolfram-' prefix.

;; * Code:

(require 'comint)
(require 'smie)

;; ** Customs Variables

(defgroup wolfram-mode nil
  "Editing Wolfram Language code"
  :group 'languages)

(defcustom wolfram-mode-hook nil
  "Normal hook run when entering `wolfram-mode'.
See `run-hooks'."
  :type 'hook
  :group 'wolfram-mode)

(defcustom wolfram-program "math"
  "Command to invoke at `run-wolfram'."
  :type 'string
  :group 'wolfram-mode)

(defcustom wolfram-program-arguments '()
  "Additional arguments to `wolfram-program'."
  :type '(repeat string)
  :group 'wolfram-mode)

(defcustom wolfram-indent 8
  "Basic Indentation for newline."
  :type 'integer
  :group 'wolfram-mode)

(defcustom wolfram-path nil
  "Directory in Mathematica $Path. Emacs has to be able to write in this directory."
  :type 'string
  :group 'wolfram-mode)

;; ** wolfram-mode

(defvar wolfram-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'newline-and-indent)
    (define-key map "]" 'wolfram-electric-braket)
    (define-key map ")" 'wolfram-electric-paren)
    (define-key map "}" 'wolfram-electric-brace)
    (define-key map "\C-c\C-r" 'wolfram-send-region)
    (define-key map "\C-c\C-e" 'wolfram-send-last-mathexp)
    (define-key map "\C-c\C-s" 'wolfram-send-last-mathexp)
    map)
  "Keymap for `wolfram-mode'.")

(defvar wolfram-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; white space
    (modify-syntax-entry ?  " " syntax-table)
    (modify-syntax-entry ?\t " " syntax-table)
    (modify-syntax-entry ?\f " " syntax-table)
    (modify-syntax-entry ?\n " " syntax-table)
    (modify-syntax-entry ?\^m " " syntax-table)

    ;; comments and parens
    (modify-syntax-entry ?( "()1n" syntax-table)
    (modify-syntax-entry ?) ")(4n" syntax-table)
    (modify-syntax-entry ?* "_ 23n" syntax-table)

    ;; pure parens
    (modify-syntax-entry ?[ "(]" syntax-table)
    (modify-syntax-entry ?] ")[" syntax-table)
    (modify-syntax-entry ?{ "(}" syntax-table)
    (modify-syntax-entry ?} "){" syntax-table)

    ;; punctuation
    (modify-syntax-entry ?= "." syntax-table)
    (modify-syntax-entry ?: "." syntax-table)
    (modify-syntax-entry ?% "." syntax-table)
    (modify-syntax-entry ?< "." syntax-table)
    (modify-syntax-entry ?> "." syntax-table)
    (modify-syntax-entry ?& "." syntax-table)
    (modify-syntax-entry ?| "." syntax-table)
    (modify-syntax-entry ?_ "." syntax-table)
    (modify-syntax-entry ?/ "." syntax-table)
    (modify-syntax-entry ?! "." syntax-table)
    (modify-syntax-entry ?@ "." syntax-table)
    (modify-syntax-entry ?# "." syntax-table)
    (modify-syntax-entry ?\' "." syntax-table)

    ;; quotes
    (modify-syntax-entry ?\\ "\\" syntax-table)
    (modify-syntax-entry ?\" "\"" syntax-table)

    ;; for Math numbers, the following would be better as
    ;; parts of symbols
    (modify-syntax-entry ?- "_" syntax-table)
    (modify-syntax-entry ?. "_" syntax-table)
    (modify-syntax-entry ?\` "_" syntax-table)
    (modify-syntax-entry ?^ "_" syntax-table)

    (modify-syntax-entry ?$ "_" syntax-table)
    (modify-syntax-entry ?+ "_" syntax-table)

    syntax-table)
  "Syntax table used in `wolfram-mode'.")

(define-abbrev-table 'wolfram-mode-abbrev-table ())

(defvar wolfram-syntax-propertize-function
  (syntax-propertize-rules
   ("\\\\[[A-Z][A-Za-z]*]" (0 "_"))))

(defvar wolfram-font-lock-keywords
  '(
    ("^In\[[0-9]+\]:=" . font-lock-keyword-face)
    ("^Out\[[0-9]+\]=" . font-lock-keyword-face)
    ("^Out\[[0-9]+\]//[A-Za-z][A-Za-z0-9]*=" . font-lock-keyword-face)
    ("\\([A-Za-z][A-Za-z0-9`]*\\)[ \t]*[\[][ \t]*[\[]" 1 "default")
    ("\\([A-Za-z][A-Za-z0-9`]*\\)[ \t]*[\[]" 1 font-lock-function-name-face)
    ("//[ \t\f\n]*\\([A-Za-z][A-Za-z0-9`]*\\)" 1 font-lock-function-name-face)
    ("\\([A-Za-z][A-Za-z0-9`]*\\)[ \t\f\n]*/@" 1 font-lock-function-name-face)
    ("\\([A-Za-z][A-Za-z0-9`]*\\)[ \t\f\n]*//@" 1 font-lock-function-name-face)
    ("\\([A-Za-z][A-Za-z0-9`]*\\)[ \t\f\n]*@@" 1 font-lock-function-name-face)
    ("~[ \t]*\\([A-Za-z][A-Za-z0-9`]*\\)[ \t]*~" 1 font-lock-function-name-face)
    ("_[) \t]*\\?\\([A-Za-z][A-Za-z0-9`]*\\)" 1 font-lock-function-name-face)
    ("\\(&&\\)" 1 "default")
    ("&" . font-lock-function-name-face)
    ("\\\\[[A-Za-z][A-Za-z0-9]*\]" . font-lock-constant-face )
    ("$[A-Za-z0-9]+" . font-lock-variable-name-face )
    ("\\([A-Za-z0-9]+\\)[ \t]*\\->" 1 font-lock-type-face )
    ("<<[ \t\f\n]*[A-Za-z][A-Za-z0-9]*`[ \t\f\n]*[A-Za-z][A-Za-z0-9]*[ \t\f\n]*`"
     . font-lock-type-face )
    ("[A-Za-z][A-Za-z0-9]*::[A-Za-z][A-Za-z0-9]*" . font-lock-warning-face)
    ("\\[Calculating\\.\\.\\.\\]" . font-lock-warning-face)
    ("\\[Mathematica.*\\]" . font-lock-warning-face)
    ("^Interrupt>" . font-lock-warning-face)
    ("-Graphics-" . font-lock-type-face)
    ("-DensityGraphics-" . font-lock-type-face)
    ("-ContourGraphics-" . font-lock-type-face)
    ("-SurfaceGraphics-" . font-lock-type-face)
    ("-Graphics3D-" . font-lock-type-face)
    ("-GraphicsArray-" . font-lock-type-face)
    ("-Sound-" . font-lock-type-face)
    ("-CompiledCode-" . font-lock-type-face)))

(defvar wolfram-outline-regexp "\\((\\*\\|.+?:=\\)")

(defvar wolfram-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    `((head) (epsilon) (string)
      (expr (head "[" exprs "]")
            (expr "[[" exprs "]]")
            ("{" exprs "}")
            ("(" expr ")")
	    ("<|" exprs "|>")
            ;; message
            (expr "::" string)
            ;; statement separation
            (expr ";" expr)
            (expr "&")
            ;; delayed set
            (expr ":=" expr)
            (head "/:" expr ":=" expr)
            ;; set
            (expr "=" expr)
            (head "/:" expr "=" expr)
            (expr "+=" expr)
            (expr "-=" expr)
            (expr "*=" expr)
            (expr "/=" expr)
            ;; operation
            (expr "~" head "~" expr)
            (expr "@@" expr)
            (expr "==" expr)
            (expr "||" expr)
            (expr "&&" expr)
            (expr "+" expr)
            (expr "-" expr)
            (expr "*" expr)
            (expr "/" expr)
            (expr "^" expr)
            ;; application
            (expr ":" expr)
            (expr "/;" expr)
            (expr "//" expr))
      (exprs (epsilon)
             (expr)
             (exprs "," expr)))
    '((assoc ";")
      (assoc "::")
      (assoc "&")
      (assoc "/:")
      (assoc ":=" "=" "+=" "-=" "*=" "/=")
      (assoc "/;" ":" "//")
      (assoc "~")
      (assoc "@@" "==")
      (assoc "||" "&&")
      (assoc "+" "-")
      (assoc "*" "/")
      (assoc "^")
      (assoc "[[")))))

(defun wolfram-smie-rules (kind token)
  "Wolfram Language SMIE indentation function for KIND and TOKEN."
  (pcase (cons kind token)
    (`(:before . "[")
     (save-excursion
       (smie-default-backward-token)
       `(column . ,(current-column))))
    (`(:after . ":=") `(column . ,wolfram-indent))
    (`(:after . ,(or "]" "}" ")" "|>")) '(column . 0))
    (`(:after . ,(or "[" "{" "(" "<|"))
     (save-excursion
       (beginning-of-line)
       (skip-chars-forward " \t")
       `(column . ,(+ wolfram-indent (current-column)))))
    (`(,_ . ";") (smie-rule-separator kind))
    (`(,_ . ",") (smie-rule-separator kind))
    (`(:elem . ,_) 0)
    (t nil)))

(defalias 'wolfram-smie-forward-token 'smie-default-forward-token)
(defalias 'wolfram-smie-backward-token 'smie-default-backward-token)

;;;###autoload
(define-derived-mode wolfram-mode prog-mode "Mathematica"
  "Major mode for editing Mathematica text files in Emacs.

\\{wolfram-mode-map}
Entry to this mode calls the value of `wolfram-mode-hook'
if that value is non-nil."
  :syntax-table wolfram-mode-syntax-table
  :abbrev-table wolfram-mode-abbrev-table
  (smie-setup wolfram-smie-grammar #'wolfram-smie-rules
              :forward-token 'wolfram-smie-forward-token
              :backward-token 'wolfram-smie-backward-token)
  (wolfram-mode-variables))

(defun wolfram-mode-variables ()
  "Local variables for both Major and Inferior mode."
  (set-syntax-table wolfram-mode-syntax-table)
  ;; set local variables
  (setq-local comment-start "(*")
  (setq-local comment-end "*)")
  (setq-local comment-start-skip "(\\*")
  (set (make-local-variable 'syntax-propertize-function)
       wolfram-syntax-propertize-function)
  (setq-local font-lock-defaults '(wolfram-font-lock-keywords nil nil))
  (setq-local outline-regexp wolfram-outline-regexp))

(defun wolfram-electric (char arg)
  "Indent on closing a CHAR ARG times."
  (if (not arg) (setq arg 1) nil)
  (dotimes (_i arg) (insert char))
  (funcall indent-line-function)
  (blink-matching-open))

(defun wolfram-electric-paren (arg)
  "Indent on closing a paren ARG times."
  (interactive "p")
  (wolfram-electric ")" arg))

(defun wolfram-electric-braket (arg)
  "Indent on closing a braket ARG times."
  (interactive "p")
  (wolfram-electric "]" arg))

(defun wolfram-electric-brace (arg)
  "Indent on closing a brace ARG times."
  (interactive "p")
  (wolfram-electric "}" arg))

(defun wolfram-electric-assoc (arg)
  "Indent on closing a association ARG times."
  (interactive "p")
  (wolfram-electric "|>" arg))

;; * inferior Mathematica mode. *

(defun wolfram-proc ()
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-wolfram-mode)
				      (current-buffer)
				    "*wolfram*"))))
    (or proc
	(error "No current process.  Do M-x `run-wolfram'"))))

(defun wolfram-send-region (start end)
  "Send the current region to the inferior Mathematica process."
  (interactive "r")
  (comint-send-region (wolfram-proc) start end)
  (comint-send-string (wolfram-proc) "\C-j"))

(define-derived-mode inferior-wolfram-mode comint-mode "Inferior Mathematica"
  "Major mode for interacting with an inferior Mathematica process"
  :abbrev-table wolfram-mode-abbrev-table
  (setq comint-prompt-regexp "^(In|Out)\[[0-9]*\]:?= *")
  (wolfram-mode-variables)
  (setq mode-line-process '(":%s"))
  (setq comint-process-echoes t))

;;;###autoload
(defun run-wolfram (cmd)
  "Run an inferior Mathematica process CMD, input and output via buffer *wolfram*."
  (interactive (list (if current-prefix-arg
                         (read-string "Run Mathematica: " wolfram-program)
                       wolfram-program)))
  (setq wolfram-program cmd)
  (let ((cmdlist (append (split-string-and-unquote wolfram-program)
                         wolfram-program-arguments)))
    (pop-to-buffer-same-window
     (set-buffer (apply 'make-comint-in-buffer "wolfram" (get-buffer "*wolfram*")
                        (car cmdlist) nil (cdr cmdlist)))))
  (inferior-wolfram-mode))

(defun wolfram-here-is-space ()
  (let ((ca (char-after))
	(cb (char-before)))
    (and ca cb
	 (string-match "[ \t\n]" (char-to-string ca))
	 (string-match "[ \t\n]" (char-to-string cb)))))

(defun wolfram-moveto-last-content ()
  (while (wolfram-here-is-space)
    (backward-char 1)))

(defun wolfram-moveto-first-content ()
  (while (wolfram-here-is-space)
    (forward-char 1)))

(defun wolfram-beginning-of-cell ()
  (wolfram-moveto-last-content)
  (if (re-search-backward "^$" nil t) (forward-char 1)
    (goto-char (point-min))))

(defun wolfram-end-of-cell ()
  (wolfram-moveto-first-content)
  (if (re-search-forward "^$" nil t) (backward-char 1)
    (goto-char (point-max))))

(defun wolfram-send-last-mathexp ()
  "Send the last math expression to the inferior Mathematica process."
  (interactive)
  (save-excursion
    (let ((wolfram-start (progn (wolfram-beginning-of-cell) (point)))
	  (wolfram-end (progn (wolfram-end-of-cell) (point))))
      (comint-send-region (wolfram-proc) wolfram-start wolfram-end)
      (comint-send-string (wolfram-proc) "\C-j"))))

;; * mathematica pretty print *

(defun wolfram-run-script ()
  "Execute and update file"
  (interactive)
  (save-buffer)
  (let ((cur-name (file-name-base (buffer-file-name)))
	(cur-file (file-name-nondirectory (buffer-file-name)))
	(cur-dir  (file-name-directory (buffer-file-name)))
	(output-buffer (get-buffer-create "*MathematicaOutput*"))
	(pretty-buffer)
	(pretty-file))

    ;; Prepare output buffer
    (with-current-buffer output-buffer
      (delete-region (point-min) (point-max)))

    ;; Ensure that package EPrint.m exists
    (wolfram-run-check-or-make-eprint-package)

    ;; Call Mathematica
    (call-process-shell-command (concat "cd "
					cur-dir
					"; MathKernel -script "
					cur-file)
				nil output-buffer)

    ;; Open buffer for pretty printing
    (setq pretty-file (concat cur-dir ".pprint_" cur-name ".org"))
    (setq pretty-buffer (find-file-noselect pretty-file t))
    (display-buffer pretty-buffer)

    ;;(my-run-command-other-window "MathKernel -script test.m")
    (with-current-buffer pretty-buffer
      (rename-buffer (concat "*MathematicaPrettyPrint_" cur-name "*"))
      (revert-buffer nil t nil)
      (goto-char (point-min))
      (org-remove-latex-fragment-image-overlays)
      (org-toggle-latex-fragment)
      (goto-char (point-max)))))

(defun wolfram-run-check-or-make-eprint-package ()
  "Checks if EPrint.m package exists in `wolfram-path'. \
The packed will be created it it is does not exists.

Also returns an error if `wolfram-path' is nil"
  (unless wolfram-path
    (error "Please set `wolfram-path', so package EPrint.m can be created in that directory"))

  (unless (file-exists-p (concat wolfram-path "/EPrint.m"))
    (write-region
     "
BeginPackage[ \"EPrint`\"]

EPrint::usage =
\"EPrint[ expr ] does pretty prints of expresion `expr` in emacs. You have to run *.m script in emacs via function `wolfram-run-script`.\"

Begin[ \"Private`\"]

n = 0;

EPrint[ expr_ ] :=
	Module[ {file,dir},
		file = $InputFileName;
		dir  = DirectoryName[file];
		name = FileBaseName[file];
		ppfile = FileNameJoin[{dir,\".pprint_\"<>name<>\".org\"}];
		WriteString[ ppfile, StringForm[\"#`1`:\\n\",n]];
		WriteString[ ppfile, \"\\\\begin{equation*}\\n\" ];
		WriteString[ ppfile, TeXForm[expr]];
		WriteString[ ppfile, \"\\n\\\\end{equation*}\"];
		WriteString[ ppfile, \"\\n\\n\"];
		n = n + 1
	]
End[]

EndPackage[]
"
     nil (concat wolfram-path "/EPrint.m"))))

;; * Provide *

(provide 'wolfram-mode)

;; Local Variables:
;; coding: utf-8-unix
;; time-stamp-pattern: "10/Modified:\\\\?[ \t]+%:y-%02m-%02d\\\\?\n"
;; End:

;;; wolfram-mode.el ends here
