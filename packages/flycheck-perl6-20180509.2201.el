;;; flycheck-perl6.el --- Perl 6 support in Flycheck -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Hinrik Örn Sigurðsson <hinrik.sig@gmail.com>

;; Author: Hinrik Örn Sigurðsson <hinrik.sig@gmail.com>
;; URL: https://github.com/hinrik/flycheck-perl6
;; Package-Version: 20180509.2201
;; Keywords: tools, convenience
;; Version: 0.1-git
;; Package-Requires: ((emacs "24.3") (flycheck "0.22"))

;; This file is not part of GNU Emacs.

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

;; Perl 6 syntax checking support for Flycheck.

;; Runs "perl6 -c" on your code. Currently does not report the exact
;; column number of the error, just the line number.

;;; Code:

(require 'flycheck)

(defgroup flycheck-perl6 nil
  "Perl 6 support for Flycheck."
  :prefix "flycheck-perl6-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/hinrik/flycheck-perl6"))

(flycheck-def-option-var flycheck-perl6-include-path nil perl6
  "A list of include directories for Perl6.

The value of this variable is a list of strings, where each
string is a directory to add to the include path of Perl6.
Relative paths are relative to the file being checked."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p)

(flycheck-define-checker perl6
  "A Perl 6 syntax checker."
  :command ("perl6" "-c"
            (option-list "-I" flycheck-perl6-include-path) source)
  :error-patterns
  ((error (or (and line-start (message) (? "\r") "\nat " (file-name) ":" line (? "\r") line-end)
              (and "compiling " (file-name) (? "\r") "\n" (message (minimal-match (1+ anything))) " at line " line)
              ; "Module not found" message
              (and "===SORRY!===" (? "\r") "\n" (message (minimal-match (1+ anything))) " at line " line))))
  :modes perl6-mode)

(add-to-list 'flycheck-checkers 'perl6)

(provide 'flycheck-perl6)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; flycheck-perl6.el ends here
