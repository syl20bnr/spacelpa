;;; flycheck-credo.el --- flycheck checker for elixir credo

;; Copyright (C) 2016 by Aaron Jensen

;; Author: Aaron Jensen <aaronjensen@gmail.com>
;; URL: https://github.com/aaronjensen/flycheck-credo
;; Package-Version: 20170526.1545
;; Version: 0.1.0
;; Package-Requires: ((flycheck "29"))

;;; Commentary:

;; This package adds support for credo to flycheck.

;; To use it, require it and ensure you have elixir-mode set up for flycheck:

;;   (eval-after-load 'flycheck
;;     '(flycheck-credo-setup))
;;   (add-hook 'elixir-mode-hook 'flycheck-mode)

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'flycheck)

(defun flycheck-credo--working-directory (&rest _ignored)
  "Find directory with from which we can run credo."
  (and buffer-file-name
       (locate-dominating-file buffer-file-name "deps/credo")))

(flycheck-def-option-var flycheck-elixir-credo-strict nil elixir-credo
  "Enable strict mode in credo.

When non-nil, pass the `--strict' flag to credo."
  :type 'boolean
  :safe #'booleanp)

(flycheck-define-checker elixir-credo
  "Elixir credo checker."
  :command ("mix"
            "credo"
            (option-flag "--strict" flycheck-elixir-credo-strict)
            "--format"
            "flycheck"
            "--read-from-stdin"
            source-original)
  :standard-input t
  :predicate
  (lambda () (and buffer-file-name
                  (locate-dominating-file buffer-file-name "deps/credo")))
  :working-directory flycheck-credo--working-directory
  :error-patterns
  ((info line-start (file-name) ":" line ":" column ": " (or "F" "R" "C")  ": " (message) line-end)
   (info line-start (file-name) ":" line ": " (or "F" "R" "C")  ": " (message) line-end)
   (warning line-start (file-name) ":" line ":" column ": " (or "D" "W")  ": " (message) line-end)
   (warning line-start (file-name) ":" line ": " (or "D" "W")  ": " (message) line-end))
  :modes elixir-mode)

;;;###autoload
(defun flycheck-credo-setup ()
  "Setup flycheck-credo.
Add `elixir-credo' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'elixir-credo t))

(provide 'flycheck-credo)
;;; flycheck-credo.el ends here
