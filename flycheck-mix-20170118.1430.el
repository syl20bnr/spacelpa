;;; flycheck-mix.el --- Elixir mix flycheck integration -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Tomasz Kowal

;; Author: Tomasz Kowal <tomekowal@gmail.com>
;; Version: 1.0.0
;; Package-Version: 20170118.1430
;; Package-Requires: ((flycheck "27") (elixir-mode "1.8.0"))
;; Keywords: Elixir flycheck mix
;; URL: https://github.com/tomekowal/flycheck-mix

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

;; This package adds support for Elixir mix to flycheck.
;; To use it, add to your init.el:
;;
;; (require 'flycheck-mix)
;; (flycheck-mix-setup)
;;
;; Elixir compilation uses macros and it can run arbitrary code.
;; You should use =elixir-mix= checker only with trusted projects.

;;; Code:
(require 'flycheck)

;; :command uses source-original, source-inplace copies the file
;; which makes mix throw errors
(flycheck-define-checker
 elixir-mix
 "Defines a checker for elixir with mix compile.
  There are to conditions that must be true to fulfil the predicate.
  1. The project must be valid mix project with =mix.exs= file
  2. The variable =flycheck-mix-enable-checking= must be set to =t="
 :command ("elixir"
           "-e"
           (eval (flycheck-mix-cd-option))
           "-S"
           "mix"
           "compile")
 :error-patterns
 ((warning line-start
           "warning:"
           ;; Multiline warnings
           (message (minimal-match (one-or-more anything)))
           (file-name "lib/" (minimal-match (one-or-more not-newline)) )
           ":"
           line
           line-end)
   (error line-start
          "** ("
          (one-or-more word)
          "Error) "
          (file-name)
          ":"
          line
          ": "
          (message)
          line-end))
 :error-filter
 (lambda (errors)
   (dolist (err (flycheck-sanitize-errors errors))
     (setf (flycheck-error-filename err)
           (concat (flycheck-mix-project-root)
                   (flycheck-error-filename err))))
   errors)
 :modes (elixir-mode)
 :predicate (lambda ()
              (and
               (flycheck-buffer-saved-p)
               (flycheck-mix-project-root))))

(defun flycheck-mix-project-root ()
  "Return directory where =mix.exs= is located."
  (locate-dominating-file buffer-file-name "mix.exs"))

(defun flycheck-mix-cd-option ()
  "Generate change directory command for elixir executable."
  (format "IEx.Helpers.cd(\"%s\")"
          (shell-quote-argument (flycheck-mix-project-root))
          ))

;;;###autoload
(defun flycheck-mix-setup ()
  "Setup Flycheck for Elixir."
  (add-to-list 'flycheck-checkers 'elixir-mix))

(provide 'flycheck-mix)
;;; flycheck-mix.el ends here
