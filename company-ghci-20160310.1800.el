;;; company-ghci.el --- company backend which uses the current ghci process.-*- lexical-binding: t -*-

;; Author: Hector Orellana <hofm92@gmail.com>
;; Package-Requires: ((company "0.8.11") (haskell-mode "13"))
;; Package-Version: 20160310.1800
;; Package-X-Original-Version: 0.03

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

;;; Usage:

;; (require 'company-ghci)
;; (add-to-list 'company-backends 'company-ghci)
;; (add-hook 'haskell-mode-hook 'company-mode)

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'haskell)
(require 'haskell-utils)
(require 'haskell-process)
(require 'haskell-completions)

(defun company-ghci/hoogle-info (symbol)
  "Use hoogle --info to search documentation of SYMBOL"
  (when (executable-find "hoogle")
    (shell-command-to-string (format "hoogle --info %s" symbol))))

(defun company-ghci/repl-command (cmd)
  "Execute CMD in the ghci process."
  (let ((response (haskell-utils-reduce-string
		   (haskell-process-queue-sync-request (haskell-process)
						       cmd))))
    (when (eq (haskell-utils-repl-response-error-status response) 'no-error)
      response)))

(defun company-ghci/get-signature (function)
  "Uses the :t repl command to get the signature of FUNCTION."
  (company-ghci/repl-command (concat ":t " function)))

(defun company-ghci/get-completions (to-complete)
  (let ((completion-info (haskell-completions-sync-repl-completion-at-point)))
    (when completion-info
      (cl-destructuring-bind
          (_beg _end completions) completion-info
        (cl-remove-if-not (lambda (candidate) (string-prefix-p to-complete candidate))
                          completions)))))

(defun company-ghci/can-complete-p ()
  (and (haskell-session-maybe)
       (let ((prefix-info (haskell-completions-grab-prefix)))
         (when prefix-info
           (cl-destructuring-bind
               (_beg _end prefix _type) prefix-info
             prefix)))))

;;;###autoload
(defun company-ghci (command &optional arg &rest ignored)
  "Company backend that provides completions using the current ghci process."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ghci))
    (prefix  (company-ghci/can-complete-p))
    (candidates (company-ghci/get-completions arg))
    (meta (company-ghci/get-signature arg))
    (doc-buffer (company-doc-buffer (company-ghci/hoogle-info arg)))))

(provide 'company-ghci)
;;; company-ghci.el ends here
