;;; flycheck-bashate.el --- Integrate bashate with flycheck

;; Copyright (c) 2016 Alex Murray

;; Author: Alex Murray <murray.alex@gmail.com>
;; Maintainer: Alex Murray <murray.alex@gmail.com>
;; URL: https://github.com/alexmurray/flycheck-bashate
;; Package-Version: 20160629.2140
;; Version: 0.1
;; Package-Requires: ((flycheck "0.24") (emacs "24.4"))

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

;; This packages integrates bashate with flycheck to automatically check the
;; style of your bash scripts on the fly using bashate

;;;; Setup

;; (eval-after-load 'flycheck
;;   '(progn
;;      (require 'flycheck-bashate)
;;      (flycheck-bashate-setup)))

;;; Code:
(require 'flycheck)

(flycheck-define-checker bashate
  "A checker using bashate.

See `https://github.com/alexmurray/bashate/'."
  :command ("bashate" source)
  :error-patterns ((error line-start "[E] "(message (minimal-match (one-or-more not-newline))) ": '" (one-or-more not-newline) "'\n"
                          " - " (file-name) " : L" line line-end)
                   (warning line-start "[W] "(message (minimal-match (one-or-more not-newline))) ": '" (one-or-more not-newline) "'\n"
                            " - " (file-name) " : L" line line-end))
  :modes sh-mode)

;;;###autoload
(defun flycheck-bashate-setup ()
  "Setup flycheck-bashate.

Add `bashate' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'bashate))

(provide 'flycheck-bashate)

;;; flycheck-bashate.el ends here
