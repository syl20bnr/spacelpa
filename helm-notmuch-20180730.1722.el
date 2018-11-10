;;; helm-notmuch.el --- Search emails with Notmuch and Helm  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
;; URL: https://github.com/xuchunyang/helm-notmuch
;; Package-Version: 20180730.1722
;; Keywords: mail
;; Version: 1.1
;; Package-Requires: ((helm "1.9.3") (notmuch "0.21"))

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

;; To use, type M-x helm-notmuch.  helm-notmuch will gets start to search when
;; length of your input is no less than 2.

;; News:
;; - 2017-09-04 v1.1 Fix a regexp bug and use `notmuch-command' instead of hardcode "notmuch"
;; - 2016-11-28 v1.0 Add two user options: `helm-notmuch-max-matches' and `helm-notmuch-match-incomplete-words'

;;; Code:

(require 'helm)
(require 'notmuch)

(defgroup helm-notmuch nil
  "Helm interface for notmuch."
  :group 'notmuch
  :link '(url-link :tag "Homepage" "https://github.com/xuchunyang/helm-notmuch"))

(defcustom helm-notmuch-max-matches 0
  "Maximum number of matches shown.
Notice that a setting of 0 means \"Show all matches\"."
  :group 'helm-notmuch
  :type '(choice (const :tag "Show all matches" 0)
                 (integer :tag "Maximum number of matches shown" 50)))

(defcustom helm-notmuch-match-incomplete-words nil
  "If non-nil, treat last word in query as incomplete.

If this variable is non-nil, include results with words for which
the last word of the input is a prefix. Note that this (slightly)
slows down searches."
  :group 'helm-notmuch
  :type 'boolean)

(defcustom helm-notmuch-thread-count-width 10
  "Maximum width of thread count in display."
  :group 'helm-notmuch
  :type 'integer)

(defcustom helm-notmuch-author-width 20
  "Maximum width of authors in display."
  :group 'helm-notmuch
  :type 'integer)

(defun helm-notmuch-collect-candidates ()
  (let* ((cmds (delq nil (list notmuch-command "search"
                               (and (> helm-notmuch-max-matches 0)
                                    (concat "--limit=" (number-to-string helm-notmuch-max-matches)))
                               helm-pattern)))
         (proc (apply 'start-process "helm-notmuch" helm-buffer cmds)))
    (prog1 proc
      (set-process-sentinel
       proc
       (lambda (process event)
         (cond
          ((= 1 (process-exit-status process))
           (with-helm-buffer
             (setq mode-line-format
                   '(" " mode-line-buffer-identification " "
                     "[notmuch process finished - (no results)]"))
             (force-mode-line-update)))
          ((string= "finished\n" event)
           (with-helm-buffer
             (setq mode-line-format
                   '(" " mode-line-buffer-identification " "
                     (:eval (format "L%s" (helm-candidate-number-at-point)))
                     " "
                     (:eval (format
                             "[notmuch process finished - (%s results)]"
                             (helm-get-candidate-number)))))
             (force-mode-line-update)))))))))

(defconst helm-notmuch-thread-id-length (length "thread:0000000000000028"))

(defun helm-notmuch-candidate-formatter (cand)
  "Format the single entry CAND."
  (let ((text (substring cand (+ 2 helm-notmuch-thread-id-length)))
        (id (substring cand 0 helm-notmuch-thread-id-length))
        cstart astart alen tstart tags)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))

      ; Align message counts
      (search-forward "[")
      (setq cstart (point))
      (search-forward "]")
      (save-excursion
        (save-restriction
          (narrow-to-region cstart (point))
          (goto-char (point-min))
          (when (re-search-forward "\([0-9]\+\)" nil t)
            (replace-match ""))))
      (forward-char)
      (just-one-space (- helm-notmuch-thread-count-width
                         (- (point) cstart)))
      (forward-char)

      ; Align (and truncate) authors
      (setq astart (point))
      (search-forward ";")
      (delete-char -1)
      (setq alen (- (point) astart))
      (if (> alen helm-notmuch-author-width)
          (progn
            (delete-region (- (point) (- alen
                                         (- helm-notmuch-author-width 3)))
                           (point))
            (insert "..."))
        (just-one-space (- (+ helm-notmuch-author-width 1) alen)))

      ; Colour tags
      (goto-char (- (point-max) 1))
      (save-excursion
        (search-backward "(")
        (setq tstart (+ (point) 1)))
      (setq tags (split-string (buffer-substring tstart (point))))
      (delete-region tstart (point))
      (insert (notmuch-tag-format-tags tags tags))

      ; Colour the whole line according to tags
      (notmuch-search-color-line (point-min) (point-max) tags)
      (setq text (buffer-string)))
    (cons text id)))

(defun helm-notmuch-maybe-match-incomplete (pattern)
  (if helm-notmuch-match-incomplete-words
      (if (string-match-p "[[:alnum:]]$" pattern)
          (concat pattern "*")
        pattern)
    pattern))

(defun helm-notmuch-show (candidate)
  "Display CANDIDATE using notmuch-show, retaining the query context."
  (notmuch-show candidate nil nil
                (helm-notmuch-maybe-match-incomplete helm-pattern)))

(defun helm-notmuch-search (candidate)
  "Display notmuch query in notmuch-search buffer, highlighting CANDIDATE."
  (notmuch-search (helm-notmuch-maybe-match-incomplete helm-pattern)
                  nil
                  (replace-regexp-in-string "^thread:" "" candidate)))

(defvar helm-source-notmuch
  (helm-build-async-source "Search email with notmuch"
    :candidates-process #'helm-notmuch-collect-candidates
    :filter-one-by-one #'helm-notmuch-candidate-formatter
    :requires-pattern 2
    :pattern-transformer #'helm-notmuch-maybe-match-incomplete
    :nohighlight t
    :action '(("Show message in notmuch" . helm-notmuch-show)
              ("Open notmuch-search query buffer" . helm-notmuch-search))))

;;;###autoload
(defun helm-notmuch ()
  (interactive)
  (helm :sources helm-source-notmuch
        :buffer "*helm notmuch*"
        :truncate-lines t))

(provide 'helm-notmuch)
;;; helm-notmuch.el ends here
