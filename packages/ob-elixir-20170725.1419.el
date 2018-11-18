;;; ob-elixir.el --- org-babel functions for elixir evaluation

;; Copyright (C) 2015 ZHOU Feng

;; Author: ZHOU Feng <zf.pascal@gmail.com>
;; URL: http://github.com/zweifisch/ob-elixir
;; Package-Version: 20170725.1419
;; Keywords: org babel elixir
;; Version: 0.0.1
;; Created: 28th Sep 2015
;; Package-Requires: ((org "8"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; org-babel functions for elixir evaluation
;;

;;; Code:
(require 'ob)

(defvar ob-elixir-process-output "")

(defconst org-babel-header-args:elixir
  '((cookie . :any)
    (name . :any)
    (remsh . :any)
    (sname . :any))
  "elixir header arguments")

(defvar ob-elixir-eoe "\u2029")

(add-to-list 'org-babel-tangle-lang-exts '("elixir" . "ex"))

(defun org-babel-execute:elixir (body params)
  (let ((session (cdr (assoc :session params)))
        (tmp (org-babel-temp-file "elixir-")))
    (ob-elixir-ensure-session session params)
    (with-temp-file tmp (insert body))
    (ob-elixir-eval session (format "import_file(\"%s\")" tmp))))

(defun ob-elixir-eval (session body)
  (let ((result (ob-elixir-eval-in-repl session body)))
    (replace-regexp-in-string
    "^import_file([^)]+)\n" ""
     (replace-regexp-in-string
      "\r" ""
      (replace-regexp-in-string
       "\n\\(\\(iex\\|[.]+\\)\\(([^@]+@[^)]+)[0-9]+\\|([0-9]+)\\)> \\)+" ""
       result)))))

(defun ob-elixir-ensure-session (session params)
  (let ((name (format "*elixir-%s*" session)))
    (unless (and (get-process name)
                 (process-live-p (get-process name)))
      (with-current-buffer (get-buffer-create name)
        (make-local-variable 'process-environment)
        (setq process-environment (cons "TERM=vt100" process-environment))
        (apply 'start-process name name "iex"
               (append (when (assoc :sname params)
                         (list "--sname" (assoc-default :sname params)))
                       (when (assoc :name params)
                         (list "--name" (assoc-default :name params)))
                       (when (assoc :cookie params)
                         (list "--cookie" (assoc-default :cookie params)))
                       (when (assoc :remsh params)
                         (list "--remsh" (assoc-default :remsh params))))))
      (sit-for 0.5)
      (set-process-filter (get-process name) 'ob-elixir-process-filter)
      (ob-elixir-eval-in-repl session "IEx.configure(colors: [enabled: false])")
      (sit-for 0.2))))

(defun ob-elixir-process-filter (process output)
  (setq ob-elixir-process-output (concat ob-elixir-process-output output)))

(defun ob-elixir-wait ()
  (while (not (string-match-p ob-elixir-eoe ob-elixir-process-output))
    (sit-for 0.2)))

(defun ob-elixir-eval-in-repl (session body)
  (let ((name (format "*elixir-%s*" session)))
    (setq ob-elixir-process-output "")
    (process-send-string name (format "%s\n" body))
    (accept-process-output (get-process name) nil nil 1)
    (process-send-string name (format "\"%s\"\n" ob-elixir-eoe))
    (ob-elixir-wait)
    (replace-regexp-in-string
     (regexp-quote (format "\"%s\"" ob-elixir-eoe)) ""
     ob-elixir-process-output)))

(provide 'ob-elixir)
;;; ob-elixir.el ends here
