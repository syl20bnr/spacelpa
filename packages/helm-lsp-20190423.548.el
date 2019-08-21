;;; helm-lsp.el --- LSP helm integration             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ivan Yonchovski

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords: languages, debug
;; Package-Version: 20190423.548
;; URL: https://github.com/yyoncho/helm-lsp
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (lsp-mode "5.0") (helm "2.0"))
;; Version: 0.1

;;; Commentary:

;; `helm' for lsp function.

;;; Code:

(require 'helm)
(require 'helm-imenu)
(require 'dash)
(require 'lsp-mode)

(defvar helm-lsp-symbols-request-id nil)
(defvar helm-lsp-symbols-result-p nil)
(defvar helm-lsp-symbols-result nil)

(defun helm-lsp-workspace-symbol-action (candidate)
  "Action for helm workspace symbol.
CANDIDATE is the selected item in the helm menu."
  (-let* (((&hash "uri" "range" (&hash "start" (&hash "line" "character"))) (gethash "location" candidate)))
    (find-file (lsp--uri-to-path uri))
    (goto-char (point-min))
    (forward-line line)
    (forward-char character)))

(defun helm-lsp--workspace-symbol (workspaces name input)
  "Search against WORKSPACES NAME with default INPUT."
  (if workspaces
      (helm
       :sources (helm-build-sync-source name
                  :candidates (lambda ()
                                (if helm-lsp-symbols-result-p
                                    helm-lsp-symbols-result
                                  (with-lsp-workspaces workspaces
                                    (-let (((request &as &plist :id request-id) (lsp-make-request
                                                                                 "workspace/symbol"
                                                                                 (list :query helm-pattern))))
                                      ;; cancel if there is pending request
                                      (when helm-lsp-symbols-request-id
                                        (lsp--cancel-request helm-lsp-symbols-request-id)
                                        (setq helm-lsp-symbols-request-id nil))

                                      (setq helm-lsp-symbols-request-id request-id)
                                      (lsp-send-request-async
                                       request
                                       (lambda (candidates)
                                         (setq helm-lsp-symbols-request-id nil)
                                         (and helm-alive-p
                                              (let ((helm-lsp-symbols-result candidates)
                                                    (helm-lsp-symbols-result-p t))
                                                (helm-update))))
                                       'detached)
                                      nil))))
                  :action 'helm-lsp-workspace-symbol-action
                  :volatile t
                  :fuzzy-match t
                  :match (-const t)
                  :keymap helm-map
                  :candidate-transformer (lambda (candidates)
                                           (-map
                                            (-lambda ((candidate &as &hash "containerName" container-name "name" "kind"))
                                              (let ((type (or (alist-get kind lsp--symbol-kind) "Unknown")))
                                                (cons
                                                 (concat (if (s-blank? container-name)
                                                             name
                                                           (concat container-name "." name))
                                                         " "
                                                         (propertize (concat "(" type ")") 'face 'font-lock-keyword-face))
                                                 candidate)))
                                            candidates))
                  :candidate-number-limit nil
                  :requires-pattern 0)
       :input input)
    (user-error "No LSP workspace active")))

;;;###autoload
(defun helm-lsp-workspace-symbol (arg)
  "`helm' for lsp workspace/symbol.
When called with prefix ARG the default selection will be symbol at point."
  (interactive "P")
  (helm-lsp--workspace-symbol (lsp-workspaces)
                              "Workspace symbol"
                              (when arg (thing-at-point 'symbol))))

;;;###autoload
(defun helm-lsp-global-workspace-symbol (arg)
  "`helm' for lsp workspace/symbol for all of the current workspaces.
When called with prefix ARG the default selection will be symbol at point."
  (interactive "P")
  (helm-lsp--workspace-symbol (-uniq (-flatten (ht-values (lsp-session-folder->servers (lsp-session)))))
                              "Global workspace symbols"
                              (when arg (thing-at-point 'symbol))))

(provide 'helm-lsp)
;;; helm-lsp.el ends here
