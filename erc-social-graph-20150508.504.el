;;; erc-social-graph.el --- A social network graph module for ERC.

;; Copyright (C) 2014 Vibhav Pant <vibhavp@gmail.com>

;; Url: https://github.com/vibhavp/erc-social-graph
;; Package-Version: 20150508.504
;; Author: Vibhav Pant <vibhavp@gmail.com>
;; Version: 1.0
;; Keywords: erc graph

;;; Commentary:
;; erc-social-graph scans user received messages, and generates a social graph
;; of the channel. These graphs can be later converted to a DOT graph, which
;; can be drawn into a graph using Graphviz.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;; This file is not a part of GNU Emacs.
;;; Code:
(require 'erc)
(require 'cl)

(defcustom erc-social-graph-update-function 'erc-social-graph-update-graph
  "The function used for updating the graph hash table.
 Takes the message sender as an argument."
  :type 'function
  :group 'erc-social-graph)

(defcustom erc-social-graph-dynamic-graph nil
  "Update the DOT graph buffer on every sent message."
  :type 'boolean
  :group 'erc-social-graph)

(defvar erc-social-graph-table (make-hash-table :test 'equal))
(defvar erc-social-graph-files (make-hash-table :test 'equal))

(defun erc-social-graph-update-graph (sender text)
  "Update the graph for the current channel"
  (maphash (lambda (nick value)
         (let ((result (ignore-errors (string-match
                       (format "%s\\([^[:alpha:]]\\|$\\)"
                           (downcase nick))
                       (downcase text)))))
           (when result
         (let* ((key (concat (downcase sender) "-" (downcase nick)))
            (mentions (gethash key (gethash (buffer-name)
                             erc-social-graph-table)
                    nil)))
           (if (eq mentions nil)
               (puthash key (setq mentions 1) (gethash
                               (buffer-name)
                               erc-social-graph-table))
             (puthash key (incf mentions) (gethash (buffer-name)
                               erc-social-graph-table)))
           (when erc-social-graph-dynamic-graph
             (erc-social-graph-update-dot-buffer (buffer-name)
                             (downcase sender)
                             (downcase nick)
                             mentions))))))
       erc-channel-users))
 (defun erc-social-graph-chomp (str)
   "Chomp leading and tailing whitespace from STR."
   (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                        str)
     (setq str (replace-match "" t t str)))
   str)


(defun erc-social-graph-check ()
  "Check if the text sent in the current buffer is a user sent message,
 and accordingly pass it to `erc-social-graph-update-function'"
  (let ((text (erc-social-graph-chomp (buffer-substring-no-properties (point-min) (point-max)))))
    (when (string=  (substring text 0 1) "<")
      (let ((sender (substring text (+ 1 (string-match "<" text))
				(string-match ">" text))))
	(setq text (substring text (+ 2 (string-match ">" text))))
	(funcall erc-social-graph-update-function sender text)))))

(defun erc-social-graph-create ()
  "Create empty graph for current-buffer"
  (puthash (buffer-name) (make-hash-table :test 'equal) erc-social-graph-table))

(defun erc-social-graph-update-dot-buffer (channel nick1 nick2 mentions)
  "Update DOT buffer for channel, set \"NICK1 -> NICK2 mentions\" to MENTIONS"
  (let ((file (gethash channel erc-social-graph-files)))
    (if file
	(with-current-buffer (find-file file)
	  (goto-char (point-min))
	  (if (re-search-forward (format
				  "\"%s\" -> \"%s\" \\[penwidth = [[:digit:]]+\\]"
				  nick1
				  nick2)
				 (point-max)
				 t)
	      (replace-match (format "\"%s\" -> \"%s\" [penwidth = %d]"
				     nick1
				     nick2
				     (if (> mentions 6)
					 6
				       mentions)))
	    ;; Create new edge/node(s)
	    (goto-char (- (point-max) 2))
	    (insert (format "\"%s\" -> \"%s\" [penwidth = %d]; \n"
			    nick1 nick2
			    (if (> mentions 6)
				6
			      mentions))))
	  (save-buffer)
	  (kill-buffer))
      ;; Create new file
      (with-current-buffer
	  (find-file (erc-social-graph-make-temp-file channel))
	(goto-char (- (point-max) 2))
	(insert (format "\"%s\" -> \"%s\" [penwidth = %d];\n"
			nick1 nick2 (if (> mentions 6)
					6
				      mentions)))
	(save-buffer)
	(kill-buffer)))))

(defun erc-social-graph-make-temp-file (channel)
  "Make a temporary DOT file for CHANNEL's buffer, and return the file name"
  (let ((file (make-temp-file
	       (concat "erc-social-graph-" channel)
	       nil
	       ".dot")))
    (with-temp-file file
      (insert "digraph{\n")
      (insert "labelloc=\"t\";\n")
      (insert (format "label=\"Generated by erc-social-graph using %s\";\n\n"
		      (erc-version)))
      (insert "}"))
    (puthash channel file erc-social-graph-files)
    file))

(defun erc-social-graph-draw (channel)
  "Draw graph for CHANNEL.
If `erc-social-graph-dynamic-graph' is non-nil preview the graph using dot's
\"Tx11\" option"
  (interactive (list (completing-read "Draw graph for channel: "
                      (mapcar 'buffer-name (erc-buffer-list)))))
  (let ((file (or (gethash channel erc-social-graph-files)
                  (erc-social-graph-make-temp-file channel)))
        (channel-graph (gethash channel erc-social-graph-table)))
    (with-current-buffer (find-file file)
      (goto-char (- (point-max) 2))
      (maphash (lambda (link value)
                 (insert (format "\"%s\" -> \"%s\" [penwidth = %d];\n"
                                 (substring link 0 (string-match "-" link))
                                 (substring link (+ 1 (string-match "-" link)))
                                 (if (> value 4)
                                     6
                                   value))))
               channel-graph))
    (if erc-social-graph-dynamic-graph
        (async-shell-command (format "dot -Tx11 %s"
                                     (gethash channel erc-social-graph-files)))
      (find-file file))))

(define-erc-module social-graph nil
  "Social network graphs for emacs"
  ;; Enable
  ((add-hook 'erc-join-hook 'erc-social-graph-create)
   (add-hook 'erc-insert-post-hook 'erc-social-graph-check)
   (add-hook 'erc-send-post-hook 'erc-social-graph-check))
  ;; Disable
  ((remove-hook 'erc-insert-post-hook 'erc-social-graph-check)
   (remove-hook 'erc-join-hook 'erc-social-graph-check)
   (remove-hook 'erc-send-post-hook 'erc-social-graph-check)))

(provide 'erc-social-graph)

;;; erc-social-graph.el ends here
