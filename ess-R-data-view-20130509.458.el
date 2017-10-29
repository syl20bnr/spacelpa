;;; ess-R-data-view.el --- Data viewer for GNU R

;; Author: myuhe <yuhei.maeda_at_gmail.com>
;; Maintainer: myuhe
;; URL: https://github.com/myuhe/ess-R-data-view.el
;; Package-Version: 20130509.458
;; Version: 0.1
;; Created: 2013-05-09
;; Keywords: convenience
;; Package-Requires: ((ctable "20130313.1743") (popup "20130324.1305") (ess "20130225.1754"))
;; Copyright (C) 2013 myuhe

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (a your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ess-R-data-view is data viewer for GNU R. It shows dataframe and matrix
;; on table view.
;; ess-R-data-view provides two commands. The first, `ess-R-dv-ctable' 
;; shows table in other buffer. It includes  border, and header is fixed.
;; The second, `ess-R-dv-pprint' shows pretty-printed text in other buffer. 
;; It shows huge text smoothly.

;;; Code:

(require 'ess-inf)
(require 'ctable)
(require 'popup)

(defvar ess-R-dv-buf " R data view"
  "Buffer for R data")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun ess-R-dv-pprint ()
  (interactive)
  (pop-to-buffer (ess-R-dv-execute (current-word))))

;;;###autoload
(defun ess-R-dv-ctable ()
  (interactive)
  (let ((obj (current-word))
        (type (ess-R-dv-type-of)))
    (if (or (string= type "data.frame")
            (string= type "matrix"))
        (ess-R-dv-ctable-1 obj type)
      (popup-tip (concat "\"" obj "\"" " is invalid data !!")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Internal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ess-R-dv-ctable-1 (obj type)
  (with-current-buffer (ess-R-dv-execute obj)
    (goto-char (point-min))
    (let ((param (copy-ctbl:param ctbl:default-rendering-param)))
    (setf (ctbl:param-fixed-header param) t)
      
    (let* ((ln
            (ess-R-dv-substring))
           (header-lst
            (e2wm:dp-R-gen-header-lst ln type))
           (column-model
            (mapcar 
             (lambda (i) (make-ctbl:cmodel :title i ))
             (ess-R-dv-map ln header-lst)))
           data)
      
      (dotimes (x (1- (count-lines (point-max) (point-min))))
        (forward-line 1)
        (add-to-list 
         'data (ess-R-dv-map (ess-R-dv-substring) header-lst) t))

      (pop-to-buffer (ctbl:cp-get-buffer 
                      (ctbl:create-table-component-buffer
                       :model (make-ctbl:model
                               :column-model column-model
                               :data data)
                       :param param)))))))

(defun ess-R-dv-execute (obj)
  (let ((buf (get-buffer-create ess-R-dv-buf)))
    (ess-command (ess-R-dv-get obj) buf)
    (with-current-buffer buf
      (goto-char (point-min)))
    buf))

(defun ess-R-dv-type-of ()
  (let ((obj (current-word))
        (tmpbuf (get-buffer-create " *ess-R-tmpbuf*"))
        type)
    (ess-command (concat "class(" obj ")\n")  tmpbuf)
    (with-current-buffer tmpbuf
       (setq type (buffer-substring
                         (+ 2 (string-match "\".*\"" (buffer-string)))
                         (- (point-max) 2))))
    (kill-buffer tmpbuf)
    type))

(defun ess-R-dv-map (ln lst)
  (mapcar  
   (lambda (i) 
     (substring ln (car i) (cdr i))) lst))

(defun ess-R-dv-substring ()
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun ess-R-dv-get (name)
  "Generate R code to get the value of the variable name.
This is complicated because some variables might have spaces in their names.
Otherwise, we could just pass the variable name directly to *R*."
  (concat "get(" (ess-R-dv-quote name) ")\n"))

(defun ess-R-dv-quote (name)
  "Quote name if not already quoted."
  (if (equal (substring name 0 1) "\"")
      name
    (concat "\"" name "\"")))

(defun e2wm:dp-R-gen-header-lst (str type)
  (let (header-lst 
        (pos (length (number-to-string (1- (count-lines (point-max) (point-min)))))))
    (when (string= type "matrix")
      (setq pos (+ 3 pos)))
    (add-to-list 
     'header-lst (cons 0 pos))
    (while
        (> (length str) pos)
      (add-to-list 
       'header-lst
       (cons pos (let ((pos-match (string-match "[^\\s ]\\s " str pos))) 
                   (if pos-match
                       (+ 1 pos-match)
                     (length str)))) t)
      (setq pos  (+ 1 (cdar (last header-lst)))))
    header-lst))

(provide 'ess-R-data-view)

;;; ess-R-data-view.el ends here
