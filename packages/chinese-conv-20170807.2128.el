;;; chinese-conv.el --- Conversion between Chinese Characters with opencc or cconv

;; Copyright (C) 2012-2016 Cong Gu

;; Author: gucong <gucong43216@gmail.com>
;; Version: 0.1.0
;; Package-Version: 20170807.2128
;; Url: https://github.com/gucong/emacs-chinese-conv
;; Package-Requires: ((cl-lib "0.5"))

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

;; This file works with opencc (https://github.com/BYVoid/OpenCC)
;; or cconv (https://github.com/xiaoyjy/cconv).  Customizable through
;; `chinese-conv-backend'.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;   (require 'chinese-conv)

;; Commands `chinese-conv' and `chinese-conv-replace' can be used
;; interactively or non-interactively.

;;; Changelog:

;; 2016/02/17
;;     * Opencc backend data path now customizable through
;;       `chinese-conv-opencc-data'
;;
;; 2015/02/26
;;     * Opencc backend update for its JSON configuration files
;;
;; 2013/02/13
;;     * New feature: multiple backend.
;;       Customizable through `chinese-conv-backend'.
;;       Built-in support for opencc and cconv. More backends
;;       can be added through `chinese-conv-backend-alist'
;;     * New Command: `chinese-conv-replace'
;;     * API change: `chinese-conv'
;;
;; 2012/01/28
;;     * Initial commit
;;

;;; Code:

(defvar chinese-conv-temp-path
  (concat temporary-file-directory "chinese_conv.tmp")
  "Temporary file for Chinese conversion.")

;; opencc backend
(defvar chinese-conv-opencc-program
  "opencc"
  "The opencc program path.")

(defvar chinese-conv-opencc-data
  "/usr/share/opencc/"
  "The opencc data path.")

(defvar chinese-conv-opencc-alist
  '(("traditional" "s2t.json")
    ("simplified" "t2s.json")
    ("Simplified Chinese to Traditional Chinese (Taiwan Standard)" "s2tw.json")
    ("Traditional Chinese (Taiwan Standard) to Simplified Chinese" "tw2s.json")
    ("Simplified Chinese to Traditional Chinese (Hong Kong Standard)" "s2hk.json")
    ("Traditional Chinese (Hong Kong Standard) to Simplified Chinese" "hk2s.json")
    ("Simplified Chinese to Traditional Chinese (Taiwan Standard) with Taiwanese idiom" "s2twp.json")
    ("Traditional Chinese (Taiwan Standard) to Simplified Chinese with Mainland Chinese idiom" "tw2sp.json"))
  "Alist of opencc conversions.")

(defun chinese-conv-opencc-command (conv)
  "Generate the command for calling opencc.  CONV is one of the conversion types defined in `chinese-conv-opencc-alist'."
  (let ((arg (cadr (assoc conv chinese-conv-opencc-alist))))
    (if (null arg) (error "Undefined conversion")
      (concat chinese-conv-opencc-program
              " -i " chinese-conv-temp-path
              " -c " chinese-conv-opencc-data arg))))

;; cconv backend
(defvar chinese-conv-cconv-program
  "cconv"
  "The cconv program path.")

(defvar chinese-conv-cconv-alist
  '(("simplified"  "UTF8-CN")
    ("traditional" "UTF8-TW")
    ("Taiwan"      "UTF8-TW")
    ("Hong Kong"   "UTF8-HK"))
  "Alist of cconv conversions.")

(defun chinese-conv-cconv-command (conv)
  "Generate the command for calling cconv.  CONV is one of the conversion type identifiers in `chinese-conv-cconv-alist'."
  (let ((arg (cadr (assoc conv chinese-conv-cconv-alist))))
    (if (null arg) (error "Undefined conversion")
      (concat chinese-conv-cconv-program
              " -f UTF8 " " -t " arg
              " " chinese-conv-temp-path))))

;; common
(defvar chinese-conv-backend-alist
  `(("opencc" ,chinese-conv-opencc-alist ,#'chinese-conv-opencc-command)
    ("cconv" ,chinese-conv-cconv-alist ,#'chinese-conv-cconv-command))
  "An alist to provide essential information about backends.

Format is ((BACKEND CONVERSION-ALIST COMMAND-GENERATOR) ... ).

CONVERSION-ALIST is in the format ((IDENTIFIER INFO ... ) ... ).
The IDENTIFIER specifies a conversion direction and INFO provides
information to be used by COMMAND-GENERATOR.  It is preferable to
have \"simplified\" and \"traditional\" among the IDENTIFIERs.

COMMAND-GENERATOR is a function that consumes a string to be
matched with IDENTIFIER in CONVERSION-ALIST and produces a shell
command in the form of a string.")

(defvar chinese-conv-backend
  "opencc"
  "Backend of the conversion, see `chinese-conv-backend-alist'.")

(defun chinese-conv-get-alist (&optional backend)
  "Get conversion alist for the given BACKEND."
  (let ((l (cadr (assoc (or backend chinese-conv-backend)
                        chinese-conv-backend-alist))))
    (if (null l) (error "Undefined backend")
      l)))

(defun chinese-conv-get-command (&optional backend)
  "Get command generator for the given BACKEND."
  (let ((f (cadr (cdr (assoc (or backend chinese-conv-backend)
                         chinese-conv-backend-alist)))))
    (if (null f) (error "Undefined backend")
      f)))

;;;###autoload
(defun chinese-conv (str conv &optional backend)
  "Convert a Chinese string, eg. between simplified and traditional forms.
Can be used interactively or non-interactively.

STR is the string to convert.
CONV is one of the conversion type identifiers.
BACKEND is the backend to be used, see `chinese-conv-backend-alist'.

Also see `chinese-conv-replace'."
  (interactive
   (let* ((guess (or (and transient-mark-mode mark-active
                        (buffer-substring-no-properties
                         (region-beginning) (region-end)))
                   (current-word nil t)))
          (word (read-string (format "String to convert (default: %s): " guess)
                             nil nil guess))
          (conv (completing-read "Conversion: "
                                 (chinese-conv-get-alist) nil t)))
     (list word conv nil)))
  (with-temp-file chinese-conv-temp-path
    (insert str "\n"))
  (let ((result
         (substring
          (shell-command-to-string
           (funcall (chinese-conv-get-command backend) conv))
          0 -1)))
    (if (called-interactively-p 'any)
        (message result)
      result)))

;;;###autoload
(defun chinese-conv-replace (start end conv &optional backend)
  "Convert a Chinese string in place.
Can be used interactively or non-interactively.

START is starting position in buffer.
END is the ending position in buffer.
CONV is one of the conversion type identifiers.
BACKEND is the backend to be used, see `chinese-conv-backend-alist'.

Also see `chinese-conv'."
  (interactive
   (let ((start (region-beginning))
         (end (region-end))
         (conv (completing-read "Conversion: "
                                (chinese-conv-get-alist) nil t)))
     (list start end conv)))
  (let ((str (buffer-substring-no-properties start end)))
    (kill-region start end)
    (insert (chinese-conv str conv (or backend chinese-conv-backend)))))

(provide 'chinese-conv)

;;; chinese-conv.el ends here
