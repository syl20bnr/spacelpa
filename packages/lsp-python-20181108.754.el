;;; lsp-python.el --- Python support for lsp-mode -*- lexical-binding: t -*-

;; Copyright (C) 2017 Vibhav Pant <vibhavp@gmail.com>

;; Author: Vibhav Pant <vibhavp@gmail.com>
;; Version: 1.0
;; Package-Version: 20181108.754
;; Package-Requires: ((lsp-mode "3.0"))
;; Keywords: python
;; URL: https://github.com/emacs-lsp/lsp-python

;;; Code:
(require 'lsp-mode)
(require 'lsp-common)

(defcustom lsp-python-server-args
  '()
  "Extra arguments for the python-stdio language server"
  :group 'lsp-python
  :risky t
  :type '(repeat string))

(defcustom lsp-python-use-init-for-project-root
  nil
  "Set to t to look for __init__.py files to determine a project's root.

The first directory not containing an __init__.py file (looking
upwards from the directory the open python file is in) is set as
the project root for the lsp server.
"
  :group 'lsp-python
  :type 'boolean)

(defun lsp-python--ls-command ()
  "Generate the language server startup command."
  `("pyls" ,@lsp-python-server-args))

(lsp-define-stdio-client lsp-python "python" nil nil
                         :command-fn 'lsp-python--ls-command)

(provide 'lsp-python)
;;; lsp-python.el ends here
