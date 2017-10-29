;;; gmail-message-mode.el --- A major-mode for editing gmail messages using markdown syntax.

;; Copyright (C) 2013 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/gmail-message-mode
;; Package-Version: 20160627.1147
;; Version: 1.4
;; Package-Requires: ((ham-mode "1.0"))
;; Keywords: mail convenience emulation
;; Prefix: gmm/
;; Separator: -

;;; Commentary:
;;
;; gmail-message-mode
;; ==========
;;
;; **gmail-message-mode** is an emacs major-mode for editing gmail
;; messages using markdown syntax, it is meant for use with browser
;; plugins which allow you to edit text fields with external applications
;; (in this case, emacs). See [Plugins][] below for a list for each
;; browser.
;;
;; **The problem:** Lately, gmail messages have been demanding html. That
;;   made it very hard to edit them outside your browser, because you had
;;   to edit html source code (for instance, linebreaks were ignored and
;;   you had to type `<br>' instead).
;;
;; **gmail-message-mode to the rescue:** Simply activate this mode in
;;   gmail messages (See [Activation][]); the buffer is converted to
;;   markdown and you may edit at will, but the file is still saved as
;;   html behind the scenes so GMail won't know a thing! *See
;;   [ham-mode][1] to understand how this works.*
;;
;; Activation
;; ----------
;; Make sure you install it:
;;
;;     M-x package-install RET gmail-message-mode
;;
;; And that's it!
;; *(if you install manually, note that it depends on [ham-mode][1])*
;;
;; This package will (using `auto-mode-alist') configure emacs to
;; activate `gmail-message-mode' whenever you're editing a file that
;; seems to be a gmail message. However, given the wide range of possible
;; plugins, it's hard to catch them all. You may have to add entries
;; manually to `auto-mode-alist', to make sure `gmail-message-mode' is
;; activated.
;;
;; ## Plugins ##
;;
;; 1. **Google-Chrome or Chromium** - [Edit with emacs][]
;; 2. **Conkeror** - [Spawn Helper (built-in)][]
;; 3. **Firefox** - Used to work with [It's all text][]. See [this thread][] for a hacky workaround.
;;
;;
;; [Activation]: #activation
;;
;; [Plugins]: #plugins
;;
;; [It's all text]: https://addons.mozilla.org/en-US/firefox/addon/its-all-text/
;;
;; [Edit with emacs]: http://www.emacswiki.org/emacs/Edit_with_Emacs
;;
;; [Spawn Helper (built-in)]: http://conkeror.org/ConkerorSpawnHelper
;;
;; [this thread]: http://github.com/docwhat/itsalltext
;;
;; [Old Compose]: http://oldcompose.com/
;;
;; [1]: https://github.com/Bruce-Connor/ham-mode

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Change Log:
;; 1.3.1 - 2013/12/13 - Informative mode name.
;; 1.3   - 2013/12/10 - Support for edit-server (from chrome).
;; 1.2   - 2013/12/10 - BREAKING CHANGES. Renamed a bunch of stuff.
;; 1.1   - 2013/12/09 - gmm/signature-properties can hide the signature.
;; 1.0.1 - 2013/12/07 - gmm/blockquote.
;; 1.0   - 2013/12/05 - Created File.
;;; Code:
(require 'ham-mode)
(require 'server)

(defconst gmail-message-mode-version "1.4" "Version of the gmail-message-mode.el package.")
(defconst gmail-message-mode-version-int 9 "Version of the gmail-message-mode.el package, as an integer.")
(defun gmail-message-mode-bug-report ()
  "Opens github issues page in a web browser. Please send any bugs you find.
Please include your emacs and gmail-message-mode versions."
  (interactive)
  (message "Your gmail-message-mode-version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           gmail-message-mode-version emacs-version)
  (browse-url "https://github.com/Bruce-Connor/gmail-message-mode/issues/new"))

;;;###autoload
(defcustom gmm/auto-mode-list
  `("[\\\\/]\\(inbox\\|mail\\)-google-com.*\\.\\(ckr\\|html?\\|txt\\)\\'" ;conkeror and other stuff
    "[\\\\/]itsalltext[\\\\/]\\(inbox\\|mail\\)\\.google\\..*\\.txt\\'" ;it's all text
    "[\\\\/]pentadactyl\\.\\(inbox\\|mail\\)\\.google\\..*\\.txt\\'" ;pentadactyl
    )
  "List of regexps which will be added to `auto-mode-alist' (associated to `gmail-message-mode').

If the file path matches any of these, `gmail-message-mode' will be
activated on the current file.

If you don't want `gmail-message-mode' to add itself to your
`auto-mode-alist' simply set this variable to nil.

If you add items manually (not through the customization
interface), you'll need to call `gmm/set-amlist' for it
to take effect.
Removing items only takes effect after restarting Emacs."
  :type '(repeat regexp)
  :group 'gmail-message-mode
  :set 'gmm/set-amlist
  :initialize 'custom-initialize-default
  :package-version '(gmail-message-mode . "1.0"))

(defun gmm/save-finish-suspend ()
  "Save the buffer as html, call `server-edit', and suspend the emacs frame.

This command is used for finishing your edits. It'll do all the
buffer needs and then send emacs to the background so that the web
browser can take focus automatically."
  (interactive)
  (save-buffer)
  (if (frame-parameter nil 'client)
      (server-edit)
    (message "Not in a client buffer, won't call `server-edit'."))
  (if (and window-system (not (eq window-system 'pc)))
      (suspend-frame)))

(defvar gmm/blockquote
  (concat "<blockquote style=\"margin: 0px 0px 0px 0.8ex;"
          " border-left: 1px solid rgb(204, 204, 204);"
          " padding-left: 1ex;"
          "\" class=\"gmail_quote\">"))

(defvar gmm/-mirrored-file nil
  "Temporary file used to generate the content of edit-server buffers.

Necessary because edit-server doesn't use actual files to
communicate with chrome.")
(make-variable-buffer-local 'gmm/-mirrored-file)

(defun gmm/-fix-tags (file)
  "Fix special tags for gmail, such as blockquote."
  (let ((newContents
         (with-temp-buffer
           (insert-file-contents file)
           (goto-char (point-min))
           (while (search-forward "<blockquote>" nil t)
             (replace-match gmm/blockquote :fixedcase :literal))
           (buffer-string))))
    (write-region newContents nil file nil t)))

;;;###autoload
(define-derived-mode gmail-message-mode ham-mode "GMail"
  "Designed for GMail messages. Transparently edit an html file using markdown.

When this mode is activated in an html file, the buffer is
converted to markdown and you may edit at will, but the file is
still saved as html behind the scenes.
\\<gmail-message-mode-map>
Also defines a key \\[gmm/save-finish-suspend] for `gmm/save-finish-suspend'.

\\{gmail-message-mode-map}"
  :group 'gmail-message-mode
  (add-hook 'ham-mode-md2html-hook 'gmm/-fix-tags :local)
  (gmm/-propertize-buffer))

;;;###autoload
(define-derived-mode gmail-message-edit-server-mode text-mode
  "GMail/mirror (do NOT edit)"
  ;; (propertize "GMail/mirror (do NOT edit)"
  ;;             'help-echo "This buffer is kept as a mirror of another file.\n\tDon't edit this buffer!")
  "Designed for GMail messages coming from google-chrome's \"Edit with Emacs\".

Not actually meant for editing. This just sets up the buffer as a
mirrored version of an html file that you'll be editing with the
actual `gmail-message-mode'.

This is supposed to be added to `edit-server-url-major-mode-alist',
so that it's called in an edit-server buffer. If you're trying to
use this in any other way, you're probably using the wrong
function. Try using (or extending) `gmail-message-mode' instead."
  :group 'gmail-message-mode
  :group 'gmail-message-mode-edit-server
  (unless (and (boundp 'edit-server-url) edit-server-url)
    (error "This isn't an edit-server buffer!
You're probably using this mode wrong.
See the documentation for `gmail-message-edit-server-mode'."))
  (let ((file (gmm/-generate-temp-file-name))
        (gmm/-mirror-buffer-let (current-buffer)))
    (setq gmm/-mirrored-file file)
    (while (null (ignore-errors
                   (write-region (buffer-string) nil
                                 file nil nil nil 'excl) t))
      (setq file (gmm/-generate-temp-file-name)))
    (message "Opened mirror buffer %s, mirrored file is %s."
             gmm/-mirrored-file (current-buffer))
    (unless (file-exists-p file)
      (error "Mirror file %s not found, but we just created it, so something's really wrong."
             file))
    (add-hook 'edit-server-done-hook 'gmm/-reflect-mirrored-file nil :local)
    (add-hook 'kill-buffer-hook 'gmm/-kill-mirror nil :local)

    ;; ;;we'd like to make it read-only, but it bugs the process
    ;; (setq buffer-read-only t)
    (let ((auto-mode-alist (cons '("-gmm-mirror-[0-9]\\{5\\}\\.gmm\\'"
                                   . gmail-message-client-mode)
                                 auto-mode-alist)))
      (save-current-buffer (find-file file)))
    ;; we can't focus the mirrored file here, because then it would be
    ;; used by edit-server as the actual client. We need to open it
    ;; after edit-server-mode is activated. (this the advice below)
    ))

(defvar gmm/-mirror-buffer-let nil
  "Only used for letbinding `pmm/-mirror-buffer-let'.")
(defvar gmm/-mirror-buffer nil
  "Hold which buffer mirrors this one.")
(make-variable-buffer-local 'gmm/-mirror-buffer)

;;;###autoload
(define-derived-mode gmail-message-client-mode gmail-message-mode "GMail/client"
  "Designed for GMail messages coming from google-chrome's \"Edit with Emacs\".

This mode is meant for editing, it is the sister of
`gmail-message-edit-server-mode', which is not meant for editing.
It works exactly as the simpler `gmail-message-mode', except that
saving or killing this buffer also affects the edit-server's
buffer (which is the mirror of this one).

This is supposed to be added to `auto-mode-alist', so that it's
called when we open mirror files setup by
`gmail-message-edit-server-mode'. If you're trying to use this in
any other way, you're probably using the wrong function. Try
using (or extending) `gmail-message-mode' instead."
  :group 'gmail-message-mode
  :group 'gmail-message-mode-edit-server
  (setq gmm/-mirror-buffer gmm/-mirror-buffer-let)
  ;; ;; We'd like to use this hook, so that the browser is always up
  ;; ;; to date. But edit-with-emacs starts a new process every time
  ;; ;; the browser is updated, which screws up the UX.
  ;; (add-hook 'ham-mode-md2html-hook 'gmm/edit-server-save :append :local)
  (add-hook 'kill-buffer-hook 'gmm/-kill-mirror nil :local))

(defun gmm/-kill-mirror ()
  "Kill this buffer's sister (mirror or mirrored)."
  (message "Buffer %s is being killed." (current-buffer))
  (unless (and (boundp 'is-killing-mirrors)
               is-killing-mirrors) ;;Avoid recursion
    (let ((is-killing-mirrors t))
      (message "	It's going to kill its sister.")
      (when (and (stringp gmm/-mirrored-file)
                 (file-readable-p gmm/-mirrored-file)
                 (get-file-buffer gmm/-mirrored-file))
        (message "	The sister is %s." (get-file-buffer gmm/-mirrored-file))
        (kill-buffer (get-file-buffer gmm/-mirrored-file)))
      (when (and (buffer-live-p gmm/-mirror-buffer))
        (message "	The sister is %s."  gmm/-mirror-buffer)
        (kill-buffer gmm/-mirror-buffer)))))

(define-key gmail-message-client-mode-map (kbd "C-x #")   'gmm/edit-server-done)
(define-key gmail-message-client-mode-map (kbd "C-c C-c") 'gmm/edit-server-done)
(define-key gmail-message-client-mode-map (kbd "C-x C-c") 'gmm/edit-server-abort)

(defun gmm/edit-server-save (&optional ignore)
  "Save the edit-server-buffer, used as an after-save-hook.
Doesn't actually save this buffer"
  (interactive)
  (with-current-buffer gmm/-mirror-buffer
    (let ((inhibit-read-only t))
      (with-no-warnings
        (edit-server-save)))))

(defun gmm/edit-server-done ()
  "Call \"done\" on the edit-server buffer.
Ends up killing current buffer."
  (interactive)
  (save-buffer)
  (with-current-buffer gmm/-mirror-buffer
    (let ((inhibit-read-only t))
      (with-no-warnings
        (edit-server-done)))))

(defun gmm/edit-server-abort ()
  "Call \"abort\" on the edit-server buffer.
Ends up killing current buffer."
  (interactive)
  (with-current-buffer gmm/-mirror-buffer
    (let ((inhibit-read-only t))
      (with-no-warnings
        (edit-server-abort)))))

(defadvice edit-server-edit-mode
  (after gmm/-after-edit-server-edit-mode-advice () activate)
  "Makes sure the gmail-message-mode buffer receives focus.
So the user doesn't accicentally edit the edit-server buffer."
  (when gmm/-mirrored-file
    (message
     "Switched from edit-server-buffer (%s) to the gmail-mode buffer %s"
     (buffer-name) (switch-to-buffer (get-file-buffer gmm/-mirrored-file)))
    ;; Ensure this buffer only displays in one window.
    (mapc 'delete-window (cdr (get-buffer-window-list)))))

(defun gmm/-generate-temp-file-name ()
  (let ((name (replace-regexp-in-string
               "[^[:alnum:]-]" "_" (buffer-name)))
        file)
    (while (or (null file) (file-exists-p file))
      (setq file
            (format "%s%s-%s-%05d.gmm" temporary-file-directory
                    name "gmm-mirror" (random 100000))))
    file))

(defun gmm/-reflect-mirrored-file ()
  "Make current buffer reflect file given by `gmm/-mirrored-file'"
  (erase-buffer)
  (insert-file-contents gmm/-mirrored-file))

(defvar gmm/-end-regexp
  "<br *clear=\"all\">\\|<div><div *class=\"gmail_extra\">\\|<div *class=\"gmail_extra\">"
  "Regexp defining where a message ends and signature or quote starts.")

(defvar gmm/signature-map
  (let ((map (make-sparse-keymap)))
    (mapc
     (lambda (x) (define-key map x 'gmm/-expand-end))
     '([down-mouse-1]
       [remap self-insert-command]
       "\C-j"
       "\C-i"
       [return]
       [tab]))
    map)
  "Keymap used on the \"...\" button which hides the signature.")

(defcustom gmm/signature-properties
  `(display ,(propertize "..." 'face 'custom-button)
            intangible t
            pointer arrow
            mouse-face mode-line-highlight
            keymap ,gmm/signature-map)
  "Property list to use on the signature.

Does not affect the final e-mail. This is just used to hide
useless stuff from the user."
  :type '(repeat symbol (choice symbol string))
  :group 'gmail-message-mode
  :package-version '(gmail-message-mode . "1.0.1"))

(defun gmm/-expand-end ()
  "Expand the ending of the message, if it was collapsed."
  (interactive)
  (let ((inhibit-read-only t))
    (when (remove-text-properties
           (point-min) (point-max)
           gmm/signature-properties)
      (message "Signature and quotes expanded, see `%s' to disable hiding."
               'gmm/signature-properties))))

(defun gmm/-propertize-buffer ()
  "Add some text properties to the buffer, like coloring the signature."
  (goto-char (point-min))
  (when (search-forward-regexp gmm/-end-regexp nil :noerror)
    (add-text-properties (match-beginning 0) (point-max)
                         gmm/signature-properties)
    (message "Hiding garbage at the end. See `%s' to disable this"
             'gmm/signature-properties)))

(define-key gmail-message-mode-map (kbd "C-c C-z") 'gmm/save-finish-suspend)
(define-key gmail-message-mode-map (kbd "C-c C-s") 'gmm/save-finish-suspend)
(define-key gmail-message-mode-map (kbd "C-c C-c") 'server-edit)

;;;###autoload
(defun gmm/set-amlist (&optional sym val)
  "Reset the auto-mode-alist."
  (when sym
    (set-default sym val))
  (mapc
   (lambda (x) (add-to-list 'auto-mode-alist (cons x 'gmail-message-mode)))
   gmm/auto-mode-list))
;;;###autoload
(progn
  (eval-after-load 'edit-server
    '(add-to-list 'edit-server-url-major-mode-alist
                  '("\\(mail\\|inbox\\)\\.google\\." . gmail-message-edit-server-mode)))
  (mapc
   (lambda (x) (add-to-list 'auto-mode-alist (cons x 'gmail-message-mode)))
   gmm/auto-mode-list))

(provide 'gmail-message-mode)
;;; gmail-message-mode.el ends here.
