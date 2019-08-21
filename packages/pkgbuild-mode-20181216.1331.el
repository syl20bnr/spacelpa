;;; pkgbuild-mode.el --- Interface to the ArchLinux package manager

;; Copyright (C) 2005-2018 Juergen Hoetzel
;;
;; Author: Juergen Hoetzel <juergen@hoetzel.info>
;; Maintainer: Juergen Hoetzel <juergen@hoetzel.info>
;; URL: https://github.com/juergenhoetzel/pkgbuild-mode
;; Package-Version: 20181216.1331
;; Package-Requires: ((emacs "25.1"))
;; Version: 1.0-snapshot
;; Keywords: languages

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; TODO
;; - menu
;; - namcap/devtools integration
;; - use auto-insert

;;; Commentary:

;; This package provides an interface to the ArchLinux package manager.

;; Put this in your .emacs file to enable autoloading of pkgbuild-mode
;; and auto-recognition of "PKGBUILD" files:
;;
;;  (autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
;;  (setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode))
;;                                auto-mode-alist))

;;; Changelog:

;; 0.14
;; Support for source_${arch} and md5sums_${arch} (Thanks kolewu)

;; 0.13
;; Fix PKGBUILD template: Use $srcdir (Thanks amagura)

;; 0.12
;; pkgbuild-tar: Use "makepkg --source" instead of using a custom tar command
;; pkgbuild-tar: Use unique output buffers

;; 0.11
;; Support Sources renaming: https://wiki.archlinux.org/index.php/PKGBUILD#source
;; Use directory name as default pkgname
;; Support Tramp (PKGBUILDs on remote machines)

;; 0.10
;; made the calculation of sums generic (use makepkg.conf setting)

;; 0.9
;;    fixed `pkgbuild-tar' (empty directory name: thanks Stefan Husmann)
;;    new custom variable: pkgbuild-template
;;    code cleanup

;; 0.8
;;    added `pkgbuild-shell-command' and
;;      `pkgbuild-shell-command-to-string' (required to always use
;;      "/bin/bash" when calling shell functions, which create new
;;      buffers)


;; 0.7 make shell-file-name buffer-local set to "/bin/bash" (required
;; to parse PKGBUILDs)


;; 0.6
;;    New interactive function pkgbuild-etags (C-c C-e)
;;      create tags table for all PKGBUILDs in your source tree, so you
;;      can search PKGBUILDs by pkgname. Customize your tags-table-list
;;      to include the TAGS file in your source tree.
;;    changed default  makepkg-command (disabled ANSI colors in emacs TERM)
;;    set default indentation to 2


;; 0.5
;;    New interactive function pkgbuild-browse-url to visit project's website (C-c C-u).
;;      Customize your browse-url-browser-function
;;    emacs 22 (cvs snapshot) compatibility: ensure makepkg buffer is not read-only


;; 0.4
;;    handle source parse errors when updating md5sums and opening PKGBUILDs
;;    only update md5sums if all sources are available
;;    code cleanup
;;    highlight sources not available when trying to update md5sums and opening PKGBUILDs (this does not work when globbing returns multiple filenames)


;; 0.3
;;   Update md5sums line when saving PKGBUILD
;;     (Can be disabled via custom variable [pkgbuild-update-md5sums-on-save])
;;   New interactive function pkgbuild-tar to create Source Tarball (C-c C-a)
;;     (Usefull for AUR uploads)
;;   Insert warn-messages in md5sums line when source files are not present
;;   Several bug fixes

;;; Code


(require 'cl)
(require 'sh-script)
(require 'advice)
(require 'compile)

(defconst pkgbuild-mode-version "0.11" "Version of `pkgbuild-mode'.")

(defconst pkgbuild-mode-menu
  (purecopy '("PKGBUILD"
              ["Update sums" pkgbuild-update-sums-line t]
              ["Browse url" pkgbuild-browse-url t]
              ["Increase release tag"    pkgbuild-increase-release-tag t]
              "---"
              ("Build package"
               ["Build tarball"       pkgbuild-tar                t]
               ["Build binary package"    pkgbuild-makepkg             t])
              "---"
              ["Creates TAGS file"         pkgbuild-etags       t]
              "---"
              ["About pkgbuild-mode"         pkgbuild-about-pkgbuild-mode       t]
              )))

;; Local variables

(defgroup pkgbuild nil
  "pkgbuild mode (ArchLinux Packages)."
  :prefix "pkgbuild-"
  :group 'languages)

(defcustom pkgbuild-template
"# Maintainer: %s <%s>
pkgname=%s
pkgver=VERSION
pkgrel=1
epoch=
pkgdesc=\"\"
arch=('i686' 'x86_64')
url=\"\"
license=('GPL')
groups=()
depends=()
makedepends=()
checkdepends=()
optdepends=()
provides=()
conflicts=()
replaces=()
backup=()
options=()
install=
changelog=
source=($pkgname-$pkgver.tar.gz
        $pkgname-$pkgver.patch)
noextract=()
md5sums=()

prepare() {
  cd \"$srcdir/$pkgname-$pkgver\"

  patch -p1 -i \"$srcdir/$pkgname-$pkgver.patch\"
}

build() {
  cd \"$srcdir/$pkgname-$pkgver\"

  ./configure --prefix=/usr
  make
}

check() {
  cd \"$srcdir/$pkgname-$pkgver\"

  make -k check
}

package() {
  cd \"$srcdir/$pkgname-$pkgver\"

  make DESTDIR=\"$pkgdir/\" install
}

# vim:set ts=2 sw=2 et:
"
  "Template for new PKGBUILDs"
  :type 'string
  :group 'pkgbuild)

(defcustom pkgbuild-etags-command "find %s -name PKGBUILD|xargs etags.emacs -o %s --language=none --regex='/pkgname=\\([^ \t]+\\)/\\1/'"
  "pkgbuild-etags needs to call the find and the etags program. %s is
the placeholder for the toplevel directory and tagsfile"
  :type 'string
  :group 'pkgbuild)

(defcustom pkgbuild-initialize t
  "Automatically add default headings to new pkgbuild files."
  :type 'boolean
  :group 'pkgbuild)

(defcustom pkgbuild-update-sums-on-save t
  "*Non-nil means buffer-safe will call a hook to update the sums line."
  :type 'boolean
  :group 'pkgbuild)

(defcustom pkgbuild-read-makepkg-command t
  "*Non-nil means \\[pkgbuild-makepkg] reads the makepkg command to use.
Otherwise, \\[pkgbuild-makepkg] just uses the value of `pkgbuild-makepkg-command'."
  :type 'boolean
  :group 'pkgbuild)

(defcustom pkgbuild-read-tar-command t
  "*Non-nil means \\[pkgbuild-tar] reads the tar command to use."
  :type 'boolean
  :group 'pkgbuild)

(defcustom pkgbuild-makepkg-command "makepkg -m -f "
  "Command to create an ArchLinux package."
  :type 'string
  :group 'pkgbuild)

(defcustom pkgbuild-user-full-name user-full-name
  "*Full name of the user.
This is used in the Maintainer tag. It defaults to the
value of `user-full-name'."
  :type 'string
  :group 'pkgbuild)

(defcustom pkgbuild-user-mail-address user-mail-address
  "*Email address of the user.
This is used in the Maintainer tag. It defaults to the
value of `user-mail-address'."
  :type 'string
  :group 'pkgbuild)

(defcustom pkgbuild-source-directory-locations ".:src:/var/cache/pacman/src"
  "search path for PKGBUILD source files"
  :type 'string
  :group 'pkgbuild)

(defcustom pkgbuild-sums-command "makepkg -g 2>/dev/null"
  "shell command to generate *sums lines"
  :type 'string
  :group 'pkgbuild)

(defcustom pkgbuild-ask-about-save t
  "*Non-nil means \\[pkgbuild-makepkg] asks which buffers to save before starting packaging.
Otherwise, it saves all modified buffers without asking."
  :type 'boolean
  :group 'pkgbuild)

(defconst pkgbuild-bash-error-line-re
  "PKGBUILD:[ \t]+line[ \t]\\([0-9]+\\):[ \t]"
  "Regular expression that describes errors.")

(defvar pkgbuild-mode-map nil    ; Create a mode-specific keymap.
  "Keymap for pkgbuild mode.")

(defface pkgbuild-error-face '((t (:underline "red")))
  "Face for PKGBUILD errors."
  :group 'pkgbuild)

(defvar pkgbuild-makepkg-history nil)

(defvar pkgbuild-in-hook-recursion nil) ;avoid recursion

(unless pkgbuild-mode-map               ; Do not change the keymap if it is already set up.
  (setq pkgbuild-mode-map (make-sparse-keymap))
  (define-key pkgbuild-mode-map "\C-c\C-r"  'pkgbuild-increase-release-tag)
  (define-key pkgbuild-mode-map "\C-c\C-b" 'pkgbuild-makepkg)
  (define-key pkgbuild-mode-map "\C-c\C-a" 'pkgbuild-tar)
  (define-key pkgbuild-mode-map "\C-c\C-u" 'pkgbuild-browse-url)
  (define-key pkgbuild-mode-map "\C-c\C-m" 'pkgbuild-update-sums-line)
  (define-key pkgbuild-mode-map "\C-c\C-e" 'pkgbuild-etags))

(defun pkgbuild-trim-right (str)        ;Helper function
  "Trim whitespace from end of the string"
  (if (string-match "[ \f\t\n\r\v]+$" str -1)
      (pkgbuild-trim-right (substring str 0 -1))
    str))

(defun pkgbuild-source-points()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp "^\\s-*source=(\\([^()]*\\))" (point-max) t)
        (let ((l (list (match-beginning 1) (match-end 1)))
              (end (match-end 1)))
          (goto-char (match-beginning 1))
          (while (search-forward-regexp "\\(\\\\[ \f\t\n\r\v]\\|[ \f\t\n\r\v]\\)+" end t)
                  (setcdr (last l 2) (cons (match-beginning 0) (cdr (last l 2))))
                  (setcdr (last l 2) (cons (match-end 1) (cdr (last l 2)))))
          l)
      nil)))

(defun pkgbuild-source-locations()
  "find source regions"
  (delete-if (lambda (region) (= (car region) (cdr region))) (loop for item on (pkgbuild-source-points) by 'cddr collect (cons (car item) (cadr item)))))

(defun pkgbuild-source-check ()
  "highlight sources not available. Return true if all sources are available. This does not work if globbing returns multiple files"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (pkgbuild-delete-all-overlays)
    (if (search-forward-regexp "^\\s-*source[^=]*=(\\([^()]*\\))" (point-max) t)
        (let* ((all-available t)
	       (shell-file-name "/bin/bash")
               (sources (split-string (shell-command-to-string (format "bash -c '%s'" "source PKGBUILD 2>/dev/null && for source in ${source[@]};do echo $source|sed \"s|:.*://.*||g\"|sed \"s|^.*://.*/||g\";done"))))
              (source-locations (pkgbuild-source-locations)))
          (if (= (length sources) (length source-locations))
              (progn
                (loop for source in sources 
                      for source-location in source-locations
                      do (when (not (pkgbuild-file-available-p source (split-string pkgbuild-source-directory-locations ":")))
                           (setq all-available nil)
                           (pkgbuild-make-overlay (car source-location) (cdr source-location))))
                all-available)
            (progn
              (message "cannot verify sources: don't use globbing %d/%d" (length sources) (length source-locations))
              nil)))
      (progn
        (message "no source line found")
        nil))))

(defun pkgbuild-delete-all-overlays ()
  "Delete all the overlays used by pkgbuild-mode."
  (interactive)                         ;test
  (let ((l (overlays-in (point-min) (point-max))))
    (while (consp l)
      (progn
        (if (pkgbuild-overlay-p (car l))
            (delete-overlay (car l)))
        (setq l (cdr l))))))

(defun pkgbuild-overlay-p (o)
  "A predicate that return true iff O is an overlay used by pkgbuild-mode."
  (and (overlayp o) (overlay-get o 'pkgbuild-overlay)))

(defun pkgbuild-make-overlay (beg end)
  "Allocate an overlay to highlight. BEG and END specify the range in the buffer."
  (let ((pkgbuild-overlay (make-overlay beg end nil t nil)))
    (overlay-put pkgbuild-overlay 'face 'pkgbuild-error-face)
    (overlay-put pkgbuild-overlay 'pkgbuild-overlay t)
    pkgbuild-overlay))

(defun pkgbuild-file-available-p (filename locations)
  "Return t if file FILENAME exists LOCATIONS."
  (find-if
   (lambda (dir)
     (let* ((name-local (expand-file-name filename dir)))
       (file-readable-p
	(if (and (file-remote-p default-directory) (not (file-remote-p name-local)))
	    (with-parsed-tramp-file-name default-directory nil
	      (tramp-make-tramp-file-name method user domain host port name-local))
	  name-local))))
   locations))

(defun pkgbuild-sums-line ()
  "calculate *sums=() line in PKGBUILDs"
  (shell-command-to-string pkgbuild-sums-command))

(defun pkgbuild-update-sums-line ()
  "Update the sums line in a PKGBUILD."
  (interactive)
  (if (not (file-readable-p "PKGBUILD")) (error "Missing PKGBUILD")
    (if (not (pkgbuild-syntax-check)) (error "Syntax Error")
      (if (pkgbuild-source-check)       ;all sources available
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward "^[[:space:]]*\\\(md\\\|sha\\\)[[:digit:]]+sums\\\(_[^=]+\\\)?=([^()]*)[ \f\t\r\v]*\n?" (point-max) t) ;sum line exists
              (delete-region (match-beginning 0) (match-end 0)))
            (goto-char (point-max))
            (if (re-search-backward "^[[:space:]]*source\\\(_[^=]+\\\)?=([^()]*)" (point-min) t)
                (progn
                  (goto-char (match-end 0))
                  (insert "\n"))
              (error "Missing source line")
              (goto-char (point-max)))
            (insert (pkgbuild-trim-right (pkgbuild-sums-line))))))))

(defun pkgbuild-about-pkgbuild-mode (&optional arg)
  "About `pkgbuild-mode'."
  (interactive "p")
  (message
   (concat "pkgbuild-mode version "
           pkgbuild-mode-version
           " by Juergen Hoetzel, <juergen@hoetzel.info>")))

(defun pkgbuild-update-sums-line-hook ()
  "Update sum lines if the file was modified"
  (if (and pkgbuild-update-sums-on-save (not pkgbuild-in-hook-recursion))
      (progn
        (setq pkgbuild-in-hook-recursion t)
        (save-buffer)                   ;always save BUFFER 2 times so we get the correct sums in this hook
        (setq pkgbuild-in-hook-recursion nil)
        (pkgbuild-update-sums-line))))

(defun pkgbuild-initialize ()
  "Create a default pkgbuild if one does not exist or is empty."
  (interactive)
  (insert (format pkgbuild-template
		  pkgbuild-user-full-name
		  pkgbuild-user-mail-address
		  (or (substring (file-relative-name (file-name-directory buffer-file-name) "..") 0 -1)
		      NAME))))

(defun pkgbuild-process-check (buffer)
  "Check if BUFFER has a running process.
If so, give the user the choice of aborting the process or the current
command."
  (let ((process (get-buffer-process (get-buffer buffer))))
    (if (and process (eq (process-status process) 'run))
        (if (yes-or-no-p (concat "Process `" (process-name process)
                                 "' running.  Kill it? "))
            (delete-process process)
          (error "Cannot run two simultaneous processes ...")))))

(defun pkgbuild-makepkg (command)
  "Build this package."
  (interactive
   (if pkgbuild-read-makepkg-command
       (list (read-from-minibuffer "makepkg command: "
                                   (eval pkgbuild-makepkg-command)
                                   nil nil '(pkgbuild-makepkg-history . 1)))
     (list (eval pkgbuild-makepkg-command))))
  (save-some-buffers (not pkgbuild-ask-about-save) nil)
  (if (file-readable-p "PKGBUILD")
      (let ((pkgbuild-buffer-name (concat "*"  command " " buffer-file-name  "*")))
        (pkgbuild-process-check pkgbuild-buffer-name)
        (if (get-buffer pkgbuild-buffer-name)
            (kill-buffer pkgbuild-buffer-name))
        (get-buffer-create pkgbuild-buffer-name)
        (display-buffer pkgbuild-buffer-name)
	(with-current-buffer pkgbuild-buffer-name
	  (compilation-mode)
	  (toggle-read-only -1))
        (let ((process
               (start-file-process-shell-command "makepkg" pkgbuild-buffer-name
                                            command)))
          (set-process-filter process 'pkgbuild-command-filter)))
    (error "No PKGBUILD in current directory")))

(defun pkgbuild-command-filter (process string)
  "Filter to process normal output."
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (process-mark process))
      (comint-watch-for-password-prompt string)
      (insert-before-markers string)
      (set-marker (process-mark process) (point)))))

(defun pkgbuild-increase-release-tag ()
  "Increase the release tag by 1."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp "^pkgrel=[ \t]*\\([0-9]+\\)[ \t]*$" nil t)
        (let ((release (1+ (string-to-number (match-string 1)))))
          (setq release (int-to-string release))
          (replace-match (concat "pkgrel=" release))
          (message (concat "Release tag changed to " release ".")))
      (message "No Release tag found..."))))

(defun pkgbuild-syntax-check ()
  "evaluate PKGBUILD and search stderr for errors"
  (interactive)
  (let  ((shell-file-name "/bin/bash")
         (stderr-buffer (concat "*PKGBUILD(" (buffer-file-name) ") stderr*"))
	 (stdout-buffer (concat "*PKGBUILD(" (buffer-file-name) ") stdout*")))
    (if (get-buffer stderr-buffer) (kill-buffer stderr-buffer))
    (if (get-buffer stdout-buffer) (kill-buffer stdout-buffer))
    (if (not (zerop
              (cl-labels ((message (arg &rest args) nil)) ;Hack disable empty output
                (shell-command "bash -c 'source PKGBUILD'" stdout-buffer stderr-buffer))))
        (multiple-value-bind (err-p line) (pkgbuild-postprocess-stderr stderr-buffer)
          (if err-p
              (goto-line line))
          nil)
      t)))


(defun pkgbuild-postprocess-stderr (buf)        ;multiple values return
  "Find errors in BUF.If an error occurred return multiple values (t line), otherwise return multiple values (nil line).  BUF must exist."
  (let (line err-p)
    (with-current-buffer buf
      (goto-char (point-min))
      (if (re-search-forward pkgbuild-bash-error-line-re nil t)
          (progn
            (setq line (string-to-number (match-string 1)))
            ; (pkgbuild-highlight-line line) TODO
            (setq err-p t)))
      (values err-p line))))

(defun pkgbuild-tarball-files ()
  "Return a list of required files for the tarball package"
  (cons "PKGBUILD"
	(remove-if (lambda (x) (string-match "^\\(https?\\|ftp\\)://" x))
		   (split-string (shell-command-to-string
				  "bash -c 'source PKGBUILD 2>/dev/null && echo ${source[@]} $install'")))))

(defun pkgbuild-pkgname ()
  "Return package name"
  (shell-command-to-string
   "bash -c 'source PKGBUILD 2>/dev/null && echo -n ${pkgname}'"))

(defun pkgbuild-tar (command)
  "Build a tarball containing all required files to build the package."
  (interactive
   (list (read-from-minibuffer "tar command: "
			       "makepkg --source -f"
			       nil nil '(pkgbuild-tar-history . 1))))
  (let ((pkgbuild-buffer-name (generate-new-buffer "*tar*")))
    (save-some-buffers (not pkgbuild-ask-about-save) nil)
    (pkgbuild-process-check pkgbuild-buffer-name)
    (display-buffer pkgbuild-buffer-name)
    (with-current-buffer (get-buffer pkgbuild-buffer-name)
      (goto-char (point-max)))
    (let ((process
           (start-file-process-shell-command "tar" pkgbuild-buffer-name
                                        command)))
      (set-process-filter process 'pkgbuild-command-filter))))


(defun pkgbuild-browse-url ()
  "Visit URL (if defined in PKGBUILD)"
  (interactive)
  (let* ((shell-file-name "/bin/bash")
	 (url (shell-command-to-string (concat (buffer-string) "\nsource /dev/stdin >/dev/null 2>&1 && echo -n $url" ))))
    (if (string= url "")
        (message "No URL defined in PKGBUILD")
      (browse-url url))))

;;;###autoload
(define-derived-mode pkgbuild-mode shell-script-mode "PKGBUILD"
  "Major mode for editing PKGBUILD files. This is much like shell-script-mode mode.
 Turning on pkgbuild mode calls the value of the variable `pkgbuild-mode-hook'
with no args, if that value is non-nil."
  (require 'easymenu)
  (easy-menu-define pkgbuild-call-menu pkgbuild-mode-map
                    "Post menu for `pkgbuild-mode'." pkgbuild-mode-menu)
  (set (make-local-variable 'sh-basic-offset) 2) ;This is what judd uses
  (set (make-local-variable 'compile-command) pkgbuild-makepkg-command)
  (sh-set-shell "/bin/bash")
  (easy-menu-add pkgbuild-mode-menu)
  ;; This does not work because makepkg req. saved file
  (add-hook 'local-write-file-hooks 'pkgbuild-update-sums-line-hook nil t)
  (if (= (buffer-size) 0)
      (pkgbuild-initialize)
    (and (pkgbuild-syntax-check) (pkgbuild-source-check))))

(defadvice sh-must-be-shell-mode (around no-check-if-in-pkgbuild-mode activate)
  "Do not check for shell-mode if major mode is \\[pkgbuild-makepkg]"
  (if (not (eq major-mode 'pkgbuild-mode)) ;workaround for older shell-scrip-mode versions
      ad-do-it))

(defun pkgbuild-etags (toplevel-directory)
  "Create TAGS file by running `etags' recursively on the directory tree `pkgbuild-toplevel-directory'.
  The TAGS file is also immediately visited with `visit-tags-table'."
  (interactive "DToplevel directory: ")
  (let* ((etags-file (expand-file-name "TAGS" toplevel-directory))
	 (shell-file-name "/bin/bash")
	 (cmd (format pkgbuild-etags-command toplevel-directory etags-file)))
    (require 'etags)
    (message "Running etags to create TAGS file: %s" cmd)
    (shell-command cmd)
    (visit-tags-table etags-file)))

;;;###autoload
(add-to-list 'auto-mode-alist '("/PKGBUILD\\'" . pkgbuild-mode))

(provide 'pkgbuild-mode)

;;; pkgbuild-mode.el ends here
