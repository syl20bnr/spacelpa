;;; naquadah-theme.el --- A theme based on Tango color set
;; Package-Version: 20190225.1427

;; Copyright (C) 2011-2012 Free Software Foundation, Inc

;; Authors: Julien Danjou <julien@danjou.info>

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme naquadah
  "A theme based on Tango color set.")

;; We want the face to be created even if they do not exist.
(put 'naquadah 'theme-immediate t)

;; These colors are stolen from Tango.
(defvar naquadah-colors
  '((((class color) (min-colors 65535))
     (aluminium-1 . "#eeeeec")
     (aluminium-2 . "#d3d7cf")
     (aluminium-3 . "#babdb6")
     (aluminium-4 . "#888a85")
     (aluminium-5 . "#555753")
     (aluminium-6 . "#2e3436")
     (butter-1 . "#fce94f")
     (butter-2 . "#edd400")
     (butter-3 . "#c4a000")
     (orange-1 . "#fcaf3e")
     (orange-2 . "#f57900")
     (orange-3 . "#ce5c00")
     (chocolate-1 . "#e9b96e")
     (chocolate-2 . "#c17d11")
     (chocolate-3 . "#9f5902")
     (chameleon-1 . "#8ae234")
     (chameleon-2 . "#73d216")
     (chameleon-3 . "#4e9a06")
     (sky-blue-1 . "#729fcf")
     (sky-blue-2 . "#3465a4")
     (sky-blue-3 . "#204a87")
     (plum-1 . "#de7fa8")
     (plum-2 . "#b5507b")
     (plum-3 . "#9c3566")
     (scarlet-red-1 . "#ff2929")
     (scarlet-red-2 . "#dc1010")
     (scarlet-red-3 . "#b40000")
     (background . "#262B2C")
     (black . "#0c191C")
     (gradient-1 . "#729fcf")  ;; sky-blue-1
     (gradient-2 . "#8ae234")  ;; chameleon-1
     (gradient-3 . "#fce94f")  ;; butter-1
     (gradient-4 . "#de7fa8")  ;; plum-1
     (gradient-5 . "#e9b96e")  ;; chocolate-1
     (gradient-6 . "#fcaf3e")  ;; orange-1
     (gradient-7 . "#3465a4")  ;; sky-blue-2
     (gradient-8 . "#73d216")  ;; chameleon-2
     (gradient-9 . "#f57900")  ;; orange-2
     (gradient-10 . "#b5507b") ;; plum-2
     (gradient-11 . "#c17d11") ;; chocolate-2
     )
    (((class color) (min-colors 256))
     (aluminium-1 . "color-255")
     (aluminium-2 . "color-253")
     (aluminium-3 . "color-251")
     (aluminium-4 . "color-245")
     (aluminium-5 . "color-240")
     (aluminium-6 . "color-235")
     (butter-1 . "color-221")
     (butter-2 . "color-220")
     (butter-3 . "color-178")
     (orange-1 . "color-214")
     (orange-2 . "color-208")
     (orange-3 . "color-130")
     (chocolate-1 . "color-180")
     (chocolate-2 . "color-172")
     (chocolate-3 . "color-94")
     (chameleon-1 . "color-82")
     (chameleon-2 . "color-76")
     (chameleon-3 . "color-34")
     (sky-blue-1 . "color-117")
     (sky-blue-2 . "color-63")
     (sky-blue-3 . "color-24")
     (plum-1 . "color-176")
     (plum-2 . "color-96")
     (plum-3 . "color-54")
     (scarlet-red-1 . "color-196")
     (scarlet-red-2 . "color-160")
     (scarlet-red-3 . "color-124")
     (background . "color-234")
     (black . "color-16")
     (gradient-1 . "color-117")    ;; sky-blue-1
     (gradient-2 . "color-82")     ;; chameleon-1
     (gradient-3 . "color-221")    ;; butter-1
     (gradient-4 . "color-176")    ;; plum-1
     (gradient-5 . "color-180")    ;; chocolate-1
     (gradient-6 . "color-214")    ;; orange-1
     (gradient-7 . "color-63")     ;; sky-blue-2
     (gradient-8 . "color-76")     ;; chameleon-2
     (gradient-9 . "color-208")    ;; orange-2
     (gradient-10 . "color-96")    ;; plum-2
     (gradient-11 . "color-172")   ;; chocolate-2
     )
    (((class color) (min-colors 88))
     (aluminium-1 . "color-87")
     (aluminium-2 . "color-86")
     (aluminium-3 . "color-85")
     (aluminium-4 . "color-84")
     (aluminium-5 . "color-82")
     (aluminium-6 . "color-80")
     (butter-1 . "color-77")
     (butter-2 . "color-76")
     (butter-3 . "color-72")
     (orange-1 . "color-72")
     (orange-2 . "color-68")
     (orange-3 . "color-68")
     (chocolate-1 . "color-73")
     (chocolate-2 . "color-68")
     (chocolate-3 . "color-52")
     (chameleon-1 . "color-60")
     (chameleon-2 . "color-44")
     (chameleon-3 . "color-40")
     (sky-blue-1 . "color-43")
     (sky-blue-2 . "color-22")
     (sky-blue-3 . "color-22")
     (plum-1 . "color-54")
     (plum-2 . "color-37")
     (plum-3 . "color-33")
     (scarlet-red-1 . "color-64")
     (scarlet-red-2 . "color-64")
     (scarlet-red-3 . "color-48")
     (background . "color-80")
     (black . "color-16")
     (gradient-1 . "color-43")    ;; sky-blue-1
     (gradient-2 . "color-60")    ;; chameleon-1
     (gradient-3 . "color-77")    ;; butter-1
     (gradient-4 . "color-54")    ;; plum-1
     (gradient-5 . "color-73")    ;; chocolate-1
     (gradient-6 . "color-72")    ;; orange-1
     (gradient-7 . "color-22")    ;; sky-blue-2
     (gradient-8 . "color-44")    ;; chameleon-2
     (gradient-9 . "color-68")    ;; orange-2
     (gradient-10 . "color-37")   ;; plum-2
     (gradient-11 . "color-68")   ;; chocolate-2
     )
    (t
     (aluminium-1 . "white")
     (aluminium-2 . "white")
     (aluminium-3 . "white")
     (aluminium-4 . "white")
     (aluminium-5 . "black")
     (aluminium-6 . "black")
     (butter-1 . "yellow")
     (butter-2 . "yellow")
     (butter-3 . "yellow")
     (orange-1 . "yellow")
     (orange-2 . "yellow")
     (orange-3 . "yellow")
     (chocolate-1 . "yellow")
     (chocolate-2 . "yellow")
     (chocolate-3 . "yellow")
     (chameleon-1 . "green")
     (chameleon-2 . "green")
     (chameleon-3 . "green")
     (sky-blue-1 . "blue")
     (sky-blue-2 . "blue")
     (sky-blue-3 . "blue")
     (plum-1 . "magenta")
     (plum-2 . "magenta")
     (plum-3 . "magenta")
     (scarlet-red-1 . "red")
     (scarlet-red-2 . "red")
     (scarlet-red-3 . "red")
     (background . "black")
     (black . "black")
     (gradient-1 . "blue")    ;; sky-blue-1
     (gradient-2 . "green")     ;; chameleon-1
     (gradient-3 . "yellow")    ;; butter-1
     (gradient-4 . "yellow")    ;; plum-1
     (gradient-5 . "yellow")    ;; chocolate-1
     (gradient-6 . "yellow")    ;; orange-1
     (gradient-7 . "blue")     ;; sky-blue-2
     (gradient-8 . "green")     ;; chameleon-2
     (gradient-9 . "red")    ;; orange-2
     (gradient-10 . "yellow")    ;; plum-2
     (gradient-11 . "yellow")   ;; chocolate-2
     ))
  "The color values for each color name for a given
      condition.  The format is: ((condition) (key . value) (key
      . value) ...)")

(defun naquadah-get-colors (name)
  (cdr
   (assoc
    name
    (car naquadah-colors))))

(defun naquadah-simple-face-to-multiple (face)
  (let ((spec (car face))
        (lst (cadr face)))
    (list spec (mapcar
                (lambda (entry)
                  (let ((color-condition (car entry)))
                    (list color-condition
                          (naquadah-color-list-expand (cdr entry) lst))))
                naquadah-colors))))

(defun naquadah-color-list-expand (color-alist lst)
  (let ((result '()))
    (while (car lst)
      (let ((key (car lst))
            (val (cadr lst)))
        (if (memq key '(:foreground :background :color :overline :strike-through))
            (setq val (or (cdr (assq val color-alist)) val)))
        (if (listp val)
            (setq val (naquadah-color-list-expand entry val)))
        (setq result (append result `(,key ,val))))
      (setq lst (cddr lst)))
    result))

(defun naquadah-properties-get (lst prop)
  (cadr (assoc prop lst)))

(defun naquadah-apply-unspecified-properties (inherit-props props)
  (while (car inherit-props)
    (let ((i-key (car inherit-props)))
      (if (not (plist-member props i-key))
          (plist-put props i-key 'unspecified)))
    (setq inherit-props (cddr inherit-props))))

(defun naquadah-append-unspecified-properties (lst)
  (dolist (elt lst)
    (let ((key (car elt))
          (props (cadr elt)))
      (when (plist-member props :inherit)
        (let ((inherit-prop (plist-get props :inherit)))
          (if (listp inherit-prop)
              (dolist (i inherit-prop)
                (naquadah-apply-unspecified-properties (naquadah-properties-get lst i) props))
            (naquadah-apply-unspecified-properties (naquadah-properties-get lst inherit-prop) props))))))
  lst)

(defun naquadah-theme-set-faces (theme &rest args)
  (apply 'custom-theme-set-faces
         (append (list theme)
                 (mapcar 'naquadah-simple-face-to-multiple
                         (naquadah-append-unspecified-properties args)))))

(naquadah-theme-set-faces
 'naquadah
 '(default (:background background :foreground aluminium-1))
 '(shadow (:foreground aluminium-3))
 '(secondary-selection (:background sky-blue-3))
 '(cursor (:background scarlet-red-3))
 '(hl-line (:background aluminium-5))
 '(trailing-whitespace (:background scarlet-red-1))
 '(escape-glyph (:foreground chameleon-1))

 '(highlight (:background scarlet-red-2))
 '(fringe (:background black))
 '(mode-line (:foreground aluminium-1 :background black
                          :box (:line-width 1 :color aluminium-6)))
 '(mode-line-inactive (:foreground aluminium-5 :background "#1F2427"
                                   :box (:line-width 1 :color background)))
 '(mode-line-buffer-id (:weight bold :foreground orange-2))
 '(header-line (:foreground aluminium-1 :background black
                            :box (:line-width 1 :color aluminium-6)))
 '(region (:background black))
 '(link (:foreground sky-blue-1))
 '(link-visited (:inherit link :foreground plum-1))
 '(match (:weight bold :background chocolate-1 :foreground black))
 '(tooltip (:inherit variable-pitch :foreground aluminium-1 :background black))
 '(bold (:weight bold))
 '(italic (:italic t))
 '(warning (:weight bold :foreground orange-1 :italic t))
 '(error (:weight bold :foreground scarlet-red-1 :italic t))
 '(success (:weight bold :foreground chameleon-1))

 '(font-lock-builtin-face (:foreground sky-blue-1))
 '(font-lock-keyword-face (:inherit font-lock-builtin-face :weight bold))
 '(font-lock-comment-face (:inherit shadow :italic t))
 '(font-lock-comment-delimiter-face (:inherit font-lock-comment-face))
 '(font-lock-constant-face (:foreground chameleon-2))
 '(font-lock-negation-char-face (:foreground chameleon-2))
 '(font-lock-type-face (:foreground butter-1 :weight bold))
 '(font-lock-doc-face (:inherit shadow :italic t))
 '(font-lock-string-face (:foreground plum-1))
 '(font-lock-variable-name-face (:foreground scarlet-red-1))
 '(font-lock-warning-face (:inherit warning))
 '(font-lock-function-name-face (:foreground orange-1 :weight bold))

 '(auto-dim-other-buffers-face (:background "#1F2427"))

 '(comint-highlight-prompt ())

 '(isearch (:background orange-3 :foreground background))
 '(isearch-fail (:background scarlet-red-2))
 '(lazy-highlight (:background chocolate-1 :foreground background))

 '(show-paren-match-face (:background chameleon-3))
 '(show-paren-mismatch-face (:background plum-3))

 '(minibuffer-prompt (:foreground sky-blue-1 :weight bold))

 ;; '(widget-mouse-face ((t (:weight bold :foreground aluminium-1 :background scarlet-red-2))))
 ;; '(widget-field ((t (:foreground orange-1 :background "gray30"))))
 ;; '(widget-single-line-field ((t (:foreground orange-1 :background "gray30"))))

 '(custom-group-tag (:weight bold :foreground orange-2 :height 1.3))
 '(custom-variable-tag (:weight bold :foreground butter-2 :height 1.1))
 '(custom-face-tag (:weight bold :foreground butter-2 :height 1.1))
 '(custom-state (:foreground sky-blue-1))
 ;; '(custom-button  ((t :background "gray50" :foreground black
 ;; :box (:line-width 1 :style released-button))))
 ;; '(custom-variable-button ((t (:inherit custom-button))))
 ;; '(custom-button-mouse  ((t (:inherit custom-button :background "gray60"))))
 ;; '(custom-button-unraised  ((t (:background "gray50" :foreground "black"))))
 ;; '(custom-button-mouse-unraised  ((t (:inherit custom-button-unraised :background "gray60"))))
 ;; '(custom-button-pressed  ((t (:inherit custom-button :box (:style pressed-button)))))
 ;; '(custom-button-mouse-pressed-unraised  ((t (:inherit custom-button-unraised :background "gray60"))))
 '(custom-documentation (:inherit font-lock-comment-face))

 ;; Gnus
 '(gnus-cite-1 (:foreground gradient-1))
 '(gnus-cite-2 (:foreground gradient-2))
 '(gnus-cite-3 (:foreground gradient-3))
 '(gnus-cite-4 (:foreground gradient-4))
 '(gnus-cite-5 (:foreground gradient-5))
 '(gnus-cite-6 (:foreground gradient-6))
 '(gnus-cite-7 (:foreground gradient-7))
 '(gnus-cite-8 (:foreground gradient-8))
 '(gnus-cite-9 (:foreground gradient-9))
 '(gnus-cite-10 (:foreground gradient-10))
 '(gnus-cite-11 (:foreground gradient-11))
 '(gnus-header-name (:weight bold :foreground sky-blue-1))
 '(gnus-header-from (:weight bold))
 '(gnus-header-subject (:foreground butter-1))
 '(gnus-header-content (:italic t :foreground aluminium-2))
 '(gnus-header-newsgroups (:inherit gnus-header-from))
 '(gnus-signature (:inherit font-lock-comment-face))
 '(gnus-summary-cancelled (:foreground plum-1 :strike-through plum-3))

 '(gnus-summary-high-ancient (:inherit gnus-summary-high-read))
 '(gnus-summary-normal-ancient (:inherit gnus-summary-normal-read))
 '(gnus-summary-low-ancient (:inherit gnus-summary-low-unread))

 '(gnus-summary-high-read (:inherit gnus-summary-normal-read :background sky-blue-3
                                    :box (:line-width 1 :color sky-blue-3)))
 '(gnus-summary-normal-read (:foreground aluminium-1 :italic t))
 '(gnus-summary-low-read (:inherit gnus-summary-normal-read))

 '(gnus-summary-high-ticked (:inherit gnus-summary-normal-ticked))
 '(gnus-summary-normal-ticked (:background scarlet-red-3
                                           :box (:line-width 1 :color scarlet-red-2)))
 '(gnus-summary-low-ticked (:inherit gnus-summary-normal-ticked))

 '(gnus-summary-high-unread (:inherit gnus-summary-normal-unread
                                      :background sky-blue-2
                                      :box (:line-width 1 :color sky-blue-2)))
 '(gnus-summary-normal-unread (:foreground aluminium-1))
 '(gnus-summary-low-unread (:foreground aluminium-1))

 '(gnus-summary-selected (:background butter-3 :foreground black))
 '(gnus-button (:weight bold))
 '(spam (:foreground orange-2 :strike-through orange-3))

 ;; Message
 '(message-header-newsgroups (:inherit gnus-header-to))
 '(message-header-name (:inherit gnus-header-name))
 '(message-header-to (:inherit gnus-header-to))
 '(message-header-other (:inherit gnus-header-content))
 '(message-header-subject (:inherit gnus-header-subject))
 '(message-header-cc (:foreground aluminium-2))
 '(message-header-xheader (:foreground aluminium-4))
 '(message-separator (:foreground sky-blue-3))
 '(message-mml (:foreground chameleon-1))

 ;; org-mode
 '(org-level-1 (:weight bold :foreground gradient-1 :height 1.3))
 '(org-level-2 (:weight bold :foreground gradient-2 :height 1.2))
 '(org-level-3 (:weight bold :foreground gradient-3 :height 1.1))
 '(org-level-4 (:weight bold :foreground gradient-4))
 '(org-level-5 (:weight bold :foreground gradient-5))
 '(org-level-6 (:weight bold :foreground gradient-6))
 '(org-level-7 (:weight bold :foreground gradient-7))
 '(org-level-8 (:weight bold :foreground gradient-8))

 '(org-column ())
 '(org-mode-line-clock (nil))
 '(org-mode-line-clock-overrun (:foreground scarlet-red-1))
 '(org-document-title (:weight bold :foreground sky-blue-1 :height 1.4))
 '(org-document-info (:foreground sky-blue-1 :italic t))
 '(org-todo (:weight bold :foreground scarlet-red-2))
 '(org-done (:weight bold :foreground chameleon-3))
 '(org-hide (:foreground background))
 '(org-scheduled (:foreground chameleon-2))
 '(org-scheduled-previously (:foreground orange-2))
 '(org-scheduled-today (:foreground chameleon-1))
 '(org-date (:foreground chocolate-1))
 '(org-special-keyword (:foreground scarlet-red-1 :weight bold))
 '(org-agenda-done ())
 '(org-time-grid (:inherit shadow))
 '(org-agenda-date (:foreground butter-1 :height 1.2))
 '(org-agenda-date-today (:inherit org-agenda-date :foreground butter-2 :weight bold :height 1.3))
 '(org-agenda-date-tc (:inherit org-agenda-date :foreground butter-3))
 '(org-agenda-date-weekend (:inherit org-agenda-date :foreground scarlet-red-1 :weight bold))

 '(org-habit-clear-future-face (:background sky-blue-3))
 '(org-habit-clear-face (:background sky-blue-2))
 '(org-habit-ready-future-face (:background chameleon-3))
 '(org-habit-ready-face (:background chameleon-2 :foreground black))
 '(org-habit-alert-ready-future-face (:background orange-3))
 '(org-habit-overdue-face (:background scarlet-red-3))
 '(org-habit-overdue-future-face (:background scarlet-red-3))

 ;; egocentric-mode
 '(egocentric-face (:foreground scarlet-red-1 :weight bold))

 ;; erc
 '(erc-direct-msg-face (:inherit egocentric-face))
 '(erc-header-line (:inherit header-line))
 '(erc-input-face (:inherit shadow))
 '(erc-my-nick-face (:inherit egocentric-face))
 '(erc-notice-face (:foreground sky-blue-1))
 '(erc-prompt-face (:background black :foreground aluminium-1 :weight bold))
 '(erc-timestamp-face (:foreground aluminium-2 :weight bold))
 '(erc-pal-face (:foreground chameleon-1 :weight bold))
 '(erc-keyword-face (:foreground orange-1))
 '(erc-fool-face (:inherit shadow))
 '(erc-current-nick-face (:inherit egocentric-face))
 '(erc-inverse-face (:foreground black :background aluminium-2))
 '(fg:erc-color-face0 (:foreground aluminium-1))
 '(fg:erc-color-face1 (:foreground black))
 '(fg:erc-color-face2 (:foreground sky-blue-1))
 '(fg:erc-color-face3 (:foreground chameleon-2))
 '(fg:erc-color-face4 (:foreground scarlet-red-2))
 '(fg:erc-color-face5 (:foreground chocolate-2))
 '(fg:erc-color-face6 (:foreground plum-2))
 '(fg:erc-color-face7 (:foreground orange-2))
 '(fg:erc-color-face8 (:foreground butter-2))
 '(fg:erc-color-face8 (:foreground chameleon-3))
 '(fg:erc-color-face10 (:foreground sky-blue-2))
 '(fg:erc-color-face11 (:foreground "cyan"))
 '(fg:erc-color-face12 (:foreground sky-blue-3))
 '(fg:erc-color-face13 (:foreground plum-3))
 '(fg:erc-color-face14 (:foreground aluminium-2))
 '(fg:erc-color-face15 (:foreground aluminium-4))
 '(bg:erc-color-face0 (:background aluminium-1))
 '(bg:erc-color-face1 (:background black))
 '(bg:erc-color-face2 (:background sky-blue-1))
 '(bg:erc-color-face3 (:background chameleon-2))
 '(bg:erc-color-face4 (:background scarlet-red-2))
 '(bg:erc-color-face5 (:background chocolate-2))
 '(bg:erc-color-face6 (:background plum-2))
 '(bg:erc-color-face7 (:background orange-2))
 '(bg:erc-color-face8 (:background butter-2))
 '(bg:erc-color-face8 (:background chameleon-3))
 '(bg:erc-color-face10 (:background sky-blue-2))
 '(bg:erc-color-face11 (:background "cyan"))
 '(bg:erc-color-face12 (:background sky-blue-3))
 '(bg:erc-color-face13 (:background plum-3))
 '(bg:erc-color-face14 (:background aluminium-2))
 '(bg:erc-color-face15 (:background aluminium-4))


 '(which-func (:foreground sky-blue-1))

 '(dired-directory (:foreground sky-blue-1))
 '(dired-symlink (:weight bold :foreground "cyan"))
 '(dired-marked (:weight bold :foreground butter-1))

 '(mm-uu-extract (:background aluminium-6))

 ;; diff-mode
 '(diff-added (:foreground chameleon-2))
 '(diff-changed (:foreground orange-1))
 '(diff-removed (:foreground scarlet-red-1))
 '(diff-hunk-header (:weight bold))
 '(diff-function (:foreground orange-1))
 '(diff-header (:background aluminium-6))
 '(diff-file-header (:foreground aluminium-1))

 ;; ediff-mode
 '(ediff-even-diff-A (:inherit diff-header))
 '(ediff-odd-diff-A (:background aluminium-5))
 '(ediff-even-diff-B (:inherit diff-header))
 '(ediff-odd-diff-B (:background aluminium-5))
 '(ediff-current-diff-A (:background "#553333"))
 '(ediff-current-diff-B (:background "#335533"))
 '(ediff-fine-diff-A (:background "#723030"))
 '(ediff-fine-diff-B (:background "#307030"))

 ;; smerge
 '(smerge-mine (:background "#723030"))
 '(smerge-refined-removed (:background scarlet-red-2))
 '(smerge-other (:background "#335533"))
 '(smerge-refined-added (:background "#307030"))

 ;; magit
 '(magit-section-heading (:foreground orange-1 :weight bold))
 '(magit-section-highlight (:background aluminium-6))
 '(magit-hash (:foreground scarlet-red-1))
 '(magit-branch-current (:foreground sky-blue-1))
 '(magit-branch-local (:foreground sky-blue-1))
 '(magit-branch-remote (:foreground chameleon-2))
 '(magit-tag (:background plum-3 :box (:color plum-2) :foreground aluminium-2))
 '(magit-diff-file-heading-highlight (:weight bold))

 '(magit-diff-hunk-heading (:inherit diff-file-header))
 '(magit-diff-added (:inherit diff-added))
 '(magit-diff-removed (:inherit diff-removed))
 '(magit-diffstat-added (:inherit diff-added))
 '(magit-diffstat-removed (:inherit diff-removed))

 '(magit-diff-hunk-heading-highlight (:inherit diff-file-header :background aluminium-5))
 '(magit-diff-added-highlight (:inherit diff-added :background aluminium-6))
 '(magit-diff-removed-highlight (:inherit diff-removed :background aluminium-6))
 '(magit-diff-context-highlight (:background aluminium-6))

 '(magit-log-graph (:foreground aluminium-2))

 ;; git-commit-mode
 '(git-commit-summary-face (:weight bold))
 '(git-commit-branch-face (:foreground orange-2 :weight bold))
 '(git-commit-nonempty-second-line-face (:foreground scarlet-red-2))
 '(git-commit-comment-face (:inherit font-lock-comment-face))
 '(git-commit-known-pseudo-header-face (:inherit gnus-header-name-face))
 '(git-commit-pseudo-header-face (:inherit gnus-header-content))

 ;; makefile-mode
 '(makefile-space (:background plum-3))

 ;; mmm-mode
 '(mmm-default-submode-face (:background aluminium-6))

 ;; rainbow-delimiters
 '(rainbow-delimiters-depth-1-face (:foreground gradient-1))
 '(rainbow-delimiters-depth-2-face (:foreground gradient-2))
 '(rainbow-delimiters-depth-3-face (:foreground gradient-3))
 '(rainbow-delimiters-depth-4-face (:foreground gradient-4))
 '(rainbow-delimiters-depth-5-face (:foreground gradient-5))
 '(rainbow-delimiters-depth-6-face (:foreground gradient-6))
 '(rainbow-delimiters-depth-7-face (:foreground gradient-7))
 '(rainbow-delimiters-depth-8-face (:foreground gradient-8))
 '(rainbow-delimiters-depth-9-face (:foreground gradient-9))
 '(rainbow-delimiters-depth-10-face (:foreground gradient-10))
 '(rainbow-delimiters-depth-11-face (:foreground gradient-11))
 '(rainbow-delimiters-depth-12-face (:foreground gradient-1))
 '(rainbow-delimiters-unmatched-face (:foreground black :background butter-3))

 ;; rst-mode
 '(rst-level-1 (:foreground gradient-1 :background aluminium-6 :height 1.3))
 '(rst-level-2 (:foreground gradient-2 :background aluminium-6 :height 1.2))
 '(rst-level-3 (:foreground gradient-3 :background aluminium-6 :height 1.1))
 '(rst-level-4 (:foreground gradient-4 :background aluminium-6))
 '(rst-level-5 (:foreground gradient-5 :background aluminium-6))
 '(rst-level-6 (:foreground gradient-6 :background aluminium-6))

 ;; term-mode
 '(term-color-black (:foreground black :background black))
 '(term-color-red (:foreground scarlet-red-1 :background scarlet-red-1))
 '(term-color-green (:foreground chameleon-1 :background chameleon-1))
 '(term-color-yellow (:foreground butter-1 :background butter-1))
 '(term-color-blue (:foreground sky-blue-1 :background sky-blue-1))
 '(term-color-magenta (:foreground plum-1 :background plum-1))
 '(term-color-cyan (:foreground "cyan3" :background "cyan3"))
 '(term-color-white (:foreground aluminium-1 :background aluminium-1))

 ;; idle-highlight
 '(idle-highlight (:foreground aluminium-1 :background scarlet-red-3))

 ;; doc-mode
 '(doc-title-1-face (:foreground gradient-1 :weight bold :height 1.3 :inherit variable-pitch))
 '(doc-title-2-face (:foreground gradient-2 :weight bold :height 1.2 :inherit variable-pitch))
 '(doc-title-3-face (:foreground gradient-3 :weight bold :height 1.1 :inherit variable-pitch))
 '(doc-title-4-face (:foreground gradient-4 :weight bold :inherit variable-pitch))

 ;; markup-faces
 '(markup-gen-face (:foreground sky-blue-1))
 '(markup-title-0-face (:foreground gradient-1 :weight bold :height 1.3 :inherit variable-pitch))
 '(markup-title-1-face (:foreground gradient-2 :weight bold :height 1.2 :inherit variable-pitch))
 '(markup-title-2-face (:foreground gradient-3 :weight bold :height 1.1 :inherit variable-pitch))
 '(markup-title-3-face (:foreground gradient-4 :weight bold :inherit variable-pitch))
 '(markup-title-4-face (:foreground gradient-5 :weight bold :inherit variable-pitch))
 '(markup-title-5-face (:foreground gradient-6 :weight bold :inherit variable-pitch))
 '(markup-emphasis-face (:slant italic))
 '(markup-strong-face (:weight bold))
 '(markup-code-face (:inherit fixed-pitch))
 '(markup-verbatime-face (:background aluminium-6))
 '(markup-meta-face (:foreground aluminium-3))
 '(markup-meta-hide-face (:foreground aluminium-5))
 '(markup-reference-face (:foreground sky-blue-1))
 '(markup-list-face (:background aluminium-6 :foreground orange-2))
 '(markup-secondary-text-face (:foreground scarlet-red-1 :height 0.8))
 '(markup-replacement-face (:foreground plum-2))
 '(markup-complex-replacement-face (:box (:line-width 2 :color plum-2)
                                         :foreground "white" :background plum-3))
 '(markup-verbatim-face (:inherit fixed-pitch))

 ;; flymake
 '(flymake-errline (:underline (:style wave :color scarlet-red-1)))
 '(flymake-warnline (:underline (:style wave :color orange-2)))

 ;; flycheck
 '(flycheck-error (:underline (:style wave :color scarlet-red-1)))
 '(flycheck-warning (:underline (:style wave :color orange-2)))
 '(flycheck-info (:underline (:style wave :color chameleon-1)))

 ;; flycheck-color-mode-line-error-face
 '(flycheck-color-mode-line-error-face (:inherit error :italic nil))
 '(flycheck-color-mode-line-warning-face (:inherit warning :italic nil))

 ;; flyspell
 '(flyspell-incorrect (:underline (:style wave :color scarlet-red-1)))
 '(flyspell-duplicate (:underline (:style wave :color orange-2)))

 ;; company
 '(company-tooltip (:background aluminium-4))
 '(company-tooltip-selection (:background orange-2))
 '(company-tooltip-common (:foreground scarlet-red-3))
 '(company-tooltip-annotation (:background scarlet-red-3))
 '(company-scrollbar-fg (:background scarlet-red-3))
 '(company-scrollbar-bg (:background black))
 '(company-preview (:background sky-blue-1))
 '(company-preview-common (:inherit company-preview :foreground scarlet-red-3))
 '(company-preview-search (:inherit company-preview :background sky-blue-1))
 '(company-echo-common (:background scarlet-red-3))

 ;; git-gutter
 '(git-gutter:modified (:foreground orange-1))
 '(git-gutter:added (:foreground chameleon-1))
 '(git-gutter:deleted (:foreground scarlet-red-1))
 '(git-gutter:unchanged (:foreground butter-1))

 '(ido-first-match (:foreground orange-1 :weight bold))
 '(ido-only-match (:foreground orange-1 :weight bold))

 ;; helm
 '(helm-source-header (:inherit success))
 '(helm-visible-mark (:inherit region :foreground aluminium-3))
 '(helm-header (:inherit mode-line))
 '(helm-candidate-number (:inherit highlight))
 '(helm-selection (:inherit secondary-selection))
 '(helm-match (:inherit warning))
 '(helm-separator (:inherit message-separator))
 '(helm-action (:foreground sky-blue-1))
 '(helm-ff-directory (:foreground sky-blue-1 :background nil :underline nil))
 '(helm-ff-file (:inherit link :foreground plum-1 :underline nil))
 '(helm-grep-file (:inherit link :foreground plum-1 :underline t))

 ;; jabber
 '(jabber-activity-face (:foreground scarlet-red-1 :weight bold))
 '(jabber-activity-personal-face (:foreground sky-blue-1 :weight bold))
 '(jabber-chat-error (:inherit error))
 '(jabber-chat-prompt-foreign (:foreground scarlet-red-1 :weight bold))
 '(jabber-chat-prompt-local (:foreground sky-blue-1 :weight bold))
 '(jabber-chat-prompt-system (:foreground chameleon-1 :weight bold))
 '(jabber-rare-time-face (:foreground chameleon-3 :underline t))
 '(jabber-roster-user-away (:foreground chameleon-3 :weight normal :slant italic))
 '(jabber-roster-user-chatty (:foreground orange-3 :weight bold :slant normal))
 '(jabber-roster-user-dnd (:foreground scarlet-red-1 :weight normal :slant italic))
 '(jabber-roster-user-error (:foreground scarlet-red-2 :weight light :slant italic))
 '(jabber-roster-user-offline (:foreground aluminium-4 :weight light :slant italic))
 '(jabber-roster-user-online (:foreground sky-blue-1 :weight bold :slant normal))
 '(jabber-roster-user-xa (:foreground plum-1 :weight normal :slant italic))
 '(jabber-title-large (:foreground sky-blue-1 :weight bold :height 1.4))
 '(jabber-title-medium (:weight bold :height 1.2))
 '(jabber-title-small (:weight bold :height 1.0)))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'naquadah)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; naquadah-theme.el ends here
