;;; rebox2.el --- Handling of comment boxes in various styles.

;; Filename: rebox2.el
;; Description:
;; Author: François Pinard
;;         Le Wang
;; Maintainer: Le Wang (lewang.emacs!!!gmayo.com remove exclamations, correct host, hint: google mail)

;; Copyright © 2011 Le Wang
;; Copyright © 1991,92,93,94,95,96,97,98,00 Progiciels Bourbeau-Pinard inc.
;; François Pinard <pinard@iro.umontreal.ca>, April 1991.

;; Created: Mon Jan 10 22:22:32 2011 (+0800)
;; Version: 0.7
;; Package-Version: 20121113.500
;; Last-Updated: Tue Nov 13 20:57:37 2012 (+0800)
;;           By: Le Wang
;;     Update #: 478
;; URL: https://github.com/lewang/rebox2
;; Keywords:
;; Compatibility: GNU Emacs 23.2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     ;; Hi, I'm a box. My style is 525 ;;
                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Features first:
;;
;; ** minor-mode features
;;
;;    - auto-fill boxes (install filladapt for optimal filling)
;;    - motion (beginning-of-line, end-of-line) within box
;;    - S-return rebox-newline
;;    - kill/yank (within box) only text, not box borders
;;    - move box by using space, backspace / center with M-c
;;      - point has to be to the left of the border
;;
;; ** interesting variables for customization
;;
;;    - `rebox-style-loop'
;;    - `rebox-min-fill-column'
;;    - `rebox-allowance'


;;; Installation:
;;
;; 1. Add rebox2.el to a directory in your load-path.
;;
;; 2. Basic install - add to your ".emacs":
;;
;;        (setq rebox-style-loop '(24 16))
;;        (require 'rebox2)
;;        (global-set-key [(meta q)] 'rebox-dwim)
;;        (global-set-key [(shift meta q)] 'rebox-cycle)
;;
;;    Note that if you do not need to specify the HUNDREDS digit of the style,
;;    rebox will figure it out based on the major-moe.
;;
;; 3. Full install - use `rebox-mode' in major-mode hooks:
;;
;;        ;; setup rebox for emacs-lisp
;;        (add-hook 'emacs-lisp-mode-hook (lambda ()
;;                                          (set (make-local-variable 'rebox-style-loop) '(25 17 21))
;;                                          (set (make-local-variable 'rebox-min-fill-column) 40)
;;                                          (rebox-mode 1)))
;;
;;    Default `rebox-style-loop' should work for most programming modes, however,
;;    you may want to set the style you prefer.
;;
;;    Here is an customization example that
;;
;;      - sets comments to use "/* ... */" style in c++-mode
;;      - adds Doxygen box style for C++

;;        (defun my-c++-setup ()
;;          (setq comment-start "/* "
;;                comment-end " */")
;;          (unless (memq 46 rebox-style-loop)
;;            (make-local-variable 'rebox-style-loop)
;;            (nconc rebox-style-loop '(46))))
;;        (add-hook 'c++-mode-hook #'my-c++-setup)
;;
;;; Ideas removed from François Pinard's original version
;;
;; * Building styles on top of each other.
;;

;;; Future improvement ideas:
;;
;; * remove reliance on dynamic binding using `destructuring-bind' or a hash
;; * allow mixed borders "-=-=-=-=-=-"
;; * optimize functions that modify the box contents so they don't unbuild and
;;   rebuild boxes all the time.
;; * style selection can use some kind of menu completion where all styles are
;;   presented and the user navigates
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;; 0.6
;;
;; * add `rebox-allowance' customization to allow title in top/bottom border
;;   or not.
;; * restructured internal datastructure to be hash-table.
;;
;; 0.5
;;
;; * major breaking change
;;
;; * entry-functions completely rethought and simplified
;;
;;   removed:
;;    * `rebox-region'
;;    * `rebox-comment'
;;    * `rebox-dwim-fill'
;;    * `rebox-dwim-no-fill'
;;   added:
;;    * `rebox-dwim'
;;    * `rebox-fill'
;;    * `rebox-cycle'
;;
;; * boxing styling system rethought and simplified
;;
;;   removed:
;;    * `rebox-default-style'
;;    * `rebox-no-box-comment-style'
;;   added:
;;    * `rebox-style-loop'
;;
;; * new system description
;;
;;   I coded the previous entry system when I was new to Emacs lisp years and
;;   years ago.  The new system is much more intuitive.
;;
;;   Call `rebox-dwim' to refill current box or region and cycle with refill.
;;
;;   Call `rebox-cycle' to cycle style without refilling.
;;
;;   Instead of describing the boxed style and comment style as different
;;   variables, you describe the loop you want rebox to follow with
;;   `rebox-style-loop'.  If rebox detects that the current box style is not
;;   in the loop, it uses the first style.
;;
;;   `rebox-style-loop' is not made to be always buffer-local, and you can
;;   customize it using the custom facilities.  Optionally, you can make a
;;   buffer-local version of it in a major-mode hook yourself.
;;
;; 0.4
;;
;; * add `rebox-min-fill-column' option fo minimum box size
;;
;; before that
;;
;; * better error handling
;; * fixed a few boxing and unboxing corner cases where boxes were malformed
;; * changed how spaces are handled, rebox was very aggressive in removing
;;   white space in every direction, and even when it was keeping spaces, it
;;   would delete them and reinsert, which killed any markers.
;;
;;   now spaces are precious and never aggressively deleted.  Unboxing
;;   followed by boxing is idempotent.
;; * instead of parsing a current-prefix-arg in convoluted ways `rebox-engine'
;;   and most other functions now take key parameters
;; * increased the lengh of the box text in the box definition so that longer
;;   merged top and bottom borders can be specified.
;; * auto-filling
;; * minor-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;,----
;;| François Pinard's original commentary (with non-relevant stuff removed)
;;`----


;; For comments held within boxes, it is painful to fill paragraphs, while
;; stretching or shrinking the surrounding box "by hand", as needed.  This
;; piece of GNU Emacs LISP code eases my life on this.  I find only fair,
;; while giving all sources for a package using such boxed comments, to also
;; give the means I use for nicely modifying comments.  So here they are!

;; The function `rebox-comment' automatically discovers the extent of the
;; boxed comments near the cursor, possibly refills the text, then adjusts the
;; comment box style.  When this command is executed, the cursor should be
;; within a comment, or else it should be between two comments, in which case
;; the command applies to the next comment.  The function `rebox-region' does
;; the same, except that it takes the current region as a boxed comment.  Both
;; commands obey numeric prefixes to add or remove a box, force a particular
;; box style, or to prevent refilling of text.  Without such prefixes, the
;; commands may deduce the current comment box style from the comment itself
;; so the style is preserved.  An unboxed comment is merely one of box styles.

;; A style is identified by three non-zero digits.  The _convention_ about
;; style numbering is such the the hundreds digit roughly represents the
;; programming language, the tens digit roughly represents a box quality (or
;; weight) and the units digit roughly a box type (or figure).  Language,
;; quality and types are collectively referred to as style attributes.

;;;; Convention:

;; A programming language is associated with comment delimiters.  Values are
;; 100 for none or unknown, 200 for `/*' and `*/' as in plain C, 300 for `//'
;; as in C++, 400 for `#' as in most scripting languages, 500 for `;' as in
;; LISP or assembler and 600 for `%' as in TeX or PostScript.

;; Box quality differs according to language. For unknown languages (100) or
;; for the C language (200), values are 10 for simple, 20 for rounded, and 30
;; or 40 for starred.  Simple quality boxes (10) use comment delimiters to
;; left and right of each comment line, and also for the top or bottom line
;; when applicable. Rounded quality boxes (20) try to suggest rounded corners
;; in boxes.  Starred quality boxes (40) mostly use a left margin of asterisks
;; or X'es, and use them also in box surroundings.  For all others languages,
;; box quality indicates the thickness in characters of the left and right
;; sides of the box: values are 10, 20, 30 or 40 for 1, 2, 3 or 4 characters
;; wide.  With C++, quality 10 is not useful, you should force 20 instead.

;; Box type values are 1 for fully opened boxes for which boxing is done
;; only for the left and right but not for top or bottom, 2 for half
;; single lined boxes for which boxing is done on all sides except top,
;; 3 for fully single lined boxes for which boxing is done on all sides,
;; 4 for half double lined boxes which is like type 2 but more bold,
;; or 5 for fully double lined boxes which is like type 3 but more bold.

;; The special style 221 is for C comments between a single opening `/*' and a
;; single closing `*/'.  The special style 111 deletes a box.

;;;; History:

;; I first observed rounded corners, as in style 223 boxes, in code from
;; Warren Tucker, a previous maintainer of the `shar' package.  Besides very
;; special files, I was carefully avoiding to use such boxes for real work,
;; as I found them much too hard to maintain.  My friend Paul Provost was
;; working at Taarna, a computer graphics place, which had boxes as part of
;; their coding standards.  He asked that we try something to get out of his
;; misery, and this how `rebox.el' was originally written.  I did not plan to
;; use it for myself, but Paul was so enthusiastic that I timidly started to
;; use boxes in my things, very little at first, but more and more as time
;; passed, yet not fully sure it was a good move.  Later, many friends
;; spontaneously started to use this tool for real, some being very serious
;; workers.  This finally convinced me that boxes are acceptable, after all.

(require 'newcomment)

(eval-when-compile
  (require 'filladapt nil t)
  (require 'cl))

;; functions passed to rebox-engine inspect these variables
(eval-when-compile
  (defvar previous-nn)
  (defvar previous-ne)
  (defvar previous-sw)
  (defvar previous-ss)
  (defvar previous-se)
  (defvar previous-regexp3)
  (defvar previous-margin)
  (defvar previous-ee)
  (defvar previous-nw)
  (defvar border-margin)
  (defvar orig-m)
  (defvar orig-col)
  (defvar max-n)
  (defvar marked-point)
  (defvar curr-ww)
  (defvar previous-regexp1)
  (defvar curr-regexp1)
  (defvar orig-func)
  )



;; Box templates.  First number is style, second is recognition weight.
(defconst rebox-templates

  ;; Generic programming language templates.  Adding 300 replaces
  ;; `?' by `/', for C++ style comments.  Adding 400 replaces `?' by
  ;; `#', for scripting languages.  Adding 500 replaces `?' by ';',
  ;; for LISP and assembler.  Adding 600 replaces `?' by `%', for
  ;; TeX and PostScript.

  '((10 114
        "?box123456")

    (11 115
        "? box123456")

    (12 215
        "? box123456 ?"
        "? --------- ?")

    (13 315
        "? --------- ?"
        "? box123456 ?"
        "? --------- ?")

    (14 415
        "? box123456 ?"
        "?????????????")

    (15 515
        "?????????????"
        "? box123456 ?"
        "?????????????")

    (20 124
        "??box123456")

    (21 125
        "?? box123456")

    (22 225
        "?? box123456 ??"
        "?? --------- ??")

    (23 325
        "?? --------- ??"
        "?? box123456 ??"
        "?? --------- ??")

    (24 425
        "?? box123456 ??"
        "???????????????")

    (25 525
        "???????????????"
        "?? box123456 ??"
        "???????????????")

    (30 134
        "???box123456")

    (31 135
        "??? box123456")

    (32 235
        "??? box123456 ???"
        "??? --------- ???")

    (33 335
        "??? --------- ???"
        "??? box123456 ???"
        "??? --------- ???")

    (34 435
        "??? box123456 ???"
        "?????????????????")

    (35 535
        "?????????????????"
        "??? box123456 ???"
        "?????????????????")

    (40 144
        "????box123456")

    (41 145
        "???? box123456")

    (42 245
        "???? box123456 ????"
        "???? --------- ????")

    (43 345
        "???? --------- ????"
        "???? box123456 ????"
        "???? --------- ????")

    (44 445
        "???? box123456 ????"
        "???????????????????")

    (45 545
        "???????????????????"
        "???? box123456 ????"
        "???????????????????")

    (50 154
        "?????box123456")

    (51 155
        "????? box123456")

    (60 164
        "??????box123456")

    (61 165
        "?????? box123456")

    ;;,----
    ;;| boxquote style for comments
    ;;`----

    (16 126
        "?,----"
        "?| box123456"
        "?`----")

    (17 226
        "?,----------"
        "?| box123456"
        "?`----------")

    (26 126
        "??,----"
        "??| box123456"
        "??`----")

    (27 226
        "??,----------"
        "??| box123456"
        "??`----------")


    ;;; Text mode (non programming) templates.

    (111 113
         "box123456")

    (112 213
         "| box123456 |"
         "+-----------+")

    (113 313
         "+-----------+"
         "| box123456 |"
         "+-----------+")

    (114 413
         "| box123456 |"
         "*===========*")

    (115 513
         "*===========*"
         "| box123456 |"
         "*===========*")

    (116 114
         "---------"
         "box123456"
         "---------")

    (121 243
         "| box123456 |")

    (122 223
         "| box123456 |"
         "`-----------'")

    (123 323
         ".-----------."
         "| box123456 |"
         "`-----------'")

    (124 423
         "| box123456 |"
         "\\===========/")

    (125 523
         "/===========\\"
         "| box123456 |"
         "\\===========/")

    ;; boxquote style

    (126 126
        ",----"
        "| box123456"
        "`----")

    (127 226
        ",----------"
        "| box123456"
        "`----------")

    (136 126
        ",----"
        "| box123456"
        "`----")

    (137 226
        ",----------"
        "| box123456"
        "`----------")

    (141 143
         "| box123456 ")

    (142 243
         "* box123456 *"
         "*************")

    (143 343
         "*************"
         "* box123456 *"
         "*************")

    (144 443
         "X box123456 X"
         "XXXXXXXXXXXXX")

    (145 543
         "XXXXXXXXXXXXX"
         "X box123456 X"
         "XXXXXXXXXXXXX")

    ;; C language templates.

    (211 118
         "/* box123456 */")

    (212 218
         "/* box123456 */"
         "/* --------- */")

    (213 318
         "/* --------- */"
         "/* box123456 */"
         "/* --------- */")

    (214 418
         "/* box123456 */"
         "/* ========= */")

    (215 518
         "/* ========= */"
         "/* box123456 */"
         "/* ========= */")

    (221 128
         "/*"
         "   box123456"
         " */")

    (222 228
         "/*          ."
         " | box123456 |"
         " `----------*/")

    (223 328
         "/*----------."
         "| box123456 |"
         "`----------*/")

    (224 428
         "/*          \\"
         "| box123456 |"
         "\\==========*/")

    (225 528
         "/*==========\\"
         "| box123456 |"
         "\\==========*/")

    (231 138
         "/*"
         " | box123456"
         " */")

    (232 238
         "/*             "
         " | box123456 | "
         " *-----------*/")

    (233 338
         "/*-----------* "
         " | box123456 | "
         " *-----------*/")

    (234 438
         "/* box123456 */"
         "/*-----------*/")

    (235 538
         "/*-----------*/"
         "/* box123456 */"
         "/*-----------*/")

    (241 148
         "/*"
         " * box123456"
         " */")

    (242 248
         "/*           * "
         " * box123456 * "
         " *************/")
    (243 348
         "/************* "
         " * box123456 * "
         " *************/")

    (244 448
         "/* box123456 */"
         "/*************/")

    (245 548
         "/*************/"
         "/* box123456 */"
         "/*************/")

    ;; doxygen style
    (246 248
      "/************//**"
      " * box123456 "
      " ****************/")

    ))

(defvar rebox-cache nil)
(make-variable-buffer-local 'rebox-cache)
(defvar rebox-comment-vars-normalized nil)
(make-variable-buffer-local 'rebox-comment-vars-normalized)

(put 'rebox-cache 'permanent-local t)

(defvar rebox-save-env-vars
  '(comment-auto-fill-only-comments
    auto-fill-function
    normal-auto-fill-function)
  "list of variables overwritten by `rebox-mode' to be saved.")

(defgroup rebox nil
  "rebox."
  :group 'convenience)

(defface rebox-style-face
  '((t (:underline t)))
  "chevron markers around the selected style"
  :group 'rebox)

(defface rebox-style-chevron-face
  '((t (:inherit font-lock-warning-face)))
  "chevron markers around the selected style"
  :group 'rebox)

(defface rebox-disabled-style-face
  '((t (:inherit font-lock-comment-face :strike-through t)))
  "disabled style face"
  :group 'rebox)

(defcustom rebox-style-loop '(21 25 27)
  "list of styles for cycling by `rebox-cycle'

* In the future there may be a better way to try out box styles,
  but for now, you will have to look at the header of the source
  file to find the styles you like.

* Two or three digit styles are permissible in this list.  If
  using three digit style, be sure to make a buffer-local version
  of this variable. (see docs)

* It may be tempting to include the no-box style -- 111 in this
  list, but if you do, cycling through styles without an
  active-region will break because rebox can't figure out a
  region to act on.

  It's preferrable to have a boxing style that's undo-able by your
  major-mode's comment handling, like 11 or 21.  That way, when
  you need to remove comment marks, you can cycle to such a style
  and use `uncomment-region'.

"
  :type '(repeat number)
  :group 'rebox)

(defcustom rebox-min-fill-column nil
  "The minimum fill-column to use when filling boxes.
Boxes that start at column0 will be at least this many columns wide.

nil means boxes resize according to text."
  :group 'rebox)

(defcustom rebox-pad-sentence-end nil
  "When this is t, and refilling, if end-of-line is also the end of a sentence, then pad it with
`sentence-end-double-space' number of spaces."
  :group 'rebox)

(defcustom rebox-allowances '(top-title bottom-title)
  "If `top-title' is in this list, then a title may
be present in the top border.

If `bottom-title' is present then ditto for bottom.

 * Both would look like:

    *={ hi }======*
    | a b c  asdf |
    *====={ bye }=*

 * For a partial-width bottom border, a bottom title is never allowed.

 * The top and bottom titles are preserved while traversing the
   style-loop.  However if you settle on a style without borders,
   they will be discarded.
"
  :type '(repeat symbol)
  :group 'rebox)


(defcustom rebox-keep-blank-lines t
  "Non-nil gives rebox permission to truncate blank lines at
beginning of box, end, and more than three consecutive blank
lines in the body of box."
  :type 'boolean
  :group 'rebox)

(defcustom rebox-mode-line-string " rebox"
  ""
  :type 'string
  :group 'rebox)

(defcustom rebox-newline-indent-command 'comment-indent-new-line
  "function called by `rebox-indent-new-line' no box is found."
  :type 'command
  :group 'rebox)

(make-variable-buffer-local 'rebox-newline-indent-command)

(defcustom rebox-backspace-command 'backward-delete-char-untabify
  "function called by `rebox-backpace' when no box is found."
  :type 'command
  :group 'rebox)

(defcustom rebox-space-command 'self-insert-command
  "function called by `rebox-space' when no box is found."
  :type 'command
  :group 'rebox)

(defcustom rebox-kill-line-command 'kill-line
  "function called by `rebox-kill-line' when no box is found."
  :type 'command
  :group 'rebox)

(defcustom rebox-kill-ring-save-command 'kill-ring-save
  "function called by `rebox-kill-ring-save' when no box is found."
  :type 'command
  :group 'rebox)


(defcustom rebox-yank-command 'yank
  "function called by `rebox-yank' when no box is found."
  :type 'command
  :group 'rebox)

(defcustom rebox-end-of-line-command 'move-end-of-line
  "function called by `rebox-end-of-line' when no box is found."
  :type 'command
  :group 'rebox)

(defcustom rebox-beginning-of-line-command 'move-beginning-of-line
  "function called by `rebox-beginning-of-line' when no box is found."
  :type 'command
  :group 'rebox)

(defcustom rebox-yank-pop-command 'yank-pop
  "function called by `rebox-yank-pop' when no box is found."
  :type 'command
  :group 'rebox)

;;; We can potentially just lookup the key-binding for "C-j", but that would
;;; be wrong for emacs-lisp-mode, so it's not always consistent.
(defcustom rebox-newline-indent-function-alist
  '((c-mode   . c-indent-new-comment-line)
    (c++-mode . c-indent-new-comment-line)
    (org-mode . org-return-indent))
  "list of (major-mode . function) for making a newline.

The function should make and indent a new comment-line for the
mode.  `comment-indent-newline' is the default.
"
  :type '(alist :key-type 'symbol
                :value-type 'symbol)
  :group 'rebox)

(defcustom rebox-hybrid-major-modes
  '(org-mode)
  "Text based major modes that also have `comment-start' defined.

In these modes, auto-filling should be on for all text.  And
boxing should recognize paragraphs as well as comment blocks.
"
  :type 'list
  :group 'rebox)

(defmacro rebox-cache ()
  '(if (and (boundp 'rebox-cache)
            (symbol-value 'rebox-cache))
       rebox-cache
     (setq rebox-cache (make-hash-table :test 'eq :size 10))))

(defsubst rebox-is-regular-comment (style)
  (and (memq (% style 10) '(0 1))
       (> style 300)))

;;;###autoload
(define-minor-mode rebox-mode
  "Toggle rebox mode for managing text and comment boxes.

1. Auto-filling is enabled, and comment boxes are auto-filled.



With no argument, this command toggles the mode.
  Non-null prefix argument turns on the mode.
  Null prefix argument turns off the mode.

You don't need to enable the minor mode to use rebox2, see rebox2
header.

"
  :init-value nil
  :lighter rebox-mode-line-string
  :version "0.6"
  :keymap '(([(shift return)] . rebox-indent-new-line)
            ([(meta q)] . rebox-dwim)
            ([(meta Q)] . rebox-cycle)
            ([(control a)] . rebox-beginning-of-line)
            ([(control e)] . rebox-end-of-line)
            ([(control k)] . rebox-kill-line)
            ([(meta w)] . rebox-kill-ring-save)
            ([(control y)] . rebox-yank)
            ([(meta y)] . rebox-yank-pop)
            ([(meta c)] . rebox-center)
            (" " . rebox-space)
            ("" . rebox-backspace))
  :group 'rebox
  (if rebox-mode
      (progn
        (rebox-save-env)
        (set (make-local-variable 'comment-auto-fill-only-comments)
             (if (and (stringp comment-start)
                      (not (zerop (length comment-start)))
                      (not (memq major-mode rebox-hybrid-major-modes)))
                 t
               nil))
        (make-local-variable 'normal-auto-fill-function)
        (setq normal-auto-fill-function 'rebox-do-auto-fill)
        (auto-fill-mode 1)
        ;; we call non-autoloaded functions from newcomment, so this is needed
        (when (fboundp 'yas/minor-mode)
          (add-hook 'yas/before-expand-snippet-hook 'turn-off-rebox nil t)
          (add-hook 'yas/after-exit-snippet-hook 'turn-on-rebox nil t))
        (when (fboundp 'iedit-mode)
          (add-hook 'iedit-mode-hook 'turn-off-rebox nil t)
          (add-hook 'iedit-mode-end-hook 'turn-on-rebox nil t)))
    (rebox-restore-env)
    (when (called-interactively-p 'any)
      (remove-hook 'yas/before-expand-snippet-hook 'turn-off-rebox t)
      (remove-hook 'yas/after-exit-snippet-hook 'turn-on-rebox t)
      (remove-hook 'iedit-mode-hook 'turn-off-rebox t)
      (remove-hook 'iedit-mode-end-hook 'turn-on-rebox t))))

(defun turn-on-rebox ()
  (rebox-mode 1))

(defun turn-off-rebox ()
  (rebox-mode 0))

(put 'rebox-error
     'error-conditions
     '(error rebox-error))

(put 'rebox-error
     'error-message
     "rebox error")

(put 'rebox-comment-not-found-error
     'error-conditions
     '(error rebox-error rebox-comment-not-found-error))

(put 'rebox-comment-not-found-error
     'error-message
     "Comment not found")

(put 'rebox-mid-line-comment-found
     'error-conditions
     '(error rebox-error rebox-mid-line-comment-found))

(put 'rebox-mid-line-comment-found
     'error-message
     "Comment started mid-line.")

(put 'rebox-invalid-style-error
     'error-conditions
     '(error rebox-error rebox-invalid-style-error))

(put 'rebox-invalid-style-error
     'error-message
     "Invalid style.")

;; we don't use syntax table for whitespace definition here because we don't
;; trust major-modes to define them properly.
(defconst rebox-blank-line-regexp "^[ \t]*$")


;; Template numbering dependent code.

(defvar rebox-language-character-alist
  '((3 . "/") (4 . "#") (5 . ";") (6 . "%"))
  "Alist relating language to comment character, for generic languages.")

;;; Regexp to match the comment start, given a LANGUAGE value as index.

(defvar rebox-regexp-start
  ["^[ \t]*\\(/\\*\\|//+\\|#+\\|;+\\|%+\\)"
   "^"                                  ; 1
   "^[ \t]*/\\*"                        ; 2
   "^[ \t]*//+"                         ; 3
   "^[ \t]*#+"                          ; 4
   "^[ \t]*\;+"                         ; 5
   "^[ \t]*%+"                          ; 6
   ])



;; Template ingestion.

;;; Information about registered templates.
(defvar rebox-style-hash nil
  "contails parsed style hash")

;;; Register all box templates.

(defun rebox-register-all-templates ()
  (setq rebox-style-hash (make-hash-table :test 'eq :size 200))
  (dolist (template rebox-templates)
    (rebox-register-template (first template)
                             (second template)
                             (nthcdr 2 template))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; On merge-se:                                                              ;
;                                                                           ;
;   In the original version, if line3 was shorter than line2, then it's     ;
;   considered a merge of SE.  I think this is because of the "*/" C style  ;
;   end-comment.  However, the boxes would never look right if that was the ;
;   case, and I don't see any box style that look good when used this way.  ;
;                                                                           ;
;   Styles like the boxquote style need the shorter third line to be        ;
;   considered considered SW.                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Register a single box template.

(defun rebox-register-template (style weight lines)
  "Digest and register a single template.
The template is numbered STYLE, and is described by one to three LINES.

If STYLE is below 100, it is generic for a few programming languages and
within lines, `?' is meant to represent the language comment character.
STYLE should be used only once through all `rebox-register-template' calls.

One of the lines should contain the substring `box' to represent the comment
to be boxed, and if three lines are given, `box' should appear in the middle
one.  Lines containing only spaces are implied as necessary before and after
the the `box' line, so we have three lines.

Normally, all three template lines should be of the same length.  If the first
line is shorter, it represents a start comment string to be bundled within the
first line of the comment text.  If the third line is shorter, it represents
an end comment string to be bundled at the end of the comment text, and
refilled with it."
  (cond ((< style 100)
         (let ((pairs rebox-language-character-alist)
               language character)
           (while pairs
             (setq language (caar pairs)
                   character (cdar pairs)
                   pairs (cdr pairs))
             (rebox-register-template
              (+ (* 100 language) style)
              weight
              (mapcar (lambda (line)
                        (while (string-match "\?" line)
                          (setq line (replace-match character t t line)))
                        line)
                      lines)))))
        ((gethash style rebox-style-hash)
         (error "Style %d defined more than once" style))
        (t
         (let (line1 line2 line3 regexp1 regexp2 regexp3
                     regexp1-title regexp3-title
                     merge-nw merge-sw nw nn ne ww ee sw ss se
                     (hash (make-hash-table :test 'eq :size 20)))
           (if (string-match "box123456" (car lines))
               (setq line1 nil
                     line2 (car lines)
                     lines (cdr lines))
             (setq line1 (car lines)
                   line2 (cadr lines)
                   lines (cddr lines))
             (unless (string-match "box123456" line2)
               (error "Erroneous template for %d style" style)))
           (setq line3 (and lines (car lines)))
           (setq merge-nw (and line1 (< (length line1) (length line2)))
                 merge-sw (and line3 (< (length line3) (length line2)))
                 nw       (cond ((not line1) nil)
                                (merge-nw line1)
                                ((zerop (match-beginning 0)) nil)
                                (t (substring line1 0 (match-beginning 0))))
                 nn       (cond ((not line1) nil)
                                (merge-nw nil)
                                (t (let ((x (aref line1 (match-beginning 0))))
                                     (if (= x ? ) nil x))))
                 ne       (cond ((not line1) nil)
                                (merge-nw nil)
                                ((= (match-end 0) (length line1)) nil)
                                (t (rebox-rstrip (substring line1 (match-end 0)))))
                 ww       (cond ((zerop (match-beginning 0)) nil)
                                (t (substring line2 0 (match-beginning 0))))
                 ee       (cond ((= (match-end 0) (length line2)) nil)
                                (t (let ((str (rebox-rstrip (substring line2 (match-end 0)))))
                                     (if (zerop (length str))
                                         nil
                                       str))))
                 sw       (cond ((not line3) nil)
                                (merge-sw (rebox-rstrip line3))
                                ((zerop (match-beginning 0)) nil)
                                (t (substring line3 0 (match-beginning 0))))
                 ss       (cond ((not line3) nil)
                                (merge-sw nil)
                                (t (let ((x (aref line3 (match-beginning 0))))
                                     (if (= x ? ) nil x))))
                 se       (cond ((not line3) nil)
                                (merge-sw nil)
                                ((= (match-end 0) (length line3)) nil)
                                (t (rebox-rstrip (substring line3 (match-end 0))))))
           (setq regexp1 (cond
                          (merge-nw
                           (concat "^[ \t]*" (rebox-regexp-quote nw :rstrip nil) "\n"))
                          ((and nw (not nn) (not ne))
                           (concat "^[ \t]*" (rebox-regexp-quote nw :rstrip nil) "\n"))
                          ((or nw nn ne)
                           (concat "^[ \t]*" (rebox-regexp-quote nw :rstrip nil)
                                   (rebox-regexp-ruler nn)
                                   (rebox-regexp-quote ne :lstrip nil) "\n")))
                 regexp2 (and (not (string-equal (rebox-rstrip (concat ww ee))
                                                 ""))
                              (if ee
                                  (concat "^[ \t]*"
                                          (rebox-regexp-quote ww :rstrip nil)
                                          ".*"
                                          (rebox-regexp-quote ee :lstrip nil)
                                          "\n")
                                ;; allow lines with trimmed whitespace at EOL
                                ;; to still match
                                (concat "^[ \t]*"
                                        "\\(?:"
                                        (rebox-regexp-quote ww :rstrip nil)
                                        ".*"
                                        "\\|"
                                        (rebox-regexp-quote ww)
                                        "\\)"
                                        "\n")))
                 regexp3 (cond
                          ((or merge-sw
                               (and sw (not ss) (not se)))
                           (concat "^[ \t]*" (rebox-regexp-quote sw :lstrip nil) "\n"))
                          ((or sw ss se)
                           (concat "^[ \t]*"
                                   (rebox-regexp-quote sw :lstrip nil)
                                   (rebox-regexp-ruler ss)
                                   (rebox-regexp-quote se :lstrip nil) "\n"))))
           (setq regexp1-title (cond
                                (merge-nw
                                 (concat "^[ \t]*" (rebox-regexp-quote nw) "\\(.*?\\)[ \t]*\n"))
                                ((and nw (not nn) (not ne))
                                 (concat "^[ \t]*" (rebox-regexp-quote nw) "\\(.*?\\)[ \t]*\n"))
                                ((or nw nn ne)
                                 (concat "^[ \t]*"
                                         (if nw
                                             (rebox-regexp-quote nw)
                                           (rebox-regexp-ruler nn))
                                         "\\(?:.*?\\)"
                                         (rebox-regexp-ruler nn)
                                         (rebox-regexp-quote ne :lstrip nil) "\n")))
                 regexp3-title (cond
                                ((or merge-sw
                                     (and sw (not ss) (not se)))
                                 (concat "^[ \t]*" (rebox-regexp-quote sw :lstrip nil) "\n"))
                                ((or sw ss se)
                                 (concat "^[ \t]*"
                                         (if sw
                                             (rebox-regexp-quote sw :rstrip nil)
                                           (rebox-regexp-ruler ss))
                                         "\\(.*?\\)"
                                         (rebox-regexp-ruler ss)
                                         (rebox-regexp-quote se :lstrip nil) "\n"))))
           (dolist (k '(style weight
                        regexp1 regexp2 regexp3
                        regexp1-title regexp3-title
                        merge-nw merge-sw
                        nw nn ne
                        ww ee
                        sw ss se))
             (when (symbol-value k)
               (puthash (intern (concat ":" (symbol-name k))) (symbol-value k) hash)))
           (puthash style hash rebox-style-hash)))))

(defun rebox-get-canonical-style (style &optional c-start)
  "return 3 digit style based on 2 digit style and a comment-start type string"
  (setq c-start (or (and c-start
                         (stringp c-start)
                         c-start)
                    comment-start))
  (cond ((and (numberp style)
              (< style 100)
              (> style 0))
         (+ (* 100 (rebox-guess-language c-start))
            style))
        ((numberp style)
         style)
        (t
         (error "unknown style: %s" style))))

(defun rebox-loop-get-style (can-style movement &optional style-loop c-start)
  "Get the next *VALID* style in STYLE-LOOP based on MOVEMENT.

That is, if movement doesn't land a valid style, get the next
valid style after it.

If style isn't found return first style."
  (setq style-loop (or style-loop
                       rebox-style-loop))
  (setq c-start (or c-start
                    comment-start))
  (let ((l-len (length style-loop))
        (can-loop (mapcar (lambda (i)
                            (rebox-get-canonical-style i c-start))
                          style-loop))
        res
        index
        new-index
        new-style)
    (setq index (- l-len (length (memq can-style can-loop))))
    ;; when it's not found, set it to zero
    (when (= index l-len)
      (setq index 0)
      ;; when you don't start with a valid style, get back to the
      ;; first style in the list
      (setq movement 0))
    (or (dotimes (i l-len)
          ;; note `%' could result in negative number
          (setq new-index (% (+ l-len (% (+ index (if (< movement 0)
                                                      (- movement i)
                                                    (+ movement i)))
                                         l-len))
                             l-len))
          (setq new-style (rebox-get-canonical-style
                           (nth new-index can-loop)
                           c-start))
          (when (gethash new-style rebox-style-hash)
            (return new-style)))
        (signal 'rebox-invalid-style-error (list (format "no valid style found in loop: %s" style-loop))))))

(defun rebox-make-fill-prefix ()
  "generate fill prefix using adaptive filling methods"
  (beginning-of-line)
  (if (featurep 'filladapt)
      (filladapt-make-fill-prefix
       (filladapt-parse-prefixes))
    (fill-context-prefix
     (point-at-bol)
     (point-at-eol))))


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;; auto-fill strategy                                                   ;;;
 ;;;                                                                      ;;;
 ;;; 1. make `normal-auto-fill-function' `rebox-do-auto-fill'             ;;;
 ;;;                                                                      ;;;
 ;;; 2. activate auto-fill-mode, setting `auto-fill-function' to          ;;;
 ;;;    `rebox-do-auto-fill'                                              ;;;
 ;;;                                                                      ;;;
 ;;; 3. when auto-fillling, if we are in a box, we call the default value ;;;
 ;;;    of `normal-auto-fill-function', since we've stripped off the      ;;;
 ;;;    comments and any major specific auto-fill functions will be       ;;;
 ;;;    confused.                                                         ;;;
 ;;;                                                                      ;;;
 ;;;    If we are not in a box, we call our saved value of                ;;;
 ;;;    `normal-auto-fill-function'                                       ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun rebox-do-auto-fill ()
  "Try to fill as box first, if that fails use `do-auto-fill'
"
  (let ((orig-m (point-marker))
        style)
    (condition-case err
        (save-restriction
          (rebox-find-and-narrow :comment-only comment-auto-fill-only-comments)
          (setq style (rebox-guess-style))
          (goto-char orig-m)
          (cond ((= style 111)
                 (signal 'rebox-error nil))
                ((rebox-is-regular-comment style)
                 (let ((comment-style 'indent))
                   (rebox-call-alternate-fill-function 'prog)))
                (t
                 (rebox-engine :previous-style style
                               :marked-point orig-m
                               :refill 'auto-fill
                               :quiet t
                               :move-point nil
                               :before-insp-func
                               (lambda ()
                                 (goto-char marked-point)
                                 ;; top or bottom or left or right border
                                 (when (or (and previous-regexp1
                                                (eq (line-number-at-pos) 1))
                                           (and previous-regexp3
                                                (eq (line-number-at-pos) (1- (line-number-at-pos (point-max)))))
                                           (< (current-column) border-margin)
                                           (and previous-ee
                                                (looking-back (rebox-regexp-quote previous-ee :lstrip nil))
                                                (looking-at-p "[ \t]*$")))
                                   (throw 'rebox-engine-done t)))
                               :after-insp-func
                               (lambda ()
                                 ;; pressing space at boundary - changing style from 520 to 521
                                 ;; moves point to bol, we need to move it back.
                                 (when (and (eq this-command 'rebox-space)
                                            (bolp))
                                   (rebox-beginning-of-line nil)
                                   (setq marked-point (point))))))))
      ('rebox-error
       (goto-char orig-m)
       (rebox-call-alternate-fill-function))
      ('error
       (error "rebox-do-auto-fill wrapper: %s" err)))))


(defun rebox-call-alternate-fill-function (&optional mode)
  (let ((fill-func (cond (;; we always enable auto-fill so we call `normal-auto-fill-function' directly.
                          (eq mode 'fundamental)
                          (default-value 'normal-auto-fill-function))
                         ((eq mode 'prog)
                          (cdr (assq 'normal-auto-fill-function
                                     (gethash :rebox-save-env-alist (rebox-cache)))))
                         (t
                          (cdr (assq 'normal-auto-fill-function
                                     (gethash :rebox-save-env-alist (rebox-cache))))))))
    (if fill-func
        (funcall fill-func)
      (signal 'rebox-error '("appropriate auto-fill-function not found.")))))


;; User interaction.

;;;###autoload
(defun rebox-beginning-of-line (arg)
  (interactive "^P")
  (let ((orig-m (point-marker))
        previous-style
        boxed-line-start-col)
    (save-restriction
      (condition-case err
          (progn
            (rebox-find-and-narrow :comment-only comment-auto-fill-only-comments)
            (setq previous-style (rebox-guess-style))
            (if (eq previous-style 111)
                (signal 'rebox-error '("style is 111"))
              (rebox-engine :style previous-style
                            :marked-point orig-m
                            :quiet t
                            :refill nil
                            :move-point nil
                            :previous-style previous-style
                            :before-insp-func
                            (lambda ()
                              (goto-char marked-point)
                              (if ;; top or bottom border
                                  (or (and previous-regexp1
                                           (eq (line-number-at-pos) 1))
                                      (and previous-regexp3
                                           (eq (line-number-at-pos) (1- (line-number-at-pos (point-max))))))
                                  (setq boxed-line-start-col previous-margin)
                                (save-restriction
                                  (narrow-to-region (+ (point-at-bol) border-margin)
                                                    (point-at-eol))
                                  (beginning-of-line 1)
                                  (setq boxed-line-start-col
                                        (if (looking-at-p (concat "[ \t]*"
                                                                  (and previous-ee
                                                                       (regexp-quote previous-ee))
                                                                  "$"))
                                            border-margin
                                          (+ (length (rebox-make-fill-prefix))
                                             border-margin)))))
                              (throw 'rebox-engine-done t)))
              (goto-char orig-m)
              (if (or (bolp)
                      (> (current-column) boxed-line-start-col))
                  (move-to-column boxed-line-start-col)
                (move-beginning-of-line 1))))
        ('rebox-error
         (goto-char orig-m)
         (rebox-call-command (rebox-get-fallback 'rebox-beginning-of-line-command)))
        ('error
         (signal (car err) (cdr err)))))))
(put 'rebox-beginning-of-line 'function-documentation
     '(concat
       "Rebox behaviour: go to beginning of actual text.\n\n"
       (rebox-document-binding 'rebox-beginning-of-line-command)))


;;;###autoload
(defun rebox-end-of-line (arg)
  (interactive "^P")
  (let ((orig-m (point-marker))
        previous-style
        boxed-line-end-col)
    (save-restriction
      (condition-case err
          (progn
            (rebox-find-and-narrow :comment-only comment-auto-fill-only-comments)
            (setq previous-style (rebox-guess-style))
            (if (eq previous-style 111)
                (signal 'rebox-error '("style is 111"))
              (rebox-engine :style previous-style
                            :quiet t
                            :refill nil
                            :move-point nil
                            :previous-style previous-style
                            :before-insp-func
                            (lambda ()
                              (goto-char orig-m)
                              (beginning-of-line)
                              (if ;; top or bottom border
                                  (or (and previous-regexp1
                                           (eq (line-number-at-pos) 1))
                                      (and previous-regexp3
                                           (eq (line-number-at-pos) (1- (line-number-at-pos (point-max))))))
                                  (setq boxed-line-end-col (point-at-eol))
                                (setq boxed-line-end-col
                                      (if previous-ee
                                          (progn
                                            (search-forward-regexp (concat "[ \t]*"
                                                                           (rebox-regexp-quote previous-ee)
                                                                           "$")
                                                                   (point-at-eol))
                                            (goto-char (match-beginning 0))
                                            (current-column))
                                        (search-forward-regexp "[ \t]*$"
                                                               (point-at-eol))
                                        (goto-char (match-beginning 0))
                                        (current-column)))
                                ;; blank line
                                (when (<= boxed-line-end-col border-margin)
                                  (setq boxed-line-end-col (progn (goto-char (point-at-eol))
                                                                  (current-column)))))
                              (throw 'rebox-engine-done t)))
              (goto-char orig-m)
              (if (or (eolp)
                      (< (current-column) boxed-line-end-col))
                  (move-to-column boxed-line-end-col)
                (move-end-of-line 1))))
        ('rebox-error
         (goto-char orig-m)
         (rebox-call-command (rebox-get-fallback 'rebox-end-of-line-command)))
        ('error
         (signal (car err) (cdr err)))))))
(put 'rebox-end-of-line 'function-documentation
     '(concat
       "Rebox behaviour: go to end of actual text.\n\n"
       (rebox-document-binding 'rebox-end-of-line-command)))

;;;###autoload
(defun rebox-kill-line (arg)
  (interactive "P*")
  (if (consp arg)
      (if (equal arg '(4))
          (funcall (rebox-get-fallback 'rebox-kill-line-command) 1)
        (funcall (rebox-get-fallback 'rebox-kill-line-command) (list (/ (car arg) 4))))
    (let (orig-col orig-line)
      (rebox-kill-yank-wrapper :before-insp-func
                               (lambda ()
                                 (goto-char marked-point)
                                 (setq orig-line (if previous-regexp1
                                                     (1- (line-number-at-pos))
                                                   (line-number-at-pos)))
                                 (setq orig-col (- (current-column) border-margin)))
                               :mod-func
                               (lambda ()
                                 (goto-char marked-point)
                                 (condition-case err
                                     (progn
                                       (if (and (use-region-p)
                                                delete-selection-mode)
                                           (progn
                                             (kill-region (region-beginning) (region-end))
                                             (goto-char (point-max))
                                             ;; ensure narrowed region is still valid
                                             (unless (bolp)
                                               (insert "\n")))
                                         (rebox-call-command (rebox-get-fallback 'rebox-kill-line-command))))
                                   ('end-of-buffer
                                    (signal 'end-of-buffer `(,(format "end of box reached, aborting %s." this-command)
                                                             ,@(cdr err))))))
                               :after-insp-func
                               (lambda ()
                                 ;; try to fix the point
                                 (goto-char marked-point)
                                 (let ((new-line-num (if curr-regexp1
                                                         (1- (line-number-at-pos))
                                                       (line-number-at-pos)))
                                       (new-col (- (current-column) previous-margin (length curr-ww))))
                                   (when (and (< new-line-num 1)
                                              (>= orig-line 1))
                                     ;; goto-line
                                     (goto-char (point-min))
                                     (forward-line (1- (+ orig-line (if curr-regexp1 1 0)))))
                                   (when (and (< new-col 0)
                                              (>= orig-col 0))
                                     (move-beginning-of-line 1)
                                     (rebox-beginning-of-line 1))
                                   (set-marker marked-point (point))))
                               :orig-func
                               (rebox-get-fallback 'rebox-kill-line-command)))))
(put 'rebox-kill-line 'function-documentation
     '(concat
       "Rebox behaviour: kill content without box.  With universal arg, always
call fallback.  With 1+ universal arg, pass (n-1) args to fallback.\n\n"
       (rebox-document-binding 'rebox-kill-line-command)))

(defun rebox-yank (arg)
  (interactive "P*")
  (rebox-kill-yank-wrapper :not-at-nw t
                           :mod-func
                           (lambda ()
                             (goto-char orig-m)
                             (rebox-call-command (rebox-get-fallback 'rebox-yank-command))
                             (set-marker orig-m (point)))
                           :orig-func
                           (rebox-get-fallback 'rebox-yank-command)))
(put 'rebox-yank 'function-documentation
     '(concat
       "Rebox behaviour: yank content into box.  With universal ARG, always
call fallback.

To pass universal ARG to fall-back function, use C-u C-u."
       (rebox-document-binding 'rebox-yank-command)))

(defun rebox-yank-pop (arg)
  (interactive "P*")
  (rebox-kill-yank-wrapper :not-at-nw t
                           :mod-func
                           (lambda ()
                             (goto-char orig-m)
                             (rebox-call-command (rebox-get-fallback 'rebox-yank-pop-command))
                             (set-marker orig-m (point)))
                           :orig-func
                           (rebox-get-fallback 'rebox-yank-pop-command)))
(put 'rebox-yank-pop 'function-documentation
     '(concat
       "Rebox behaviour: yank-pop without box.  With universal arg,
always call fallback.\n\n"
       (rebox-document-binding 'rebox-yank-pop-command)))

(defun rebox-kill-ring-save (arg)
  (interactive "P")
  (let ((mod-p (buffer-modified-p)))
    (rebox-kill-yank-wrapper :try-whole-box t
                             ;; `not-past-se' refers to bol just after box
                             :not-past-se (if (use-region-p)
                                              nil
                                            t)
                             :mod-func
                             (lambda ()
                               (goto-char orig-m)
                               (rebox-call-command rebox-kill-ring-save-command)
                               (set-marker orig-m (point-marker)))
                             :orig-func
                             (rebox-get-fallback 'rebox-kill-ring-save-command))
    ;; kill-ring-save shouldn't change buffer-modified status
    (set-buffer-modified-p mod-p)))
(put 'rebox-kill-ring-save 'function-documentation
     '(concat
       "Rebox behaviour: save content without box.  With universal arg, always call fallback.\n\n"
       (rebox-document-binding 'rebox-kill-ring-save-command)))

(defun rebox-center ()
  (interactive "*")
  (rebox-left-border-wrapper (lambda ()
                               (if (< (current-column) border-margin)
                                   (center-region (point-min) (point-max))
                                 (when orig-func
                                   (rebox-call-command orig-func)
                                   (set-marker orig-m (point))))
                               (throw 'rebox-engine-done t))
                             (rebox-get-fallback)))
(put 'rebox-center 'function-documentation
     '(concat
       "Rebox behaviour: center box.\n\n"
       (rebox-document-binding)))

(defun rebox-space (n)
  (interactive "p*")
  (rebox-left-border-wrapper (lambda ()
                               (goto-char orig-m)
                               (if (< (current-column) border-margin)
                                   (progn
                                     (set-marker-insertion-type orig-m t)
                                     (string-rectangle (point-min)
                                                       (progn (goto-char (point-max))
                                                              (point-at-bol 0))
                                                       (make-string n ? )))
                                 (rebox-call-command rebox-space-command)
                                 ;; we can't change insertion-type in case
                                 ;; this is the last column of the box
                                 (set-marker orig-m (point)))
                               (throw 'rebox-engine-done t))
                             (rebox-get-fallback 'rebox-space-command)))
(put 'rebox-space 'function-documentation
     '(concat
       "Rebox behaviour: if point is in the left border of a box, move box to the
 right.  With argument N, move n columns.\n\n"
       (rebox-document-binding 'rebox-space-command)))

(defun rebox-backspace (n)
  (interactive "*p")
  (rebox-left-border-wrapper (lambda ()
                                 (if (< orig-col border-margin)
                                     (delete-rectangle (point-min)
                                                       (progn (goto-char (point-max))
                                                              (beginning-of-line 0)
                                                              (setq max-n (min orig-col previous-margin))
                                                              (if (< n max-n)
                                                                  (move-to-column n)
                                                                (move-to-column max-n))
                                                              (point)))
                                   (goto-char orig-m)
                                   (rebox-call-command rebox-backspace-command))
                                 (throw 'rebox-engine-done t))
                               (rebox-get-fallback 'rebox-backspace-command)))
 (put 'rebox-backspace 'function-documentation
     '(concat
       "Rebox behaviour: in the left border of a box, move box to the left.
With argument N, move n columns.\n\n"
       (rebox-document-binding 'rebox-backspace-command)))

;;;###autoload
(defun rebox-indent-new-line (arg)
  "Create newline.

Prefix arg greater than zero inserts arg lines.  Other prefix arg
causes refilling without actually inserting a newline.

If point is within a box, insert line in box.  As usual any
comment is a box.


If point is outside a box call function from
`rebox-newline-indent-function-alist'.
"
  (interactive "*P")
  (save-restriction
    (let (orig-m
          style
          text-beg-col)
      (condition-case err
          (progn
            (setq orig-m (point-marker))
            (rebox-find-and-narrow :comment-only comment-auto-fill-only-comments)
            (set-marker-insertion-type orig-m t)
            (setq style (rebox-guess-style))
            (if (not (or (= style 111)
                         (rebox-is-regular-comment style)))
                (progn
                  (setq arg (cond ((not arg)
                                   1)
                                  ((and arg
                                        (numberp arg)
                                        (> arg 0))
                                   arg)
                                  (t
                                   (signal 'error (format "arg %s not supported by `rebox-indent-new-line'"
                                                          arg)))))
                  (rebox-engine :previous-style style
                                :refill nil
                                :mod-func
                                (lambda ()
                                  ;;-lw- just always goto beginning of line?
                                  (when arg
                                    (goto-char orig-m)
                                    (if (looking-at-p "[ \t]*$")
                                        ;; creating blank line
                                        (progn
                                          (beginning-of-line)
                                          (setq text-beg-col
                                                (if (looking-at-p "[ \t]*$")
                                                    (progn
                                                      (goto-char orig-m)
                                                      (+ previous-margin
                                                         (length curr-ww)
                                                         (current-column)))
                                                  (skip-chars-forward " \t")
                                                  (+ previous-margin
                                                     (length curr-ww)
                                                     (current-column)))))
                                      (setq text-beg-col (+ previous-margin (length curr-ww))))
                                    (goto-char orig-m)
                                    (newline arg))))
                  (goto-char orig-m)
                  (move-to-column text-beg-col t))
              (let ((comment-style 'indent)
                    column-start)
                (widen)
                (goto-char orig-m)
                (comment-beginning)
                (setq column-start (current-column))
                (goto-char orig-m)
                (rebox-call-command (rebox-get-newline-indent-function))
                (indent-to-column column-start))))
        ('rebox-error
         (let ((err-marker (point-marker))
               (saved-func (rebox-get-newline-indent-function)))
           (goto-char orig-m)
           (cond ((eq (car err) 'rebox-comment-not-found-error)
                  (message "rebox-indent-new-line: unable to find comment, calling %s."
                           saved-func))
                 ((eq (car err) 'rebox-mid-line-comment-found)
                  (message "midline comment found at (%s), calling %s" err-marker saved-func)))
           (rebox-call-command saved-func)))
        ('rebox-comment-not-found-error
         (message "rebox-indent-new-line: unable to find comment, calling saved function.")
         (rebox-call-command (rebox-get-newline-indent-function)))
        ('error
         (error "rebox-indent-new-line wrapper: %s" err))))))

;;;###autoload
(defun rebox-dwim (arg)
  "On first invocation, fill region or comment.

Subsequent invocations cycle though styles defined in
`rebox-style-loop', **REFILLING EACH TIME**.

Style may be specified through prefix arg.

If refilling is not desired, use `rebox-cycle'
"
  (interactive "*P")
  (if (eq last-command this-command)
      (rebox-cycle arg 'refill)
    (if arg
        (rebox-cycle arg 'refill)
      (rebox-fill))))

;;;###autoload
(defun rebox-fill ()
  "refill the current box or fill current region as a box"
  (interactive "*")
  (rebox-cycle 'keep-style t))

;;;###autoload
(defun rebox-cycle (arg &optional refill)
  "Cycle current region or comment to the next style in loop.

When called interactively, never refill.

With universal arg (C-u), use previous style in loop.

With numeric arg, use explicit style.
"
  (interactive "*P")
  (let ((orig-m (point-marker))
        ;;copy of mark, so we can possibly change the insertion type
        style
        movement
        previous-style)
    (cond ((consp arg)
           (setq movement -1))
          ((null arg)
           (setq movement 1))
          ((and (numberp arg)
                (> arg 0))
           (setq style (rebox-get-canonical-style arg)))
          ((eq arg 'keep-style))
          (t
           (error "invalid argument -- `%s'" arg)))
    (save-restriction
      (condition-case err
          (flet ((work (r-beg r-end &optional preserve-region)
                       (narrow-to-region r-beg r-end)
                       (setq previous-style (rebox-guess-style))
                       (cond ((eq arg 'keep-style)
                              (setq style previous-style))
                             ((not style)
                              (setq style (rebox-loop-get-style previous-style movement))))
                       (if (and refill
                                (eq arg 'keep-style))
                           (message "Refilling style %s" previous-style)
                         (message (concat "rebox loop: "
                                          (rebox-propertize-style-loop style)
                                          (if refill
                                              " w/ refill"
                                            ""))))
                       (rebox-engine :style style
                                     :previous-style previous-style
                                     :refill refill
                                     :quiet t
                                     :move-point (not preserve-region)
                                     :marked-point orig-m)
                       (when preserve-region
                         (if (< (point) (mark))
                             (progn
                               (set-mark (point-max))
                               (set-marker orig-m (point-min)))
                           (set-mark (point-min))
                           (set-marker orig-m (point-max)))
                         (setq deactivate-mark nil))))
            (if (use-region-p)
                (if (and (prog2
                             (goto-char (region-beginning))
                             (eq (point) (point-at-bol))
                           (goto-char orig-m))
                         (prog2
                             (goto-char (region-end))
                             (eq (point) (point-at-bol))
                           (goto-char orig-m)))
                    (work (region-beginning) (region-end) 'preserve-region)
                  (let ((r-beg (prog2
                                   (goto-char (region-beginning))
                                   (point-marker)
                                 (goto-char orig-m)))
                        (r-end (prog2
                                   (goto-char (region-end))
                                   (point-marker)
                                 (goto-char orig-m))))
                    (rebox-ensure-region-whole-lines r-beg r-end)
                    (if (< (point) (mark))
                        (progn
                          (set-mark r-end)
                          (goto-char r-beg))
                      (set-mark r-beg)
                      (goto-char r-end))
                    (work (region-beginning) (region-end) 'preserve-region)
                    (set-marker orig-m (point-max))))
              (rebox-find-and-narrow :comment-only comment-auto-fill-only-comments)
              (work (point-min) (point-max))))
        ('rebox-invalid-style-error
         (error "%s" err))
        ('rebox-error
         (goto-char orig-m)
         (when (and refill
                    (not (use-region-p)))
           (rebox-call-command 'fill-paragraph)))
        ('error
         (error "rebox-cycle caught error: %s" err))))
    (goto-char orig-m)))




(defun* rebox-kill-yank-wrapper (&key try-whole-box not-at-nw not-past-se mod-func orig-func before-insp-func after-insp-func)
  (let ((orig-m (point-marker))
        previous-style)
    (condition-case err
        (progn
          (when (or (consp current-prefix-arg)
                    buffer-read-only)
            ;; call orig-func
            (signal 'rebox-error nil))
          (save-restriction
            (when (and (eq last-command 'yank)
                       (not (gethash :last-style (rebox-cache))))
              (signal 'rebox-error nil))
            (rebox-find-and-narrow :comment-only comment-auto-fill-only-comments
                                   :try-whole-box try-whole-box)
            (when (and
                   not-at-nw
                   (progn
                     (goto-char (point-min))
                     (skip-syntax-forward " " orig-m)
                     (= (point) orig-m)))
              (signal 'rebox-error '("mark is out of box")))
            (when (and (= orig-m (point-max))
                       (progn
                         (goto-char orig-m)
                         (bolp))
                       not-past-se)
              (signal 'rebox-error '("mark is out of box")))
            (when (and (use-region-p)
                       (or (< (mark) (point-min))
                           (> (mark) (point-max))))
              (signal 'rebox-error '("mark is out of box")))
            (setq previous-style (rebox-guess-style))
            (if (eq previous-style 111)
                (signal 'rebox-error '("style is 111"))
              (if (eq last-command 'yank)
                  (assert (eq (gethash :last-style (rebox-cache)) previous-style))
                (puthash :last-style previous-style (rebox-cache)))
              (rebox-engine :style previous-style
                            :marked-point orig-m
                            :quiet t
                            :refill nil
                            :move-point nil
                            :previous-style previous-style
                            :before-insp-func
                            before-insp-func
                            :mod-func
                            mod-func
                            :after-insp-func
                            after-insp-func))))
      ('rebox-error
       (puthash :last-style nil (rebox-cache))
       (goto-char orig-m)
       (and orig-func
            (let ((current-prefix-arg (when (consp current-prefix-arg)
                                        (list (/ (car current-prefix-arg) 4)))))
              (rebox-call-command orig-func))))
      ('error
       (signal (car err) (cdr err))))))

(defun rebox-left-border-wrapper (before-insp-func orig-func)
  (let ((orig-m (point-marker))
        (orig-col (current-column))
        previous-style)
    (condition-case err
        (progn
          (when (use-region-p)
            (signal 'rebox-error '("region used")))
          (save-restriction
            (rebox-find-and-narrow :comment-only comment-auto-fill-only-comments)
            (setq previous-style (rebox-guess-style))
            (if (eq previous-style 111)
                (signal 'rebox-error '("style is 111"))
              (goto-char orig-m)
              (rebox-engine :style previous-style
                            :marked-point orig-m
                            :quiet t
                            :refill nil
                            :move-point nil
                            :previous-style previous-style
                            :before-insp-func
                            before-insp-func))))
      ('rebox-error
       (goto-char orig-m)
       (and orig-func
            (rebox-call-command orig-func)))
      ('error
       (signal (car err) (cdr err))))))



(defun rebox-ensure-region-whole-lines (r-beg r-end)
  "Ensure region covered by r-beg and r-end are whole lines.

R-BEG and R-END are markers.  We assume R-BEG < R-END.

Returns t when changes were made to the markers."
  (let (col
        changes-made)
    (goto-char r-beg)
    (unless (bolp)
      (setq changes-made t)
      (setq col (current-column))
      (beginning-of-line 1)
      (unless (prog2
                  (skip-chars-forward " \t")
                  (>= (current-column) col))
        (goto-char r-beg)
        (insert "\n")
        (indent-to col))
      (set-marker r-beg (point-at-bol)))
    (goto-char r-end)
    (unless (bolp)
      (setq changes-made t)
      (if (and (looking-at-p "[ \t]*$")
               (not (eobp)))
          (set-marker r-end (point-at-bol 2))
        (insert "\n")
        (set-marker r-end (point))))
    changes-made))

(defun* rebox-engine (&key style
                           mod-func
                           before-insp-func
                           after-insp-func
                           (refill t)
                           (previous-style nil)
                           (quiet nil)
                           (marked-point nil)
                           (move-point t))
  "Add, delete or adjust a comment box in the narrowed buffer.

The narrowed buffer should contain only whole lines, otherwise it will look strange once widened.

"
  (let* ((undo-list buffer-undo-list)
         clean-undo-list
         (marked-point (or marked-point
                           (point-marker)))
         (previous-margin (rebox-left-margin))
         (previous-style (or previous-style
                             (rebox-guess-style)))
         (prev-h (gethash previous-style rebox-style-hash))
         (previous-regexp1 (gethash :regexp1 prev-h))
         (previous-regexp2 (gethash :regexp2 prev-h))
         (previous-regexp3 (gethash :regexp3 prev-h))
         (previous-merge-nw (gethash :merge-nw prev-h))
         (previous-merge-sw (gethash :merge-sw prev-h))
         (previous-nw (gethash :nw prev-h))
         (previous-nn (gethash :nn prev-h))
         (previous-ne (gethash :ne prev-h))
         (previous-ww (gethash :ww prev-h))
         (previous-ee (gethash :ee prev-h))
         (previous-sw (gethash :sw prev-h))
         (previous-ss (gethash :ss prev-h))
         (previous-se (gethash :se prev-h))
         (style (or style previous-style))
         (curr-h (or (gethash style rebox-style-hash)
                     (signal 'rebox-invalid-style-error (list (format "style (%s) is unknown" style)))))
         (curr-regexp1 (gethash :regexp1 curr-h))
         (curr-regexp2 (gethash :regexp2 curr-h))
         (curr-regexp3 (gethash :regexp3 curr-h))
         (curr-merge-nw (gethash :merge-nw curr-h))
         (curr-merge-sw (gethash :merge-sw curr-h))
         (curr-nw (gethash :nw curr-h))
         (curr-nn (gethash :nn curr-h))
         (curr-ne (gethash :ne curr-h))
         (curr-ww (gethash :ww curr-h))
         (curr-ee (gethash :ee curr-h))
         (curr-sw (gethash :sw curr-h))
         (curr-ss (gethash :ss curr-h))
         (curr-se (gethash :se curr-h))
         (unindent-count (+ previous-margin (length previous-ww)))
         (border-margin (+ previous-margin (if
                                               ;; if there is only a left
                                               ;; border, take spaces at EOL
                                               ;; trimming into account
                                               (and previous-ee
                                                    previous-nn)
                                               (length previous-ww)
                                             (length (rebox-rstrip previous-ww)))))
         title-plist)


    (unless quiet
      (if (= previous-style style)
          (message "Keeping style \"%s\"%s."
                   style
                   (concat (when refill
                             ", refilling")
                           (when before-insp-func
                             ", running before-insp-func")
                           (when mod-func
                             ", running mod-func")
                           (when after-insp-func
                             ", running after-insp-func")
                           ))
        (message "Style: %d -> %d" (or previous-style 0) style)))

    ;; attempt to box only spaces
    (goto-char (point-min))
    (skip-chars-forward " \t\n")
    (when (not (< (point) (point-max)))
      (signal 'rebox-error '("Cannot box region consisting of only spaces.")))

    ;; untabify, but preserve position of marked-point
    (let ((marked-col (progn
                        (goto-char marked-point)
                        (current-column))))
      (untabify (point-min) (point-max))
      (goto-char marked-point)
      (unless (= (current-column) marked-col)
        (move-to-column marked-col)
        (set-marker marked-point (point))))

    (catch 'rebox-engine-done
      ;; inspect the box
      (when before-insp-func
        (if (functionp before-insp-func)
            (funcall before-insp-func)
          (error "%s is not a function" before-insp-func)))

      ;; if we don't set the insertion type, and it gets pulled to the beginning
      ;; of line, it will get stuck there.
      (goto-char marked-point)
      (when (eolp)
        (set-marker-insertion-type marked-point t))

      ;; Remove all previous comment marks.
      (unless (eq previous-style 111)
        (goto-char marked-point)
        (setq title-plist (rebox-unbuild prev-h)))

      ;; don't lose title when we are traversing style loop
      (if (memq last-command '(rebox-cycle rebox-dwim rebox-refill))
          (setq title-plist (gethash :title-plist (rebox-cache)))
        (puthash :title-plist title-plist (rebox-cache)))

      (unless rebox-keep-blank-lines
        ;; Remove all spurious whitespace.
        (goto-char (point-min))
        (while (re-search-forward " +$" nil t)
          (replace-match "" t t))
        (goto-char (point-min))
        (delete-char (- (skip-chars-forward "\n")))
        (goto-char (point-max))
        (when (= (preceding-char) ?\n)
          (forward-char -1))
        (delete-char (- (skip-chars-backward "\n")))
        (goto-char (point-min))
        (while (re-search-forward "\n\n\n+" nil t)
          (replace-match "\n\n" t t)))

      ;; Move all the text rigidly to the left for insuring that the left
      ;; margin stays at the same place.
      (unless (zerop unindent-count)
        (goto-char (point-min))
        (while (not (eobp))
          (delete-region (point) (progn
                                   (skip-chars-forward " " (+ (point) unindent-count))
                                   (point)))
          (forward-line 1)))

      ;; modify the box
      (when mod-func
        (if (functionp mod-func)
            (let (
                  ;; in case editing functions check major-mode for
                  ;; indentation, etc.
                  (major-mode 'fundamental-mode)
                  (fill-column (rebox-get-fill-column curr-ww curr-ee previous-margin title-plist)))
              (funcall mod-func))
          (error "%s is not a function" mod-func)))

      ;; empty boxes are allowed if we are just doing an autofill, otherwise
      ;; we can never start a comment, as the "space" after `comment-start'
      ;; will trigger auto-fill-function.
      (when (not (eq refill 'auto-fill))
        ;; if the narrowed box is all blanks now, delete it and quit
        (goto-char (point-min))
        (skip-chars-forward " \t\n")
        (when (eq (point) (point-max))
          (delete-region (point-min) (point-max))
          (throw 'rebox-engine-done t)))

      ;; Possibly refill, then build the comment box.
      (let ((indent-tabs-mode nil))
        (rebox-build refill previous-margin curr-h marked-point move-point title-plist))
      (setq clean-undo-list t))

    ;; Retabify to the left only (adapted from tabify.el).
    (if indent-tabs-mode
        (let ((marked-col (progn
                            ;; bug # 7946 workaround
                            ;; should be fixed in next emacs23 release after 23.2.1
                            (set-buffer (current-buffer))
                            (goto-char marked-point)
                            (current-column))))
          (goto-char (point-min))
          (while (re-search-forward "^[ \t][ \t]+" nil t)
            (let ((column (current-column)))
              (delete-region (match-beginning 0) (point))
              (indent-to column)))
          (goto-char marked-point)
          (unless (= (current-column) marked-col)
            (move-to-column marked-col)
            (set-marker marked-point (point))))
      (goto-char marked-point))

    (when after-insp-func
      (if (functionp after-insp-func)
          (funcall after-insp-func)
        (error "%s is not a function" after-insp-func)))

    ;; Remove all intermediate boundaries from the undo list.
    (unless (or (not clean-undo-list)
                (eq buffer-undo-list undo-list))
      (let ((cursor buffer-undo-list))
        (while (not (eq (cdr cursor) undo-list))
          (if (car (cdr cursor))
              (setq cursor (cdr cursor))
            (rplacd cursor (cdr (cdr cursor)))))))))


;; Classification of boxes (and also, construction data).

(defun rebox-find-box-beg-end (comment-only)
  (let ((orig-p (point))
        (has-comment-definition (and comment-start
                                     (not (zerop (length comment-start)))))
        comment-b comment-e)
    (if has-comment-definition
        ;; are we inside a comment?
        (when (progn
                (goto-char (point-at-eol))
                (rebox-line-is-comment :move-multiline nil))
          (setq comment-b (point-at-bol))
          (goto-char (point-at-eol 0))  ; previous line's eol
          (catch 'roo
            (while (rebox-line-is-comment :throw-label 'roo)
              (setq comment-b (point-at-bol))
              (if (eq (point-at-bol) (point-min))
                  (throw 'roo nil)
                (goto-char (point-at-eol 0)))))
          (goto-char orig-p)
          (end-of-line 2)
          (setq comment-e (point-at-bol))
          (catch 'roo
            (while (rebox-line-is-comment :throw-label 'roo
                                           :move-multiline nil)
              (goto-char (point-at-eol 2))
              (setq comment-e (point-at-bol))
              (when (and (eq (point) (point-max)) ; hit eob
                         (not (eq (point) (point-at-bol)))) ; eob is *not* newline
                ;; we need to end on a blank line for rebox to work properly,
                ;; we don't call `newline' to avoid refilling.
                (insert "\n")
                (setq comment-e (point))
                (throw 'roo nil))))))
    (goto-char orig-p)
    (when  (and (not comment-b)
                (not comment-e)
                (not comment-only))
      ;; no comment format defined
      (setq comment-b (if (search-backward-regexp rebox-blank-line-regexp nil t)
                          (prog1
                              (point-at-bol 2)
                            (goto-char orig-p))
                        (point-min)))
      (setq comment-e (if (search-forward-regexp rebox-blank-line-regexp nil t)
                          (prog1
                              (point-at-bol 1)
                            (goto-char orig-p))
                        ;; we need a blank line at the end of the narrow
                        (goto-char (point-max))
                        (insert "\n")
                        (point))))
    (list comment-b comment-e)))

(defun* rebox-find-and-narrow (&key (comment-only t) (try-whole-box nil))
  "Find the limits of the block of comments following or enclosing
the cursor, or return an error if the cursor is not within such a
block of comments.  Extend it as far as possible in both
directions, then narrow the buffer around it."
  (let ((orig-p (point))
        comment-b comment-e
        temp
        got-it)
    (setq temp (rebox-find-box-beg-end comment-only))
    (setq comment-b (car temp))
    (setq comment-e (cadr temp))
    (if (or (not comment-b)
            (not comment-e))
        (when try-whole-box
          (progn
            (goto-char orig-p)
            (when (and (bolp)
                       (not (bobp)))
              (progn
                (backward-char)
                (setq temp (rebox-find-box-beg-end comment-only))
                (setq comment-b (car temp))
                (setq comment-e (cadr temp))
                (when (and comment-b
                           comment-e
                           (= comment-e orig-p))
                  (setq got-it t))))))
      (setq got-it t))
    (if got-it
        (narrow-to-region comment-b comment-e)
      (signal 'rebox-comment-not-found-error nil))))

(defun rebox-guess-language (my-comment-start-str)
  "Guess the language in use based on the comment-start character.

returns guess as single digit."
  (setq my-comment-start-str
        (cond ((stringp my-comment-start-str)
               my-comment-start-str)
              ((not my-comment-start-str)
               "")
              (t
               (error "invalid my-comment-start-str: %s" my-comment-start-str))))
  (let ((language 1)
        (index (1- (length rebox-regexp-start))))
    (while (not (zerop index))
      (if (string-match (aref rebox-regexp-start index) my-comment-start-str)
          (setq language index
                index 0)
        (setq index (1- index))))
    language))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                ;;
;; Some caching is possible here to remember the last found style ;;
;; and try that first, etc.                                       ;;
;;                                                                ;;
;; However, computing time is cheap, my time is not.              ;;
;;                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rebox-guess-style ()
  "Guess the current box style from the text in the whole (narrowed) buffer."
  (let (best-style best-weight)
    (maphash
     (lambda (style hash)
       ;; Let's try all styles in turn.  A style has to match exactly to be
       ;; eligible.  More heavy is a style, more prone it is to be retained.
       (let* ((weight (gethash :weight hash))
              (regexp1 (if (memq 'top-title rebox-allowances)
                           (gethash :regexp1-title hash)
                         (gethash :regexp1 hash)))
              (regexp2 (gethash :regexp2 hash))
              (regexp3 (if (memq 'bottom-title rebox-allowances)
                           (gethash :regexp3-title hash)
                         (gethash :regexp3 hash)))
              (min-lines (length (remove nil (list regexp1 regexp2 regexp3))))
              (limit (cond
                      ((and best-weight (<= weight best-weight))
                       nil)
                      ((< (count-lines (point-min) (point-max)) min-lines)
                       nil)
                      ((not regexp3)
                       (point-max))
                      ((progn (goto-char (point-max))
                              (forward-line -1)
                              (looking-at regexp3))
                       (point)))))
         (when limit
           (goto-char (point-min))
           (cond ((not regexp1))
                 ((looking-at regexp1) (goto-char (match-end 0)))
                 (t (setq limit nil)))
           (when (and limit regexp2)
             (while (and (< (point) limit) (looking-at regexp2))
               (goto-char (match-end 0)))
             (unless (= (point) limit)
               (setq limit nil)))
           (when limit
             (setq best-style style
                   best-weight weight)))))
     rebox-style-hash)
    best-style))

(defun* rebox-line-is-comment (&key (move-multiline t)
                                     (throw-label nil))
  (unless rebox-comment-vars-normalized
    (comment-normalize-vars)
    (setq rebox-comment-vars-normalized t))
  (let ((bare-comment-end (and comment-end (rebox-rstrip (rebox-lstrip comment-end))))
        (bare-comment-start (and comment-start (rebox-rstrip (rebox-lstrip comment-start))))
        (starting-bol (point-at-bol))
        comment-start-pos
        temp)
    (end-of-line 1)
    (setq comment-start-pos (comment-beginning))
    (unless comment-start-pos
      ;; C-style comments, we could be in the white space past comment end
      (when (and (stringp comment-end)
                 (not (equal comment-end "")))
        (goto-char (point-at-eol))
        (search-backward-regexp (concat (regexp-quote bare-comment-end)
                                        "[ \t]*$")
                                (point-at-bol)
                                t)
        (setq comment-start-pos (comment-beginning))))
    ;;
    ;; detect mid-line comments
    ;;
    ;; consider doxygen header row which is two consecutive comments:
    ;;
    ;;     "/********//**"
    (loop
     with temp = comment-start-pos
     while temp
     do (progn
          (goto-char temp)
          (skip-chars-backward " \t")
          (if (bolp)
              (return)
            (backward-char)
            (when (setq temp (comment-beginning))
              (setq comment-start-pos temp)))))

    (when comment-start-pos
      (unless (= (point) (point-at-bol))
        (signal 'rebox-mid-line-comment-found nil)))
    (if (and (< (point) starting-bol)
             (not move-multiline))
        (goto-char starting-bol))
    comment-start-pos))


(defun* rebox-regexp-quote (string &key (rstrip t) (lstrip t) (match-trailing-spaces t))
"Return a regexp matching STRING without its surrounding space,
maybe followed by spaces or tabs.  If STRING is nil, return the
empty regexp.

With no-rstrip specified, don't strip spaces to the right."
  (cond ((not string) "")
        ((not (stringp string))
         (error "debug me, rebox-regexp-quote got non-string %s" string))
        (t
         (and rstrip
              (setq string (rebox-rstrip string)))
         (and lstrip
              (setq string (rebox-lstrip string)))
         (setq string (regexp-quote string))
         (if match-trailing-spaces
             (concat string "[ \t]*")
           string))))


;; -lw-
;; when the box is just one character wide:

          ;;;;;;;
          ;; a ;;
          ;;;;;;;

;; if we make the regex 2 characters wide, it won't match.

(defun rebox-regexp-ruler (character)
  "Return a regexp matching one or more repetitions of CHARACTER,
maybe followed by spaces or tabs.  Is CHARACTER is nil, return
the empty regexp."
  (if character
      (concat (regexp-quote (string character)) "+[ \t]*")
    ""))


(defun rebox-rstrip (string)
  "Return string with trailing spaces removed."
  (while (and (> (length string) 0)
              (memq (aref string (1- (length string))) '(?  ?\t)))
    (setq string (substring string 0 (1- (length string)))))
  string)

(defun rebox-lstrip (string)
  "Return string with leading spaces removed."
  (while (and (> (length string) 0)
              (memq (aref string 0) '(?  ?\t)))
    (setq string (substring string 1)))
  string)

(defun rebox-get-fallback (&optional saved-function)
  "return fallback function found"
  (if rebox-mode
      (let* ((rebox-mode nil)
             (key (this-single-command-keys))
             (command (key-binding key)))
        (when (and (equal key [backspace])
                   (null command))
          (setq command (key-binding (kbd "DEL"))))
        command)
    (eval saved-function)))


(defun rebox-call-command (command)
  "call command without `rebox-mode-map'.

This allows rebox to play nicely with other commands who want to introspect key-binding."
  (let ((rebox-mode nil))
    (call-interactively command)))

(defun rebox-document-binding (&optional saved-function)
  (concat
   "Works by performing reboxed behaviour when invoked in
a box context.  Call the fallback command"
   (when (eq this-command 'describe-key)
     (let ((fallback (rebox-get-fallback)))
       (when fallback
         (if (eq fallback (eval saved-function))
          (format ", which in this case is \"%s\" defined by `%s'"
             (eval saved-function)
             saved-function)
         (format ", which in this case is `%s'" (rebox-get-fallback))))))
   "."))


;; Reconstruction of boxes.

(defun rebox-unbuild (style-h)
  (let* ((merge-nw (gethash :merge-nw style-h))
         (merge-sw (gethash :merge-sw style-h))
         (nw (gethash :nw style-h))
         (nn (gethash :nn style-h))
         (ne (gethash :ne style-h))
         (ww (gethash :ww style-h))
         (ee (gethash :ee style-h))
         (sw (gethash :sw style-h))
         (ss (gethash :ss style-h))
         (se (gethash :se style-h))
         (nw-regexp (and nw (regexp-quote nw)))
         (ww-regexp (and ww (regexp-quote ww)))
         (sw-regexp (and sw (regexp-quote sw)))
         (top-title "")
         (bottom-title "")
         top-title-start
         bottom-title-start
         max-title-len
         point-in-border
         limit-m)
    (cond ((and (or nw nn ne)
                (= (line-number-at-pos) 1))
           (setq point-in-border (cons
                                 'top
                                 (max (- (current-column) (length nw))
                                      0))))
          ((and (or sw ss se)
                (= (line-number-at-pos) (save-excursion
                                          (goto-char (point-max))
                                          (forward-line -1)
                                          (line-number-at-pos))))
           (setq point-in-border (cons
                                 'bottom
                                 (max (- (current-column) (length sw))
                                      0)))))
    ;; When there is no right border, on an empty line, left border's trailing
    ;; spaces may be be trimmed.  People don't like empty spaces at EOL.
    (when (and ww-regexp
               (not ee))
      (string-match "\\`\\(.*?\\)\\(\\s-*\\)\\'" ww-regexp)
      (setq ww-regexp (concat (match-string 1 ww-regexp)
                              "\\(?:"
                              (match-string 2 ww-regexp)
                              "\\)?")))
    ;; Clean up first line.
    (goto-char (point-min))
    (end-of-line)
    (skip-chars-backward " \t")
    (when ne
      (let ((start (- (point) (length ne))))
        (when (and (>= start (point-min))
                   (string-equal ne (buffer-substring start (point))))
          (delete-region (point) (- (point) (length ne))))))
    (forward-line 0)
    (when (and nw-regexp (search-forward-regexp nw-regexp (point-at-eol)))
      (replace-match (make-string (- (match-end 0) (match-beginning 0))
                                  ? )))
    (setq top-title-start (point))
    (when nn
      (let ((len (save-excursion
                   (skip-chars-forward (char-to-string nn)))))
        (insert (make-string len ? ))
        (delete-region (point) (+ (point) len)))
      (goto-char (point-at-eol))
      (delete-region (point) (+ (point) (skip-chars-backward (char-to-string nn)))))

    ;; Clear the top border line.
    ;;
    ;; If there was any top border, clear it.  I used to try to keep
    ;; text on the top border like the C-style comments:
    ;;
    ;; /* {Title}
    ;;  *  text
    ;;  */
    ;;
    ;; However, when you unbox and then rebox, it becomes unclear whether
    ;; {Title} should be in the top border or the box itself.

    (when (or nw-regexp nn ne)
      (goto-char (point-at-bol))
      (if (looking-at-p rebox-blank-line-regexp)
          (delete-region (point) (point-at-bol 2))
        (if (memq 'top-title rebox-allowances)
            (progn
              (goto-char (point-at-eol))
              (skip-chars-backward " \t")
              (setq top-title (buffer-substring-no-properties top-title-start (point)))
              (goto-char (point-at-bol))
              (delete-region (point) (point-at-bol 2)))
          (error "Top border should be clear now.  Debug me."))))

    ;; Clean up last line.
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (when se
      (let ((start (- (point) (length se))))
        (when (and (>= start (point-min))
                   (string-equal se (buffer-substring start (point))))
          (delete-region (point) (- (point) (length se))))))
    (forward-line 0)
    (when (and sw-regexp (search-forward-regexp sw-regexp (point-at-eol)))
      (replace-match (make-string (- (match-end 0) (match-beginning 0))
                                  ? )))
    (setq bottom-title-start (point))
    (when ss
      (let ((len (save-excursion
                   (skip-chars-forward (char-to-string ss)))))
        (insert (make-string len ? ))
        (delete-region (point) (+ (point) len)))
      (goto-char (point-at-eol))
      (delete-region (point) (+ (point) (skip-chars-backward (char-to-string ss)))))
    (setq limit-m (make-marker))
    ;; if there was any bottom border, and it's now just blanks, delete bottom line
    (if (or se sw-regexp ss)
        (progn
          (goto-char (point-at-bol))
          (if (looking-at-p rebox-blank-line-regexp)
              (delete-region (point) (point-at-bol 2))
            (if (and (memq 'bottom-title rebox-allowances)
                     (not merge-sw))
                (progn
                  (goto-char (point-at-eol))
                  (skip-chars-backward " \t")
                  (setq bottom-title (buffer-substring-no-properties bottom-title-start (point)))
                  (goto-char (point-at-bol))
                  (delete-region (point) (point-at-bol 2)))
              (error "Bottom border should be clear now.  Debug me.")))
          (set-marker limit-m (point)))
      (set-marker limit-m (1+ (point))))
    ;; Clean up all lines.
    (goto-char (point-min))
    (while (< (point) limit-m)
      (end-of-line)
      (skip-chars-backward " \t")
      (when ee
        (let ((start (- (point) (length ee))))
          (when (and (>= start (point-min))
                     (string-equal ee (buffer-substring start (point))))
            (delete-region (point) (- (point) (length ee))))))
      (beginning-of-line)
      (when (and ww-regexp (search-forward-regexp ww-regexp (point-at-eol)))
          (replace-match (make-string (- (match-end 0) (match-beginning 0))
                                      ? )))
      (forward-line 1))
    (setq max-title-len (max (length top-title)
                             (length bottom-title)))
    (list :top-title top-title :bottom-title bottom-title
          :max-len max-title-len
          :point-in-border point-in-border)))

;; -lw- here
(defun rebox-build (refill margin style-h marked-point move-point title-plist)
"After refilling it if REFILL is not nil, while respecting a left MARGIN,
put the narrowed buffer back into a boxed comment according to
box STYLE."
  (let* ((merge-nw (gethash :merge-nw style-h))
         (merge-sw (gethash :merge-sw style-h))
         (nw (gethash :nw style-h))
         (nn (gethash :nn style-h))
         (ne (gethash :ne style-h))
         (ww (gethash :ww style-h))
         (ee (gethash :ee style-h))
         (sw (gethash :sw style-h))
         (ss (gethash :ss style-h))
         (se (gethash :se style-h))
         (top-title (or (plist-get title-plist :top-title) ""))
         (bottom-title (or (plist-get title-plist :bottom-title) ""))
         (title-max-len (or (plist-get title-plist :max-len) 0))
         (point-in-border (plist-get title-plist :point-in-border))
         right-margin
         count-trailing-spaces
         limit-m
         temp
         temp-mod
         )

    (setq limit-m (point-max-marker))

    ;; Possibly refill, and adjust margins to account for left inserts.
    (if refill
        (let (;;;; whatever adaptive filling should take care of this
              (fill-prefix nil)
              ;; In a box, we don't want mode-specific fill functions
              (fill-paragraph-function (if (or (not comment-start)
                                               (equal "" comment-start)
                                               (memq major-mode rebox-hybrid-major-modes))
                                           fill-paragraph-function
                                         nil))
              (fill-column (rebox-get-fill-column ww ee margin title-plist))
              ;; since we are filling as "text" we don't want any programming
              ;; definitions of sentence-end to exist.
              (sentence-end nil)
              ;; some filling functions will consult major-mode for filling
              ;; advice, we don't want this since we've removed the
              ;; comment-starts.
              (major-mode 'fundamental-mode)
              (comment-auto-fill-only-comments nil))
          (if (eq refill 'auto-fill)
              (progn
                (setq count-trailing-spaces marked-point)
                (goto-char marked-point)
                (rebox-call-alternate-fill-function 'fundamental))
            (setq count-trailing-spaces nil)
            (fill-region (point-min) limit-m)))
      (setq count-trailing-spaces marked-point))
    (setq right-margin (max (+ (max (rebox-right-margin :count-trailing-spaces count-trailing-spaces
                                                        :pad-sentence-end rebox-pad-sentence-end)
                                    title-max-len)
                               (length ww)
                               margin)
                            ;; minimum box width is 1
                            (1+ (length ww))
                            (if (and rebox-min-fill-column
                                     (numberp rebox-min-fill-column))
                                (- rebox-min-fill-column (length ee))
                              0)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; we need to append ";" to a title that's propping up the box size or ;;
    ;; else we won't be able to recognize the box with regexp[13]-title    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (when (and nn
               (>= (+ (length top-title)
                      (length nw)
                      margin) right-margin))
      (setq top-title (concat top-title (vector nn))
            temp-mod t))
    (when (and ss
               (>= (+ (length bottom-title)
                      (length sw)
                      margin) right-margin))
      (setq bottom-title (concat bottom-title (vector ss))
            temp-mod t))
    (when temp-mod (incf right-margin))

    ;; Construct the top line.
    (goto-char (point-min))
    (cond (merge-nw
           (insert (make-string (- margin (current-column)) ? )
                   nw
                   top-title
                   "\n"))
          ((or nw nn ne)
           (indent-to margin)
           (when nw
             (insert nw))
           (when (or nn ne)
             (string-match "\\`\\([ \t]*\\)" top-title)
             (insert (replace-match (make-string (- (match-end 0) (match-beginning 0))
                                                 (or nn ? ))
                              nil
                              t
                              top-title))
             (insert (make-string (- right-margin (current-column))
                                  (or nn ? )))
             (when ne
               (insert ne)))
           (insert "\n")))
    ;; Construct all middle lines.
    (while (< (point) limit-m)
      (when (or ww ee (/= (following-char) ?\n))
        (insert (make-string (- margin (current-column)) ? ))
        (when ww
          (insert ww))
        (when ee
          (end-of-line)
          (indent-to right-margin)
          (insert ee)))
      (forward-line 1))
    ;; Construct the bottom line.
    (when (or sw ss se)
      (indent-to margin)
      (when sw
        (insert sw))
      (when (or ss se)
        (string-match "\\`\\([ \t]*\\)" bottom-title)
        (insert (replace-match (make-string (- (match-end 0) (match-beginning 0))
                                            (or ss ? ))
                               nil
                               t
                               bottom-title))
        (insert (make-string (- right-margin (current-column))
                             (or ss ? )))
        (when se
          (insert se)))
      (insert "\n"))

    (goto-char marked-point)
    (when move-point
      (cond ((eq (car point-in-border) 'top)
             (goto-char (point-min))
             (move-to-column (+ (cdr point-in-border)
                                (length nw))))
            ((eq (car point-in-border) 'bottom)
             (goto-char (point-max))
             (forward-line -1)
             (if (or sw ss se)
                 (progn
                   (move-to-column (+ (cdr point-in-border)
                                      (length sw))))
               (move-to-column right-margin)
               (skip-chars-backward " \t")))
            (t
             ;; figure out if we've moved the marked-point to an unreasonable position
             (let* ((my-col (current-column))
                    (my-line (line-number-at-pos))
                    (point-max-line (line-number-at-pos (point-max)))
                    (min-line (if (or merge-nw nw nn ne)
                                  2
                                1))
                    (max-line (if (or merge-sw sw ss se)
                                  (- point-max-line 2)
                                (- point-max-line 1)))
                    (min-col (+ margin (length ww)))
                    (max-col right-margin)
                    )
               ;; move vertically
               (cond ((< my-line min-line)
                      (forward-line 1))
                     ((> my-line max-line)
                      (goto-char (point-min))
                      (forward-line (1- max-line))))
               ;; move horizontally
               (cond ((< my-col min-col)
                      (beginning-of-line)
                      (forward-char min-col)
                      (setq temp (skip-chars-forward " \t"))
                      ;; nothing but space on this line
                      (when (looking-at-p (concat (regexp-quote (if ee
                                                                    (rebox-lstrip ee)
                                                                  ""))
                                                  "[ \t]*$"))
                        (backward-char temp)))
                     ((> my-col max-col)
                      (goto-char (+ (point-at-bol) max-col))
                      (setq temp (skip-chars-backward " \t"))
                      (when (< (current-column) min-col)
                        (beginning-of-line)
                        (forward-char min-col)))))))
      (unless (= (point) marked-point)
        (set-marker marked-point (point))))))


(defun rebox-left-margin ()
"Return the minimum value of the left margin of all lines, or -1 if
all lines are empty."
  (let ((margin -1))
    (goto-char (point-min))
    (while (and (not (zerop margin)) (not (eobp)))
      (skip-chars-forward " \t")
      (let ((column (current-column)))
        (and (not (= (following-char) ?\n))
             (or (< margin 0) (< column margin))
             (setq margin column)))
      (forward-line 1))
    margin))


(defun* rebox-right-margin (&key (count-trailing-spaces t) pad-sentence-end)
  "Return the maximum value of the right margin of all lines.

COUNT-TRAILING-SPACES may be a marker, in which case only spaces
on that line, up to that marker will be counted.  nil to never
count trailing spaces or t to always count.

"
  (let ((margin 0)
        (sentence-pad-count (if sentence-end-double-space
                                2
                              1))
        sentence-end-found)
    (when (markerp count-trailing-spaces)
      (when (progn (goto-char count-trailing-spaces)
                   (looking-at "[ \t]*$"))
        (setq margin (current-column)))
      (setq count-trailing-spaces nil))
    (goto-char (point-min))
    (while (not (eobp))
      (if (and pad-sentence-end
               (re-search-forward (concat (sentence-end)
                                          "$") (point-at-eol) t))
          (setq sentence-end-found t)
        (end-of-line)
        (setq sentence-end-found nil))
      (unless count-trailing-spaces
        (skip-chars-backward " \t"))
      (setq margin (max margin
                        (+ (current-column)
                           (if sentence-end-found
                               (max
                                (- sentence-pad-count (skip-chars-forward " "))
                                0)
                             0))))
      (forward-line 1))
    (unless count-trailing-spaces
      ;; we iterate through lines twice to avoid excess damage to markers
      (goto-char (point-min))
      (while (not (eobp))
        (end-of-line)
        (when (> (current-column) margin)
          (delete-char (- margin (current-column))))
        (forward-line 1)))
    margin))

(defun rebox-get-newline-indent-function ()
  "return preferred newline-and-indent function"
  (setq rebox-newline-indent-command
        (or rebox-newline-indent-command
            (cdr (assq major-mode rebox-newline-indent-function-alist))
            rebox-newline-indent-command)))

(defun rebox-get-fill-column (ww ee margin title-plist)
  (max (- (max fill-column
               (if (and rebox-min-fill-column
                        (numberp rebox-min-fill-column))
                   rebox-min-fill-column
                 0))
          (length ww)
          (length ee)
          margin)
       (or (plist-get title-plist :max-len)
           0)))

(defun rebox-propertize-style-loop (can-style &optional loop c-start)
  (setq loop    (or loop
                    rebox-style-loop))
  (setq c-start (or c-start
                    comment-start))
  (concat "("
          (mapconcat
           (lambda (item)
             (setq item (rebox-get-canonical-style item c-start))
             (cond ((eq item can-style)
                    (concat
                     (propertize ">" 'face 'rebox-style-chevron-face)
                     (propertize (format "%s" item) 'face 'rebox-style-face)
                     (propertize "<" 'face 'rebox-style-chevron-face)))
                   ((null (gethash item rebox-style-hash))
                    (propertize (format "%s" item) 'face 'rebox-disabled-style-face))
                   (t
                    (format "%s" item))))
           loop
           " ")
          ")"))

(defun rebox-save-env ()
  "save some settings"
  (unless (gethash :rebox-save-env-done (rebox-cache))
    (let (env)
      (mapc (lambda (var)
              (push (cons var (symbol-value var)) env))
            rebox-save-env-vars)
      (puthash :rebox-save-env-alist env (rebox-cache)))
    (puthash :rebox-save-env-done t (rebox-cache))))

(defun rebox-restore-env ()
  "load some settings"
  (let ((env (gethash :rebox-save-env-alist (rebox-cache))))
    (mapc (lambda (var)
            (set var (cdr (assq var env))))
          rebox-save-env-vars)))

;;; Initialize the internal structures.

(rebox-register-all-templates)

(defun rebox-promote-map ()
  (let ((rebox-mode-binding (assq 'rebox-mode minor-mode-map-alist)))
    (setq minor-mode-map-alist (delete rebox-mode-binding minor-mode-map-alist))
    (push rebox-mode-binding minor-mode-map-alist)))

;;; we need priority over paredit, which dumbly maps "DEL"
(eval-after-load "paredit"
  '(rebox-promote-map))



(provide 'rebox2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rebox2.el ends here
