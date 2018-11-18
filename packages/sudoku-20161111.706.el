;;; sudoku.el --- Simple sudoku game, can download puzzles

;; Copyright (C) 2009-2011,2015,2016 by Zajcev Evgeny

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Oct 29 21:55:35 2009
;; Keywords: games
;; Package-Version: 20161111.706
;; Package-Requires: ((emacs "24.4"))
;; Version: 1.4

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Idea taken from http://www.columbia.edu/~jr2075/sudoku.el
;;
;; Totally rewritten.
;;
;; Features:
;;
;;   * Custom puzzle editor
;;   * Printing puzzles (TeX source generator)
;;   * Puzzle Solver (simple heuristics + trial and error)
;;   * Auto-insert assistance (S1, H1, CC i.e. Constrained Cell)
;;   * Step-by-step puzzle deduce (using S1, H1 and CC heuristics)
;;   * Nifty pencils (assist for Nishio, Cycles, Loops)
;;   * Undo/Redo operations
;;   * Optimal (single solution) puzzles generator (very
;;     straightforward, but usable)
;;   * Board downloader (from websudoku.com and menneske.no)
;;   * Save/Load puzzles
;;   * Collection of built-in puzzles

;; Usage
;; ~~~~~
;; To start using sudoku, add one of:
;;
;;   (require 'sudoku)
;;
;; or, more preferable way:
;;
;;   (autoload 'sudoku "sudoku" "Start playing sudoku." t)
;;
;; to your init.el file.
;;
;; Start playing with M-x sudoku RET

;; Customization
;; ~~~~~~~~~~~~~
;; You might want to customize next variables:
;;
;;   * `sudoku-level'     - Level for puzzles
;;   * `sudoku-download'  - Non-nil to download puzzles
;;   * `sudoku-download-source' - Source to download puzzles from
;;   * `sudoku-modeline-show-values' - Show possible values in modeline
;;
;; To create custom puzzle (snatched from newspaper or magazine) do:
;; M-x sudoku-custom RET and follow instructions.
;;
;; If you use `desktop.el' you might want next code in your init.el:
;;
;;   (push 'sudoku-custom-puzzles desktop-globals-to-save)
;;   (push 'sudoku-solved-puzzles desktop-globals-to-save)

;; Deduce/Auto-insert
;; ~~~~~~~~~~~~~~~~~~
;; This file includes simple board deducing heuristics, that might
;; help not to get bored with filling simple values.  Deduce method
;; `sudoku-insert-hidden-single' is pretty smart, it can solve almost
;; every `medium' sudoku puzzle in conjunction with
;; `sudoku-insert-single'.  To enable auto-insert feature, use:
;;
;;   (sudoku-turn-on-autoinsert)
;;
;; or use method suitable for autoloading:
;;
;;   (setq sudoku-autoinsert-mode t)
;;   (add-hook 'sudoku-after-change-hook 'sudoku-autoinsert)
;;
;; You might also want to customize `sudoku-deduce-methods' variable.
;;
;; Auto-insert is useful for sudoku hackers, who dislikes to insert
;; pretty trivial values into `evil' puzzles, but want to focus on
;; real problems.  You can toggle auto-insert mode while solving
;; puzzle with `a' key.
;;
;; To deduce next step using auto-inserters use `d' key.

;; Pencils
;; ~~~~~~~
;; Sometimes, especially when solving complex puzzle, you might want to try
;; out some values with pencil to find a contradiction and then return
;; to pen.  You can do it with `p' command.  Use `p' to enable pencil,
;; and `C-u p' to return back to pen.  There are only two pencils
;; available.  Current pencil is displayed on screen.

;; Printing
;; ~~~~~~~~
;; To print puzzle you will need sudoku TeX package (available at
;; http://ctan.math.utah.edu/tex-archive/macros/latex/contrib/sudoku/)
;;
;; press `P' while solving puzzle and it will generate TeX file.

;; Saving/Loading Puzzles
;; ~~~~~~~~~~~~~~~~~~~~~~
;; You can save/load puzzles to/from sdk files (SadMan Sudoku file
;; format, http://www.sadmansoftware.com/sudoku/faq19.htm) with
;; `sudoku-save-puzzle' (C-x C-s) and `sudoku-load-puzzle' commands.
;;
;; To make Emacs load puzzle with `C-x C-f' add next to init.el
;;
;;   (add-to-list 'auto-mode-alist '("\\.sdk\\'" . sudoku-mode))

;;; History:
;;  ~~~~~~~
;;
;; Version 1.5: [TODO]
;;   - Puzzle timer
;;
;; Version 1.4:
;;   - Initial port to GNU Emacs

;;; Code:


(require 'cl-lib)
(require 'easymenu)
(eval-when-compile
  (require 'cl)) ;; for defsetf

;;{{{ Custom variables

(defgroup sudoku nil
  "Sudoku - web-enabled puzzle game"
  :group  'games
  :version "1.4"
  :link '(url-link :tag "sudoku.el sources at github"
                   "http://github.com/zevlg/sudoku.el")
  :prefix "sudoku-")

(defcustom sudoku-level 'hard
  "*Level of difficulty for sudoku."
  :type '(choice (const :tag "Easy" easy)
                 (const :tag "Medium" medium)
                 (const :tag "Hard" hard)
                 (const :tag "Evil" evil))
  :group 'sudoku)

(defcustom sudoku-download nil
  "*Should sudoku download puzzles from the web?"
  :type  'boolean
  :group 'sudoku)

(defcustom sudoku-download-source 'menneske
  "*Source to download sudoku from."
  :type '(choice (const :tag "menneske.no" menneske)
                 (const :tag "websudoku.com" websudoku))
  :group 'sudoku)

(defface sudoku-face
  '((t (:inherit default :height 2.0)))
  "Base face used by sudoku."
  :group 'sudoku)

(defface sudoku-orig-value-face
  `((t (:inherit sudoku-face)))
  "Face for original values in sudoku field."
  :group 'sudoku)

(defface sudoku-value-face
  `((((type tty) (class color))
     (:foreground "cyan" :inherit sudoku-orig-value-face))
    (((class color) (background light))
     (:foreground "darkblue" :inherit sudoku-orig-value-face))
    (((class color) (background dark))
     (:foreground "blue" :inherit sudoku-orig-value-face))
    (t (:inherit sudoku-orig-value-face)))
  "Face for values you've inserted."
  :group 'sudoku)

(defface sudoku-value-pencil-1-face
  `((((type tty) (class color))
     (:foreground "slategray" :inherit sudoku-face))
    (((class color) (background light))
     (:foreground "slategray" :inherit sudoku-face))
    (((class color) (background dark))
     (:foreground "slategray" :inherit sudoku-face))
    (t (:inverse-video t :inherit sudoku-face)))
  "Face used for first pencil values."
  :group 'sudoku)

(defface sudoku-value-pencil-2-face
  `((((type tty) (class color))
     (:foreground "darkgrey" :inherit sudoku-face))
    (((class color) (background light))
     (:foreground "darkgrey" :inherit sudoku-face))
    (((class color) (background dark))
     (:foreground "darkgrey" :inherit sudoku-face))
    (t (:inverse-video t :inherit sudoku-face)))
  "Face used for second pencil values."
  :group 'sudoku)

(defface sudoku-autovalue-face
  `((((type tty) (class color))
     (:foreground "cyan" :inherit sudoku-orig-value-face))
    (((class color) (background light))
     (:foreground "darkviolet" :inherit sudoku-orig-value-face))
    (((class color) (background dark))
     (:foreground "mediumpurple" :inherit sudoku-orig-value-face))
    (t (:inherit sudoku-orig-value-face)))
  "Face for values automatically insterted."
  :group 'sudoku)

(defcustom sudoku-style 'plain
  "Style of board.
Any style assumes fixed-width font."
  :type '(choice (const :tag "Plain" plain)
                 (const :tag "Sharp sign borders" sharp)
                 (const :tag "Unicode graphics" unicode))
  :group 'sudoku)

(defcustom sudoku-blank-cell '((plain . ?\.) (sharp . ?\.) (unicode . #x0387))
  "Alist of characters used for blank square."
  :type 'alist
  :group 'sudoku)

(defcustom sudoku-modeline-show-values t
  "*Non-nil to show possible values for current cell in modeline."
  :type 'boolean
  :group 'sudoku)

(defcustom sudoku-deduce-methods
  '(sudoku-insert-single
    sudoku-insert-hidden-single
    sudoku-insert-constrained)
  "*List of methods to use when deducing/autoinserting."
  :type 'hook
  :group 'sudoku)

(defcustom sudoku-deduce-messages t
  "*Non-nil to show techniques used for deduce/autoinsert in echo area."
  :type 'boolean
  :group 'sudoku)

(defcustom sudoku-mode-hook nil
  "Hooks to run when entering sudoku mode."
  :type 'hook
  :group 'sudoku)

(defcustom sudoku-after-change-hook nil
  "Hooks to run when some cell is changed.
Called with one argument - cell."
  :type 'hook
  :group 'sudoku)

;;}}}
;;{{{ Variables

(defconst sudoku-style-chars
  '((unicode . [
              ;; ┏ ━ ┯ ┓   top outlines
              #x250f #x2501 #x252f #x2513
              ;; ┠ ┃ ┨ ┃   mid outlines
              #x2520 #x2503 #x2528 #x2503
              ;; ┗ ━ ┷ ┛   bottom outlines
              #x2517 #x2501 #x2537 #x251b
              ;; ┼ ─ │   insiders
              #x253c #x2500 #x2502])
    (plain . [?+ ?- ?+ ?+
              ?+ ?| ?+ ?|
              ?+ ?- ?+ ?+
              ?+ ?- ?|])
    (sharp .  [?# ?# ?# ?#
               ?# ?# ?# ?#
               ?# ?# ?# ?#
               ?+ ?- ?|])))

(defvar sudoku-solved-puzzles nil
  "List of solved sudoku puzzles.")

(defvar sudoku-custom-puzzles nil
  "List of custom sudoku puzzles.")

(cl-defstruct (sudoku-puzzle (:type list))
  board                                 ; puzzle board
  plist)                                ; puzzle properties

(defun sudoku-puzzle-get (puzzle prop)
  (plist-get (sudoku-puzzle-plist puzzle) prop))

(defun sudoku-puzzle-put (puzzle prop val)
  (setf (sudoku-puzzle-plist puzzle)
        (plist-put (sudoku-puzzle-plist puzzle) prop val)))

(defun sudoku-puzzle-level (puzzle)
  (sudoku-puzzle-get puzzle :level))
(defsetf sudoku-puzzle-level (puzzle) (level)
  `(sudoku-puzzle-put ,puzzle :level ,level))

(defun sudoku-puzzle-id (puzzle)
  (sudoku-puzzle-get puzzle :id))
(defsetf sudoku-puzzle-id (puzzle) (id)
  `(sudoku-puzzle-put ,puzzle :id ,id))

(defun sudoku-puzzle-file (puzzle)
  (sudoku-puzzle-get puzzle :file))
(defsetf sudoku-puzzle-file (puzzle) (file)
  `(sudoku-puzzle-put ,puzzle :file ,file))

(defun sudoku-puzzle-url (puzzle)
  (sudoku-puzzle-get puzzle :url))
(defsetf sudoku-puzzle-url (puzzle) (url)
  `(sudoku-puzzle-put ,puzzle :url ,url))

(defun sudoku-puzzle-comment (puzzle)
  (sudoku-puzzle-get puzzle :comment))
(defsetf sudoku-puzzle-comment (puzzle) (level)
  `(sudoku-puzzle-put ,puzzle :comment ,level))

(defvar sudoku-mode nil)
(make-variable-buffer-local 'sudoku-mode)

(defvar sudoku-autoinsert-mode nil
  "Non-nil if sudoku autoinsert mode is enabled using
`sudoku-turn-on-autoinsert'")

(defconst sudoku-buffer-name "*Sudoku*")

(defvar sudoku-puzzle nil "Current puzzle.")
(defvar sudoku-current-pencil nil)
(defvar sudoku-current-board nil)
(defvar sudoku-current-possibles nil)

;; For undo/redo
(defvar sudoku-boards-stack nil)
(defvar sudoku-undo-list nil)

;; Variables for modeline
(defvar sudoku-modeline-cell nil)
(defvar sudoku-modeline-possible-values nil)

(defvar sudoku-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Various movement
    (define-key map [up] 'sudoku-move-point-up)
    (define-key map "\C-p" 'sudoku-move-point-up)
    (define-key map "k" 'sudoku-move-point-up)
    (define-key map [down] 'sudoku-move-point-down)
    (define-key map "\C-n" 'sudoku-move-point-down)
    (define-key map "j" 'sudoku-move-point-down)
    (define-key map [return] 'sudoku-move-point-down)
    (define-key map [left] 'sudoku-move-point-left)
    (define-key map "\C-b" 'sudoku-move-point-left)
    (define-key map "h" 'sudoku-move-point-left)
    (define-key map [right] 'sudoku-move-point-right)
    (define-key map "\C-f" 'sudoku-move-point-right)
    (define-key map "l" 'sudoku-move-point-right)
    (define-key map [tab] 'sudoku-move-point-right)

    (define-key map "\C-a" 'sudoku-move-point-leftmost)
    (define-key map "\C-e" 'sudoku-move-point-rightmost)
    (define-key map [prior] 'sudoku-move-point-upmost)
    (define-key map [next] 'sudoku-move-point-downmost)

    (define-key map [home] 'sudoku-move-point-first-cell)
    (define-key map [(meta ?<)] 'sudoku-move-point-first-cell)
    (define-key map [end] 'sudoku-move-point-last-cell)
    (define-key map [(meta ?>)] 'sudoku-move-point-last-cell)

    ;; Erasing current cell
    (define-key map "\C-d" 'sudoku-cell-erase)
    (define-key map "-" 'sudoku-cell-erase)
    (define-key map "_" 'sudoku-cell-erase)
    (define-key map " " 'sudoku-cell-erase)
    (define-key map "0" 'sudoku-cell-erase)
    (define-key map [backspace] 'sudoku-cell-erase)

    ;; Disabled in sudoku mode
    (define-key map "\C-v" 'sudoku-disabled-key)
    (define-key map "\M-v" 'sudoku-disabled-key)
    (define-key map "\M-f" 'sudoku-disabled-key)
    (define-key map "\M-b" 'sudoku-disabled-key)
    (define-key map [mouse-1] 'sudoku-disabled-key)
    (define-key map [down-mouse-1] 'sudoku-disabled-key)
    (define-key map [drag-mouse-1] 'sudoku-disabled-key)
    (define-key map [double-mouse-1] 'sudoku-disabled-key)

    (define-key map "\C-k" 'sudoku-disabled-key)

    ;; Inserting values
    (define-key map "1" 'sudoku-change-point)
    (define-key map "2" 'sudoku-change-point)
    (define-key map "3" 'sudoku-change-point)
    (define-key map "4" 'sudoku-change-point)
    (define-key map "5" 'sudoku-change-point)
    (define-key map "6" 'sudoku-change-point)
    (define-key map "7" 'sudoku-change-point)
    (define-key map "8" 'sudoku-change-point)
    (define-key map "9" 'sudoku-change-point)

    ;; Undo/redo
    (define-key map [(control /)] #'sudoku-undo)
    (define-key map [(control _)] #'sudoku-undo)
    (define-key map [undo] #'sudoku-undo)
    (define-key map [(control ?x) (control ?/)] #'sudoku-redo)

    (define-key map [?q] 'sudoku-quit)

    (define-key map [?e] 'sudoku-custom)
    (define-key map [?a] 'sudoku-toggle-autoinsert-mode)
    (define-key map [?c] 'sudoku-goto-center)
    (define-key map [?d] 'sudoku-deduce)
    (define-key map [?p] 'sudoku-pencil-mode)
    (define-key map [?h] 'sudoku-hint)
    (define-key map [?f] 'sudoku-puzzle-by-pid)

    (define-key map [(control ?c) (control ?c)] 'sudoku-comment-puzzle)
    (define-key map [(control ?x) (control ?s)] 'sudoku-save-puzzle)

    ;; To avoid slippery fingers
    (define-key map [?N] 'sudoku)
    (define-key map [?R] 'sudoku-restart)
    (define-key map [?P] 'sudoku-print)
    map)
  "Keymap for sudoku mode")

(easy-menu-add-item nil '("tools" "games") ["Sudoku" sudoku t])

(easy-menu-define sudoku-mode-menu sudoku-mode-map "sudoku menu."
 '("Sudoku"
   ["New game"      sudoku t]
   ["Reset game"    sudoku-restart t]
   ["Custom puzzle" sudoku-custom t]
   "---"
   ["Autoinsert" (sudoku-toggle-autoinsert-mode)
    :style toggle :selected sudoku-autoinsert-mode]
   ["Download" (setq sudoku-download (not sudoku-download))
    :style toggle :selected sudoku-download]
   ("Set level"
    ["Easy"  (setq sudoku-level 'easy)
     :style radio :selected (eq sudoku-level 'easy)]
    ["Medium" (setq sudoku-level 'medium)
     :style radio :selected (eq sudoku-level 'medium)]
    ["Hard"  (setq sudoku-level 'hard)
     :style radio :selected (eq sudoku-level 'hard)]
    ["Evil" (setq sudoku-level 'evil)
     :style radio :selected (eq sudoku-level 'evil)])
   "---"
   ["Quit game"     sudoku-quit t]))

;;}}}


(defmacro sudoku-positivep (num)
  `(and (numberp ,num) (> ,num 0)))

(defun sudoku-insert (o face)
  "Insert O and highlight with FACE."
  (let ((s (if (characterp o) (char-to-string o) o)))
    (insert (propertize s 'face face))))

(defmacro sudoku-foreach-cell (cell &rest forms)
  "Run FORMS for each CELL."
  `(dotimes (i 9)
     (dotimes (j 9)
       (let ((,cell (cons i j)))
         (progn ,@forms)))))
(put 'sudoku-foreach-cell 'lisp-indent-function 'defun)

;;{{{ Sudoku command

;; We don't need this function, however for sake of `C-h m' and
;; auto-mode-alist we have it
(defun sudoku-mode ()
  "Major mode to solve sudoku puzzles.

Commands:
\\{sudoku-mode-map}

Entering to this mode runs the normal hook `sudoku-mode-hook'."
  (interactive)
  (set-buffer (sudoku-load-puzzle (buffer-file-name))))

;;;###autoload
(defun sudoku (&optional arg)
  "Start playing sudoku.
Avoid selecting already solved puzzle unless prefix ARG is specified."
  (interactive "P")
  (if sudoku-download
      (sudoku-initialize (sudoku-download-puzzle sudoku-level))

    (let* ((puzs (sudoku-builtin-puzzles sudoku-level))
           (nsp (or (and arg puzs)
                    ;; Remove current puzzle and already solved
                    ;; puzzles
                    (cl-set-difference
                     puzs (cons (sudoku-puzzle-in-bif sudoku-puzzle)
                                sudoku-solved-puzzles)
                     :test #'equal))))
      (message "Available %d unsolved %S puzzles"
               (length nsp) sudoku-level)
      (unless nsp
        (error "No unsolved %S puzzles, set `sudoku-download' to non-nil"
               sudoku-level))
      (sudoku-initialize
       (sudoku-make-puzzle
        (nth (mod (random t) (length nsp)) nsp))))))

(defun sudoku-initialize (puzzle &optional no-select puzzle-state)
  "Initialize and display PUZZLE."
  (with-current-buffer (get-buffer-create sudoku-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer)

    (setq sudoku-puzzle puzzle
          sudoku-current-pencil nil
          sudoku-current-board (copy-tree (sudoku-puzzle-board puzzle))
          sudoku-current-possibles (sudoku-calculate-possibles)
          sudoku-boards-stack nil
          sudoku-undo-list nil)

    (sudoku-board-print sudoku-current-board)
    (sudoku-goto-center)

    ;; Setup sudoku-mode
    (kill-all-local-variables)
    (use-local-map sudoku-mode-map)
    (setq major-mode 'sudoku-mode
          mode-name  "sudoku"
          sudoku-mode t)
    (setq buffer-read-only t)
    (buffer-disable-undo)

;    (setq ctl-arrow 0)                  ; for pseudo
    (setq truncate-lines t)

    ;; Setup modeline for sudoku
    (sudoku-mode-setup-modeline)

    ;; make buffer visible
    (unless no-select
      (switch-to-buffer (current-buffer)))

    ;; Install the state
    (when puzzle-state
      (let ((st-board (sudoku-string-to-board puzzle-state)))
        (sudoku-foreach-cell cc
          (let ((ccval (sudoku-cell-num (sudoku-cell cc st-board))))
            (unless (or (sudoku-cell-has-orig-value cc)
                        (zerop ccval))
              (sudoku-change-cell cc (sudoku-point-genprops ccval)))))
        (sudoku-board-redraw)))

    (when (and (not (sudoku-custom-p))
               sudoku-autoinsert-mode)
      (sudoku-autoinsert))

    (run-mode-hooks 'sudoku-mode-hook)
    (current-buffer)))

;;}}}
;;{{{ Modeline

(defun sudoku-mode-setup-modeline ()
  "Setup modeline for `sudoku-mode'."
  (setq mode-line-format
        (list
         "%e"
         mode-line-front-space mode-line-mule-info mode-line-client
         mode-line-modified mode-line-remote mode-line-frame-identification
         mode-line-buffer-identification
         "  "
         '("Cell: " sudoku-modeline-cell)
         '(sudoku-modeline-show-values
           (", Hints: " sudoku-modeline-possible-values))
         "    "
         global-mode-string
         )))

(defun sudoku-modeline-update ()
  "Update values for modeline, and redraw modeline."
  (setq sudoku-modeline-cell
        (sudoku-cell-name (sudoku-current-cell))
        sudoku-modeline-possible-values
        (sudoku-cell-possibles-string))
  (force-mode-line-update))

;;}}}
;;{{{ Custom puzzle editor

(defun sudoku-empty-puzzle ()
  "Return empty puzzle."
  (make-sudoku-puzzle :board (make-list 9 (make-list 9 0))
                      :plist `(:level ,sudoku-level)))

(defun sudoku-custom-instructions ()
  "List of instructions strings for custom puzzle editor."
  (list (propertize "Custom puzzle editor" 'face 'underline)
        "Fill in values"
        "Press `C-M-c' when done."
        ""
        (format "Level: %S" sudoku-level)
        (format "Cells to go: %d" (sudoku-remaining-cells))))

(defun sudoku-custom (empty-p)
  "Create custom board and play it.
If called with prefix arg then edit empty puzzle."
  (interactive "P")
  (let ((sudoku-after-change-hook nil)
        (sudoku-custom-editor t))
    (declare (special sudoku-custom-editor))
    (sudoku-initialize (or (and empty-p (sudoku-empty-puzzle))
                           sudoku-puzzle))
    (recursive-edit))

  (setf (sudoku-puzzle-board sudoku-puzzle) sudoku-current-board)
  (unless (sudoku-puzzle-id sudoku-puzzle)
    (setf (sudoku-puzzle-id sudoku-puzzle) "Custom"))
  (push sudoku-puzzle sudoku-custom-puzzles)
  (sudoku-initialize sudoku-puzzle))

(defun sudoku-custom-p ()
  "Return non-nil if we are under custom puzzle editor."
  (boundp 'sudoku-custom-editor))

;;}}}

;;{{{ Quit/restart

(defun sudoku-quit-immediately ()
  "Quit without a prompt. Designed to be used by other functions."
  (kill-buffer (get-buffer sudoku-buffer-name)))

(defun sudoku-quit ()
  "Quit with confirmation."
  (interactive)
  (when (y-or-n-p "Quit sudoku? ")
    (sudoku-quit-immediately)))

(defun sudoku-restart ()
  "Return the board to its original state."
  (interactive)
  (unless sudoku-puzzle
    (error "You have to start before you can restart"))
  (sudoku-initialize sudoku-puzzle))

;;}}}
;;{{{ Misc functions for solvers

(defun sudoku-row (board n)
  "Return the nth row of a board."
  (nth n board))

(defun sudoku-cell (cell &optional board)
  "Return the BOARD's CELL."
  (nth (car cell) (sudoku-row (or board sudoku-current-board) (cdr cell))))

(defun sudoku-cell-empty-p (cell)
  "Return non-nil if CELL is empty."
  (zerop (sudoku-cell-value cell)))

;;}}}
;;{{{ Possibles/solver functions

(defun sudoku-calculate-possibles ()
  "Calculate list of possible values for whole board."
  (cl-loop for x from 0 below 9
    collect (cl-loop for y from 0 below 9
              collect (sudoku-cell-possibles (cons x y)))))

(defmacro sudoku-cell-ref (cell board)
  `(nth (car ,cell) (nth (cdr ,cell) ,board)))

(defun sudoku-remove-possible (cell pv)
  "From CELL remove PV as possible value."
  (setf (sudoku-cell-ref cell sudoku-current-possibles)
        (remove pv (sudoku-cell-ref cell sudoku-current-possibles))))

(defun sudoku-add-possible (cell pv)
  (setf (sudoku-cell-ref cell sudoku-current-possibles)
        (cons pv (sudoku-cell-ref cell sudoku-current-possibles))))

(defmacro sudoku-with-save-possibles (&rest forms)
  "Execute FORMS saving `sudoku-current-possibles'."
  `(let ((sudoku-current-possibles (copy-tree sudoku-current-possibles)))
     ,@forms))
(put 'sudoku-with-save-possibles 'lisp-indent-function 'defun)

(defun sudoku-cell-possibles (&optional cell)
  "Return all the possible values for a cell (i.e., those not
already in the row, column, and subsquare containing it."
  (unless cell (setq cell (sudoku-current-cell)))
  (when (sudoku-cell-empty-p cell)
    (cl-set-difference '(1 2 3 4 5 6 7 8 9)
                       (mapcar #'sudoku-cell-value
                               (append (sudoku-col-cells cell)
                                       (sudoku-row-cells cell)
                                       (sudoku-square-cells cell))))))
(put 'sudoku-cell-possibles 'method-name "S1")

(defun sudoku-cell-possibles-string (&optional cell)
  "Return string of possibles values on BOARD for CELL."
  (mapconcat #'int-to-string
             (sort (sudoku-cell-possibles cell) #'<)
             ","))

(defun sudoku-square-cells (cell)
  "Return cells list for square where CELL is."
  (let* ((sn (+ (* (/ (cdr cell) 3) 3) (/ (car cell) 3)))
         (srow (* 3 (/ sn 3))) (scol (* (mod sn 3) 3))
         ret)
    (cl-loop for r from srow to (+ 2 srow)
      do (cl-loop for c from scol to (+ 2 scol)
           do (push (cons c r) ret)))
    ret))

(defun sudoku-row-cells (cell)
  "Return cells list for row where CELL is."
  (cl-loop for i from 0 below 9
    collect (cons i (cdr cell))))

(defun sudoku-col-cells (cell)
  "Return cells list for column where CELL is."
  (cl-loop for i from 0 below 9
    collect (cons (car cell) i)))

(defun sudoku-cell-possibles-hidden-only (&optional cell)
  "Smart version of possibles.
With this solver you can solve almost any `hard' sudoku,
but not `evil'."
  (unless cell (setq cell (sudoku-current-cell)))
  (when (sudoku-cell-empty-p cell)
    (cl-remove-duplicates
     (cl-mapcan #'(lambda (posvs)
                    (cl-set-difference '(1 2 3 4 5 6 7 8 9) posvs))
             (mapcar #'(lambda (some-cells)
                         (cl-mapcan #'(lambda (cc)
                                        (or (sudoku-cell-possibles cc)
                                            (list (sudoku-cell-value cc))))
                                    some-cells))
                     (mapcar #'(lambda (cells) (remove cell cells))
                             (list (sudoku-row-cells cell)
                                   (sudoku-col-cells cell)
                                   (sudoku-square-cells cell))))))))

(put 'sudoku-cell-possibles-hidden-only 'method-name "H1")

(defun sudoku-set-combinations (set &optional arity)
  "Return all possible combinations (subsets) of SET.
Assumes SET is a valid set.  With optional ARITY, returns only subsets
with ARITY members."
  (cond ((null arity)
         (setq arity 0)
         (cl-mapcan #'(lambda (elt)
                        (sudoku-set-combinations set (incf arity)))
                    set))
        ((= arity 1) (mapcar #'list set))
        ((<= arity 0) '(nil))
        (t (let ((rest) (ctr 1))
             (cl-mapcan #'(lambda (first)
                            (setq rest (nthcdr ctr set)
                                  ctr (1+ ctr))
                            (mapcar #'(lambda (elt) (cons first elt))
                                    (sudoku-set-combinations rest (1- arity))))
                        set)))))

(defun sudoku-cells-constrains (cells)
  "Return any existing constrains for CELLS set."
  (let* ((rcells (cl-remove-if-not #'sudoku-cell-empty-p cells))
         (rposss (mapcar #'(lambda (cc)
                             (cons cc (sudoku-cell-possibles cc)))
                         rcells))
         (combs (cl-mapcan #'(lambda (ar)
                               (sudoku-set-combinations rcells ar))
                           (cl-loop for i from 2 below (length rcells)
                                 collect i))))
    (cl-flet ((getpos (cl) (cdr (assoc cl rposss))))
      (cl-loop for com in combs
        for compos = (cl-reduce #'cl-union (mapcar #'getpos com))
        when (= (length compos) (length com))
        collect (cons com compos)))))

(define-error 'sudoku-non-uniq "Non-uniq board")

(defun sudoku-board-solutions (&optional board err-non-uniq)
  "Return solutions list for the board.
If ERR-NON-UNIQ is non-nil then signal error if non-unique
solution is found."
  (declare (special trials avg-depth s1-cnt h1-cnt))
  (unless board (setq board sudoku-current-board))
  (let ((gc-cons-threshold most-positive-fixnum)
        (sudoku-current-board (copy-tree board))
        (shints (sudoku-board-hints board))
        error)
    ;; Step 1. SiSo
    ;; Step 2. Hidden single
    (cl-loop for continue = nil
      for ecs = (cl-remove-if-not #'sudoku-cell-empty-p (sudoku-board-cells))
      do (mapc #'(lambda (cc)
                   (let ((poss (sudoku-cell-possibles cc)))
                     (if (null poss)
                         (return (setq error t))
                       (when (= (length poss) 1)
                         (when (boundp 's1-cnt) (incf s1-cnt))
                         (sudoku-change-cell cc (first poss))
                         (setq continue t)))))
               ecs)
      do (mapc #'(lambda (cc)
                   (let ((poss (sudoku-cell-possibles-hidden-only cc)))
                     (when (= (length poss) 1)
                       (unless (memq (first poss)
                                     (sudoku-cell-possibles cc))
                         (return (setq error t)))
                       (when (boundp 'h1-cnt) (incf h1-cnt))
                       (sudoku-change-cell cc (first poss))
                       (setq continue t))))
               ecs)
      while continue)

    ;; Average depth without T/E
    (when (boundp 'avg-depth)
      (setq avg-depth
            (/ (+ avg-depth (- (sudoku-board-hints sudoku-current-board)
                               shints)) 2)))

    (cond (error nil)
          ((zerop (sudoku-remaining-cells))
           ;; Already solved board, cleanup board and return
           (list (mapcar #'(lambda (r) (mapcar #'sudoku-cell-num r))
                         sudoku-current-board)))

          (t
           ;; Step 3. Trial and error
           (let* ((ecs (cl-remove-if-not #'sudoku-cell-empty-p
                                         (sudoku-board-cells)))
                  (ecs-ps (sort
                           (cl-mapcar #'cons ecs
                                      (mapcar #'sudoku-cell-possibles ecs))
                           #'(lambda (e1 e2)
                               (< (length (cdr e1))
                                  (length (cdr e2))))))
                  (fecv (car ecs-ps))
                  (f-cell (car fecv))
                  (f-poss (cdr fecv)))
             (cl-loop for pv in f-poss
               for sols = 0 then sols
               do (sudoku-change-cell f-cell pv)
               for sol = (sudoku-board-solutions nil err-non-uniq)
               if sol nconc sol and do (incf sols)
               if (boundp 'trials) do (incf trials)
               if (and err-non-uniq (> sols 1))
               do (signal 'sudoku-non-uniq sol))
              )))))

(defun sudoku-solution-stats (board)
  "Return statistics for the BOARD."
  (let ((trials 0) (avg-depth (sudoku-board-hints board))
        (s1-cnt 0) (h1-cnt 0))
    (and (sudoku-board-solutions board t)
         (list trials avg-depth s1-cnt h1-cnt))))

(defun sudoku-board-first-solution (board)
  "Return first solution for BOARD."
  (condition-case err
      (car (sudoku-board-solutions board t))
    (sudoku-non-uniq (car (cdr err)))))

(defun sudoku-shuffle-list (list)
  "Shuffle given LIST."
  (let* ((len (length list))
         (taken (make-vector len nil))
         (result (make-vector len nil)))
    (dolist (e list)
      (while (let ((i (random len)))
               (cond ((aref taken i))
                     (t (aset taken i t)
                        (aset result i e)
                        nil)))))
    (append result '())))

(defun sudoku-board-seed (n board)
  "Seed the board with N values."
  (let ((cells (butlast (sudoku-shuffle-list
                         (sudoku-board-cells)) (- 81 n)))
        (sudoku-current-board (copy-tree board)))
    (cl-flet ((rnd-value (cc)
             (let ((ps (sudoku-cell-possibles cc)))
               (nth (random (length ps)) ps))))
      (mapc #'(lambda (cc)
                (sudoku-change-cell cc (rnd-value cc)))
            cells))
    sudoku-current-board))

(defun sudoku-board-random (&optional seed)
  "Generate random filled board."
  (or (sudoku-board-first-solution
       (sudoku-board-seed (or seed 20) (make-list 9 (make-list 9 0))))
      (sudoku-board-random seed)))

(defun sudoku-board-generate (&optional min-hints)
  "Generate random puzzle with minimum hint MIN-HINTS.
By default MIN-HINTS is 30."
  (unless min-hints (setq min-hints 30))
  (let ((rcs (sudoku-shuffle-list (sudoku-board-cells)))
        sudoku-current-board)
    (cl-loop for sudoku-current-board = (sudoku-board-random)
      do (cl-loop for rc in rcs
           for sv = (sudoku-cell-value rc)
           do (sudoku-change-cell rc 0)
           do (condition-case nil
                  (sudoku-board-solutions nil t)
                (sudoku-non-uniq
                 (sudoku-change-cell rc sv))))
      when (<= (- 81 (sudoku-remaining-cells))
               min-hints)
      return sudoku-current-board)))

(defun sudoku-puzzle-generate (&optional level min-hints)
  "Generate puzzle in built-in format."
  (let* ((gb (sudoku-board-generate min-hints))
         (bs (sudoku-solution-stats gb))
         (lv (if (zerop (first bs))
                 (if (> (third bs) 20) 'easy 'medium)
               (if (> (first bs) 2) 'evil 'hard))))
    (if (or (eq level 'any)
            (eq lv (or level sudoku-level)))
        (list (sudoku-board-to-string gb)
              :level lv :url (second
                              (assq 'url-source
                                    (get 'sudoku 'custom-links))))
      (sudoku-puzzle-generate level min-hints))))

;;}}}
;;{{{ Autoinserter

(defun sudoku-board-cells ()
  "Return list of all cells."
  (cl-loop for i from 0 below 9
    for col = (cl-loop for j from 0 below 9
                collect (cons i j))
    nconc col))

(defun sudoku-autoinsert-only (func by-cell &optional deduce)
  "For each cell in the board insert only possible value, if any.
Return non-nil if at least one value has been inserted."
  (unless func (setq func #'sudoku-cell-possibles))
  (let ((ocv (cl-loop for cc in (sudoku-board-cells)
               for pss = (funcall func cc)
               if (= (length pss) 1)
               return (cons cc (first pss)))))
    ;; OCV is very first cell available with only value
    (when ocv
      (let ((c (car ocv)) (v (cdr ocv)))
        (sudoku-test-and-change
         c (list v :face 'sudoku-autovalue-face
                 :by-cell by-cell
                 :pencil sudoku-current-pencil))
        (when sudoku-deduce-messages
          (message "%s: %s -> %d" (get func 'method-name)
                   (sudoku-cell-name c) v))
        (redisplay)
        (unless deduce
          (sudoku-autoinsert-only func by-cell))
        t))))

(defun sudoku-insert-single (by-cell &optional deduce)
  "Insert single values.
Return non-nil if at least one has been inserted."
  (sudoku-autoinsert-only #'sudoku-cell-possibles by-cell deduce))

(defun sudoku-insert-hidden-single (by-cell &optional deduce)
  "Insert hidden single values.
Return non-nil if at least one has been inserted."
  (sudoku-autoinsert-only #'sudoku-cell-possibles-hidden-only by-cell
                          deduce))

(defun sudoku-insert-constrained-cells (cells &optional by-cell deduce)
  "Insert all constrained cells."
  (let ((sqc (sort (sudoku-cells-constrains cells)
                   #'(lambda (cs1 cs2)
                       (> (length (car cs1)) (length (car cs2))))))
        inserted)
    (cl-loop for fc in sqc
      for scs on sqc
      do (cl-loop for sc in scs
           do (let ((cd (cl-set-difference (car fc) (car sc) :test #'equal))
                    (cv (cl-set-difference (cdr fc) (cdr sc))))
                (when (and (= 1 (length cd)) (= 1 (length cv)))
                  (sudoku-test-and-change
                   (first cd) (list (first cv) :face 'sudoku-autovalue-face
                                    :by-cell by-cell
                                    :pencil sudoku-current-pencil))
                  (when sudoku-deduce-messages
                    (message "CC: %s -> %d" (sudoku-cell-name (first cd))
                             (first cv)))
                  (setq inserted t)
                  (when deduce (return))))))
    inserted))

(defun sudoku-insert-constrained (by-cell &optional deduce)
  "Insert constrained values.
Return non-nil if at least one has been inserted."
  (let ((rows (cl-loop for i from 0 below 9
                collect (sudoku-row-cells (cons 0 i))))
        (cols (cl-loop for i from 0 below 9
                collect (sudoku-col-cells (cons i 0))))
        (squares (mapcar #'sudoku-square-cells
                         '((0 . 0) (3 . 0) (6 . 0)
                           (0 . 3) (3 . 3) (6 . 3)
                           (0 . 6) (3 . 6) (6 . 6))))
        inserted)
    (cl-loop for cls in (append rows cols squares)
      if (sudoku-insert-constrained-cells cls by-cell deduce)
      do (progn
           (setq inserted t)
           (when deduce (return))))
    inserted))

(defun sudoku-autoinsert (&optional cell deduce)
  "Fill cells with only value possible.
Goto CELL then.
Suitable to be used with `sudoku-after-change-hook'.
If `sudoku-autoinsert' raises \"Not a valid move\" error
then it means it found contradiction, so some of your previous moves
was incorrect.
Auto-insert does not work for pencils."
  (interactive)
  (unless (or (sudoku-positivep sudoku-current-pencil)
              (and cell (sudoku-cell-empty-p cell)))
    (let ((gc (or cell (sudoku-current-cell)))
          (sb (copy-tree sudoku-current-board)))
      (unwind-protect
          (block 'autoins
            (while (cl-some #'(lambda (fun)
                                (let ((rv (funcall fun cell deduce)))
                                  (when (and deduce rv)
                                    (return-from 'autoins t))
                                  rv))
                            sudoku-deduce-methods)))
        ;; Save previous board to stack, for undo
        (unless (equal sb sudoku-current-board)
          (push sb sudoku-boards-stack))
        (sudoku-goto-cell gc)))))

(defun sudoku-deduce ()
  "Deduce next step if any."
  (interactive)
  (unless (sudoku-autoinsert nil t)
    (message "Can't deduce further")))

(defun sudoku-remove-autoinserted (&optional cell)
  "Erase cells that was autoinserted after CELL inserted."
  (unless cell (setq cell (sudoku-current-cell)))
  (when (sudoku-cell-empty-p cell)
    (let ((sudoku-after-change-hook nil))
      (sudoku-foreach-cell cc
        (when (equal (sudoku-cell-plist-get cc :by-cell) cell)
          (sudoku-cell-erase cc))))))

(defun sudoku-turn-on-autoinsert ()
  (setq sudoku-autoinsert-mode t)
  (add-hook 'sudoku-after-change-hook 'sudoku-autoinsert)
  (sudoku-board-redraw)
  (when (eq major-mode 'sudoku-mode)
    (sudoku-autoinsert)))

(defun sudoku-turn-off-autoinsert ()
  (setq sudoku-autoinsert-mode nil)
  (remove-hook 'sudoku-after-change-hook 'sudoku-autoinsert)
  (sudoku-board-redraw))

(defun sudoku-toggle-autoinsert-mode ()
  "Toggle autoinstert mode."
  (interactive)
  (if sudoku-autoinsert-mode
      (sudoku-turn-off-autoinsert)
    (sudoku-turn-on-autoinsert)))

;;}}}
;;{{{ Undo/Redo

(defun sudoku-undo ()
  "Undo last change to board."
  (interactive)
  (let ((sb (pop sudoku-boards-stack)))
    (unless sb
      (error "Nothing to undo"))
    (push sudoku-current-board sudoku-undo-list)
    (setq sudoku-current-board (copy-tree sb))
    (sudoku-board-redraw)
    (message "Undo!")))

(defun sudoku-redo ()
  "Redo last undo."
  (interactive)
  (let ((rb (pop sudoku-undo-list)))
    (unless rb (error "Nothing to redo"))
    (push rb sudoku-boards-stack)
    (setq sudoku-current-board (copy-tree rb))
    (sudoku-board-redraw)
    (message "Redo!")))

;;}}}
;;{{{ Display/redraw board

(defun sudoku-board-hints (board)
  "Return number of hints for PUZZLE."
  (- 81 (sudoku-remaining-cells board)))

(defun sudoku-cell-num (cell)
  "Low level. Do not use it."
  (or (and (numberp cell) cell) (car cell)))

(defun sudoku-cell-name (cell)
  "Return CELL's name in format RxCy."
  (format "R%dC%d" (1+ (cdr cell)) (1+ (car cell))))

(defun sudoku-cell-value (cell)
  "Return value for the given CELL."
  (sudoku-cell-num (sudoku-cell cell)))

(defun sudoku-cell-props (cell)
  "Low level. Do not use it."
  (and (listp cell) (cdr cell)))

(defun sudoku-cell-plist-get (cell prop)
  "From CELL's properties get property PROP."
  (plist-get (sudoku-cell-props (sudoku-cell cell)) prop))

(defun sudoku-blank-cell ()
  "Return blank character for current `sudoku-style'."
  (cdr (assq sudoku-style sudoku-blank-cell)))

(defun sudoku-board-insert-cell (cell)
  (if (or (null cell) (and (numberp cell) (= cell 0)))
      (sudoku-insert (sudoku-blank-cell) 'sudoku-face)
    (sudoku-insert (+ 48 (sudoku-cell-num cell))
                   (or (plist-get (sudoku-cell-props cell) :face)
                       'sudoku-orig-value-face))))

(defun sudoku-pid-with-commas (pid)
  "Format PID with commas inside."
  (let* ((digits (string-to-list "1234567890"))
         (pl (length pid))
         (ss (% pl 3))
         (off 0) rs)
    (if (notevery #'(lambda (el) (memq el digits)) (string-to-list pid))
        pid
      (when (zerop ss) (setq ss 3))
      (while (< off pl)
        (push (substring pid off (+ off ss)) rs)
        (setq off (+ off ss) ss 3))
      (mapconcat #'identity (nreverse rs) ","))))

(defun sudoku-relative-filename (file)
  "Return relative filename for FILE."
  (if (string-match (concat "^" (expand-file-name "~") "/\\(.*\\)") file)
      (concat "~/" (match-string 1 file))
    file))

(defun sudoku-board-instructions ()
  "Return list of instructions strings."
  (list (format "Level: %S" (sudoku-puzzle-level sudoku-puzzle))
;         (when (sudoku-puzzle-url sudoku-puzzle)
;           (format "Puzzle URL: %s" (sudoku-puzzle-url sudoku-puzzle)))
        (when (sudoku-puzzle-file sudoku-puzzle)
          (format "Puzzle file: %s"
                  (sudoku-relative-filename
                   (sudoku-puzzle-file sudoku-puzzle))))
        (when (sudoku-puzzle-id sudoku-puzzle)
          (format "Puzzle ID: %s" (sudoku-pid-with-commas
                                   (sudoku-puzzle-id sudoku-puzzle))))
        (when (sudoku-puzzle-comment sudoku-puzzle)
          (format "Comment: %s" (sudoku-puzzle-comment sudoku-puzzle)))
        (format "Start hints: %d" (sudoku-board-hints
                             (sudoku-puzzle-board sudoku-puzzle)))
        (format "Cells to go: %d" (sudoku-remaining-cells))
        (format "Pencil mode: %d%s"
                (or sudoku-current-pencil 0)
                (or (and (or (null sudoku-current-pencil)
                             (zerop sudoku-current-pencil))
                         " (pen)")
                    " (pencil)"))
        (format "Auto-insert: %s"
                (if sudoku-autoinsert-mode "enabled" "disabled"))
        ))

(defun sudoku-board-print (board &optional help-strings)
  "Insert sudoku BOARD at the beginning of buffer.
If MESSAGE is specified insert it after the board."
  (save-excursion
    (unless help-strings
      (if (sudoku-custom-p)
          (setq help-strings (sudoku-custom-instructions))
        (setq help-strings (sudoku-board-instructions))))
    ;; Fix help strings list
    (setq help-strings (cl-remove-if-not #'stringp help-strings))
    (let* ((style (cdr (assq sudoku-style sudoku-style-chars)))
           (top-hc (make-string 7 (aref style 1)))
           (bot-hc (make-string 7 (aref style 9)))
           (hc (make-string 7 (aref style 13))))
      (cl-flet* ((inshelp ()
               (let ((hs (pop help-strings)))
                 (when hs (insert "    " hs))))
             (insrow (rownum)
               (let ((row (nth rownum board)))
                 (sudoku-insert (format "%c " (aref style 5))
                                'sudoku-face)
                 (sudoku-board-insert-cell (nth 0 row))
                 (sudoku-insert " " 'sudoku-face)
                 (sudoku-board-insert-cell (nth 1 row))
                 (sudoku-insert " " 'sudoku-face)
                 (sudoku-board-insert-cell (nth 2 row))
                 (sudoku-insert " " 'sudoku-face)
                 (sudoku-insert (format "%c " (aref style 14))
                                'sudoku-face)
                 (sudoku-board-insert-cell (nth 3 row))
                 (sudoku-insert " " 'sudoku-face)
                 (sudoku-board-insert-cell (nth 4 row))
                 (sudoku-insert " " 'sudoku-face)
                 (sudoku-board-insert-cell (nth 5 row))
                 (sudoku-insert " " 'sudoku-face)
                 (sudoku-insert (format "%c " (aref style 14))
                                'sudoku-face)
                 (sudoku-board-insert-cell (nth 6 row))
                 (sudoku-insert " " 'sudoku-face)
                 (sudoku-board-insert-cell (nth 7 row))
                 (sudoku-insert " " 'sudoku-face)
                 (sudoku-board-insert-cell (nth 8 row))
                 (sudoku-insert " " 'sudoku-face)
                 (sudoku-insert (format "%c" (aref style 7))
                                'sudoku-face)
                 (inshelp)
                 (insert "\n")
                 )))
        (sudoku-insert (format "%c%s%c%s%c%s%c"
                               (aref style 0) top-hc (aref style 2) top-hc
                               (aref style 2) top-hc (aref style 3))
                       'sudoku-face)
        (insert "\n")
        (insrow 0) (insrow 1) (insrow 2)
        (sudoku-insert (format "%c%s%c%s%c%s%c"
                               (aref style 4) hc (aref style 12) hc
                               (aref style 12) hc (aref style 6))
                       'sudoku-face)
        (inshelp) (insert "\n")
        (insrow 3) (insrow 4) (insrow 5)
        (sudoku-insert (format "%c%s%c%s%c%s%c"
                               (aref style 4) hc (aref style 12) hc
                               (aref style 12) hc (aref style 6))
                       'sudoku-face)
        (inshelp) (insert "\n")
        (insrow 6) (insrow 7) (insrow 8)
        (sudoku-insert (format "%c%s%c%s%c%s%c"
                               (aref style 8) bot-hc (aref style 10) bot-hc
                               (aref style 10) bot-hc (aref style 11))
                       'sudoku-face)
        (inshelp) (insert "\n")
        (when (sudoku-puzzle-url sudoku-puzzle)
          (insert "\n")
          (insert " URL: " (sudoku-puzzle-url sudoku-puzzle) "\n"))
        ))))

(defun sudoku-board-redraw (&optional board help-strings)
  "Redraw sudoku board, keeping point at current cell."
  (let ((sdk-buf (get-buffer sudoku-buffer-name)))
    (when (buffer-live-p sdk-buf)
      (with-current-buffer sdk-buf
        (let ((cell (sudoku-current-cell))
              (buffer-read-only nil))
          (erase-buffer)
          (sudoku-board-print (or board sudoku-current-board) help-strings)
          (sudoku-goto-cell cell))))))

;;}}}
;;{{{ Changing cells

(defun sudoku-cell-has-orig-value (cell)
  "Return non-nil if cell has original value."
  (/= (sudoku-cell cell (sudoku-puzzle-board sudoku-puzzle)) 0))

(defun sudoku-change-cell (cell input)
  "Changes a specific cell."
  (setf (nth (car cell) (nth (cdr cell) sudoku-current-board))
        input))

(defun sudoku-test-and-change (cell input)
  "Tests whether a change is valid. If it is, enters the cell and
redraws the board.
All moves are valid while editing custom board."
  (if (and (not (sudoku-custom-p))
           (sudoku-cell-has-orig-value cell))
      (error "Original value, can't change")
    (unless (or (sudoku-custom-p)
                (memq (sudoku-cell-num input)
                      `(0 ,(sudoku-cell-value cell)
                          ,@(sudoku-cell-possibles cell))))
      (error (format "Not a valid move: %S -> %d"
                     cell (sudoku-cell-num input))))

    (sudoku-change-cell cell input)
    (sudoku-board-redraw)
    (sudoku-goto-cell cell)
    (when (zerop (sudoku-remaining-cells))
      (sudoku-completion-routine))))

(defun sudoku-change-point (input &optional cell)
  "Change the value to INPUT at a point.
Run `sudoku-after-change-hook' after INPUT is inserted.
Run tests to ensure that the change is a valid one."
  (interactive (list (sudoku-point-genprops (- last-command-event 48))))
  (let ((cell (or cell (sudoku-current-cell)))
        (sb (copy-tree sudoku-current-board)))
    (sudoku-test-and-change cell input)
    (sudoku-remove-autoinserted cell)
    (unwind-protect
        (run-hook-with-args 'sudoku-after-change-hook cell)
      ;; Save SB for undo
      (unless (equal sb sudoku-current-board)
        (push sb sudoku-boards-stack)))

    ;; Keep the cell position
    (sudoku-goto-cell cell)))

(defun sudoku-cell-erase (&optional cell)
  "Erase value at CELL.
Erase current cell if interactively called."
  (interactive)
  (sudoku-change-point 0 cell))

;;}}}
;;{{{ Utils functions/commands

(defun sudoku-current-cell (&optional point)
  "Return cell coordinates for POINT poisition."
  (save-excursion
    (goto-char (or point (point)))
    (let* ((ln (- (line-number-at-pos) 2))
           (cl (/ (- (current-column) 2) 2))
           (lin (- ln (/ ln 4)))
           (col (- cl (/ cl 4))))
      (cons col lin))))

(defun sudoku-cell-point (cell)
  "Return a point on the screen for a given CELL."
  (let* ((col (car cell)) (lin (cdr cell))
         (cl (+ 2 (* 2 (+ (/ col 3) col))))
         (ln (+ 2 (/ lin 3) lin)))
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- ln)) (move-to-column cl)
      (point))))

(defun sudoku-current-value-face ()
  "Return face to be used for value."
  (if (memq sudoku-current-pencil '(1 2))
      (let ((fc (intern (format "sudoku-value-pencil-%d-face"
                                sudoku-current-pencil))))
        (if (zerop (length (cl-remove-if-not
                            #'(lambda (cc)
                                (eq (sudoku-cell-plist-get cc :pencil)
                                    sudoku-current-pencil))
                            (sudoku-board-cells))))
            ;; First time pencil value is used
            (list 'sudoku-orig-value-face fc)
          fc))
    'sudoku-value-face))

(defun sudoku-point-genprops (num)
  (if (sudoku-custom-p)
      num
    (list num :face (sudoku-current-value-face)
          :pencil sudoku-current-pencil)))

(defun sudoku-remaining-cells (&optional board)
  "Tests to see how many cells are remaining"
  (cl-count 0 (apply #'append (or board sudoku-current-board))))

(defun sudoku-completion-routine ()
  "Runs when there are no cells remaining. Gives a message of
victory, and then asks if you want to play again."
  (sudoku-board-redraw
   nil (append (sudoku-board-instructions)
               (list "" (propertize "YOU WON!" 'face 'sudoku-orig-value-face))))
  (sudoku-goto-center)

  ;; Save puzzle (in built-in format) to solved list
  (push (sudoku-puzzle-in-bif sudoku-puzzle)
        sudoku-solved-puzzles))

(defun sudoku-hint ()
  "Print possible values for current cell."
  (interactive)
  (let ((cell (sudoku-current-cell)))
    (if (sudoku-cell-has-orig-value cell)
        (message "Original value. No other possibilities.")
      (message "Possible values: %s"
               (sudoku-cell-possibles-string cell)))))

;;}}}
;;{{{ Pencil commands

(defun sudoku-pencil-mode (arg)
  "Start next pencil mode.
If prefig ARG is specified, then reset pencil mode to previous one.
If `sudoku-current-pencil' is 2, then reset to pen."
  (interactive "P")
  (cond (arg
         (when (sudoku-positivep sudoku-current-pencil)
           (let* ((cp sudoku-current-pencil)
                  (sudoku-current-pencil (1- cp)))
             (sudoku-pencil-reset cp))
           (decf sudoku-current-pencil)))

        ((eq sudoku-current-pencil 2)
         (let ((sudoku-current-pencil 0))
           (sudoku-pencil-reset 2 1))
         (setq sudoku-current-pencil 0))

        (t;; Enter pencil mode
         (if (numberp sudoku-current-pencil)
             (incf sudoku-current-pencil)
           (setq sudoku-current-pencil 1))))
  (sudoku-board-redraw))

(defun sudoku-pencil-reset (&rest pencils)
  "Reset/accept pencil values for PENCILS."
  (interactive)
  (let ((ap (and (cl-some #'(lambda (c)
                              (memq (sudoku-cell-plist-get c :pencil)
                                    pencils))
                          (sudoku-board-cells))
                 (y-or-n-p (format "Accept %s pencil%s? "
                                   (if (> (length pencils) 1)
                                       "all"
                                     (number-to-string (first pencils)))
                                   (if (> (length pencils) 1) "s" ""))))))
    (sudoku-foreach-cell cell
      (when (memq (sudoku-cell-plist-get cell :pencil) pencils)
        (sudoku-change-cell
         cell (if ap
                  (sudoku-point-genprops (sudoku-cell-value cell))
                0))))))

;;}}}
;;{{{ Motion commands

(defun sudoku-goto-cell (coords)
  "Moves to a given pair of coordinates."
  (goto-char (sudoku-cell-point coords))
  (sudoku-modeline-update))

(defun sudoku-move-point (direction &optional n)
  "Move point in DIRECTION N times.
DIRECTION is one of `left', `leftmost', `right', `rightmost', `up',
`upmost', `down', `downmost'.
Doesn't let you go outside the bounds of the board."
  (let* ((cell (sudoku-current-cell))
         (x (car cell)) (y (cdr cell)))
    (dotimes (i (or n 1))
      (ecase direction
        (left (when (> x 0) (decf x)))
        (leftmost (setq x 0))
        (right (when (< x 8) (incf x)))
        (rightmost (setq x 8))
        (up (when (> y 0) (decf y)))
        (upmost (setq y 0))
        (down (when (< y 8) (incf y)))
        (downmost (setq y 8))))
    (sudoku-goto-cell (cons x y))))

(defun sudoku-move-point-left (n)
  "Moves the point N cells left."
  (interactive "p")
  (sudoku-move-point 'left n))

(defun sudoku-move-point-leftmost ()
  "Moves the point to the leftmost cell."
  (interactive)
  (sudoku-move-point 'leftmost))

(defun sudoku-move-point-right (n)
  "Moves the point N cells right."
  (interactive "p")
  (sudoku-move-point 'right n))

(defun sudoku-move-point-rightmost ()
  "Moves the point to the rightmost cell."
  (interactive)
  (sudoku-move-point 'rightmost))

(defun sudoku-move-point-up (n)
  "Moves the point N cells up."
  (interactive "p")
  (sudoku-move-point 'up n))

(defun sudoku-move-point-upmost ()
  "Moves the point to the upmost cell."
  (interactive)
  (sudoku-move-point 'upmost))

(defun sudoku-move-point-down (n)
  "Moves the point N cells down."
  (interactive "p")
  (sudoku-move-point 'down n))

(defun sudoku-move-point-downmost ()
  "Moves the point to the downmost cell."
  (interactive)
  (sudoku-move-point 'downmost))

(defun sudoku-move-point-first-cell ()
  "Move the point to the first cell."
  (interactive)
  (sudoku-move-point 'upmost)
  (sudoku-move-point 'leftmost))

(defun sudoku-move-point-last-cell ()
  "Move the point to the last cell."
  (interactive)
  (sudoku-move-point 'downmost)
  (sudoku-move-point 'rightmost))

(defun sudoku-disabled-key ()
  (interactive)
  (message "%s is disabled in sudoku-mode"
           (key-description (this-command-keys))))

(defun sudoku-goto-center ()
  (interactive)
  (sudoku-goto-cell '(4 . 4)))

;;}}}
;;{{{ Puzzle downloader

(defun sudoku-puzzle-by-pid (source pid)
  "Find and download puzzle from SOURCE and PID."
  (interactive (list (completing-read "Puzzle source: "
                                      '(("websudoku.com") ("menneske.no"))
                                      nil t)
                     (read-string "Puzzle ID: ")))
  (when (stringp source)
    (setq source
          (cond ((string= source "websudoku.com") 'websudoku)
                ((string= source "menneske.no") 'menneske)
                (t (error "Unsupported source")))))
  (let ((sudoku-download-source source))
    (sudoku-initialize (sudoku-download-puzzle sudoku-level pid))))

(defun sudoku-download-source (level &optional pid)
  (let* ((lvs (ecase sudoku-download-source
                (websudoku
                 '((easy . "1") (medium . "2") (hard . "3") (evil . "4")))
                (menneske
                 '((easy . "1") (medium . "3") (hard . "5") (evil . "6")))))
         (ls (cdr (assq level lvs)))
         (source (ecase sudoku-download-source
                   (websudoku
                    (concat "http://play.websudoku.com/?level=" ls
                            (if pid (concat "&set_id=" pid) "")))
                   (menneske
                    (concat "http://www.menneske.no/sudoku/"
                            (if pid
                                (concat "showpuzzle.html?number=" pid)
                              (concat "random.html?diff=" ls)))))))
    source))

(defun sudoku-download-puzzle (level &optional pid)
  "Return sudoku board of LEVEL downloaded from websudoku.com."
  (if pid
      (message "Downloading %s puzzle from %S..."
               (sudoku-pid-with-commas pid) sudoku-download-source)
    (message "Downloading %S puzzle from %S..."
             level sudoku-download-source))
  (let* ((source (sudoku-download-source level pid))
         (sud (with-current-buffer (url-retrieve-synchronously source)
                (unwind-protect
                    (progn
                      (goto-char (point-min))
                      (ecase sudoku-download-source
                        (websudoku (sudoku-parse-websudoku))
                        (menneske (sudoku-parse-menneske))))
                  (kill-buffer)))))
    (sudoku-make-puzzle
     `(,(car sud) :level ,level
       :url ,(sudoku-download-source level (plist-get (cdr sud) :id))
       ,@(cdr sud)))))

(defun sudoku-parse-websudoku ()
  "Parse HTML and return list of three items.
\(PUZZLE-ID CHEATS BOARD\)"
  (let (cheat pid)
    (save-excursion
      ;; Extract cheat if any
      (when (re-search-forward "var cheat='\\([0-9]+\\)';" nil t)
        (setq cheat (match-string 1)))
      ;; Extract Puzzle identificator (pid) if any
      (when (re-search-forward "var pid='\\([0-9]+\\)';" nil t)
        (setq pid (match-string 1))))

    ;; Cut to table
    (save-excursion
      (delete-region
       (point-min)
       (and (re-search-forward
             "<FORM NAME=\"board\" METHOD=POST ACTION=\"./\"[^>]*>")
            (match-beginning 0)))
      (delete-region
       (and (search-forward "</TABLE>") (match-end 0))
       (point-max)))

    ;; collect all 81 cell-values in a list.
    (list (cl-loop while (re-search-forward "<INPUT CLASS=\\([ds]0\\)" nil t)
            if (and (string= (match-string 1) "s0")
                    (re-search-forward "VALUE=\"\\([1-9]\\)\"" nil t))
            concat (match-string 1)
            else concat "0")
          :id pid :cheat cheat)))

(defun sudoku-parse-menneske ()
  "Parse sudoku puzzle received from menneske.no"
  (let (pid)
    (save-excursion
      ;; Extract Puzzle identificator (pid) if any
      (when (re-search-forward "</table>[^:]*: \\([0-9]+\\)<br/>" nil t)
        (setq pid (match-string 1))))

    (save-excursion
      (delete-region (point-min)
                     (and (search-forward "<div class=\"grid\"><table>")
                          (match-end 0)))
      (delete-region (and (search-forward "</table>")
                          (match-beginning 0)) (point-max)))

    (list (cl-loop while (re-search-forward "<td[^>]*>\\([^<]+\\)</td>" nil t)
            for v = (match-string 1)
            concat (if (string= v "&nbsp;") "0" v))
          :id pid)))

;;}}}
;;{{{ Printing (TeX generation)

(defun sudoku-print ()
  "Print current puzzle."
  (interactive)
  (let ((auto-insert nil))
    (find-file (format "/tmp/sudoku-%s.tex"
                       (sudoku-puzzle-id sudoku-puzzle))))
  (erase-buffer)
  (insert "\\documentclass{article}\n")
  (insert "\\usepackage{sudoku}\n")
  (insert "\\begin{document}\n")
  (insert "\\thispagestyle{empty}\n")
  (insert "\\setlength\\sudokusize{12cm}\n")
  (insert "\\begin{sudoku}\n")
  (insert (mapconcat #'(lambda (rr)
                         (concat "|" (mapconcat
                                      #'(lambda (n)
                                          (if (zerop n)
                                              " "
                                            (int-to-string n)))
                                      rr "|")
                                 "|"))
                     (sudoku-puzzle-board sudoku-puzzle) ".\n"))
  (insert ".\n")
  (insert "\\end{sudoku}\n")
  (insert "\\vspace{2cm}\n")

  (insert "\\begin{verbatim}\n")
  (insert (format "Level: %S\n" (sudoku-puzzle-level sudoku-puzzle)))
  (when (sudoku-puzzle-url sudoku-puzzle)
    (insert (format "URL: %s" (sudoku-puzzle-url sudoku-puzzle)) "\n"))
  (insert (format "Puzzle ID: %s\n" (sudoku-pid-with-commas
                                     (sudoku-puzzle-id sudoku-puzzle))))
  (insert (format "Hints: %d\n" (sudoku-board-hints
                                 (sudoku-puzzle-board sudoku-puzzle))))
  (insert (format "Date: %s\n" (current-time-string)))
  (when (sudoku-puzzle-comment sudoku-puzzle)
    (insert (format "Comment: %s\n"
                    (sudoku-puzzle-comment sudoku-puzzle))))
  (insert "\\end{verbatim}\n")
  (insert "\\hrule\n")
  (insert "\\end{document}\n")
  (goto-char (point-min)))

;;}}}
;;{{{ Saving/Loading sudoku files

(defun sudoku-comment-puzzle (comment)
  "Set comment for the puzzle."
  (interactive (list (read-from-minibuffer
                      "Comment: " (sudoku-puzzle-comment sudoku-puzzle))))
  (setf (sudoku-puzzle-comment sudoku-puzzle) comment)
  (sudoku-board-redraw))

(defun sudoku-save-puzzle (file)
  "Save current puzzle to sdk FILE."
  (interactive (list (or (sudoku-puzzle-file sudoku-puzzle)
                         (read-file-name "Sudoku file: "))))
  (let* ((in-board (if (sudoku-custom-p)
                       sudoku-current-board
                     (sudoku-puzzle-board sudoku-puzzle)))
         (board sudoku-current-board)
         (has-state (not (equal in-board board))))
    (cl-flet ((board-string (board)
             (mapconcat #'(lambda (r)
                            (mapconcat #'(lambda (c)
                                           (let ((cn (sudoku-cell-num c)))
                                             (if (zerop cn)
                                                 "."
                                               (int-to-string cn))))
                                       r ""))
                        board "\r\n")))
      (with-current-buffer (find-file-noselect file nil t)
        (erase-buffer)

        (insert "#A Emacs sudoku.el\r\n")
        (insert "#U "
          (or (sudoku-puzzle-url sudoku-puzzle)
              "http://github.com/zevlg/sudoku.el") "\r\n")
        (when (sudoku-puzzle-comment sudoku-puzzle)
          (insert "#C " (sudoku-puzzle-comment sudoku-puzzle) "\r\n"))
        (insert "#B " (format-time-string "%d/%m/%y") "\r\n")
        (insert (format "#H %d" (sudoku-board-hints in-board)) "\r\n")
        (insert "#L "
          (capitalize (symbol-name (sudoku-puzzle-level sudoku-puzzle)))
          "\r\n")
        (when has-state
          (insert "[Puzzle]\r\n"))
        (insert (board-string in-board))
        (when has-state
          (insert "\r\n[State]\r\n")
          (insert (board-string board)))
        (save-buffer))))

  (setf (sudoku-puzzle-file sudoku-puzzle) file)
  (sudoku-board-redraw))

(defun sudoku-load-puzzle (file &optional no-select)
  "Load sdk FILE."
  (interactive "FPuzzle file: ")
  (let ((ppl (list :file file)) brd brd-state)
    (with-temp-buffer
      (insert-file-contents-literally file)
      (goto-char (point-min))
      (while (looking-at
              "#\\(A\\|B\\|C\\|D\\|S\\|U\\|L\\|N\\|H\\) ?\\([^\r]*\\)\r?$")
        (cond ((string= "L" (match-string 1))
               (setq ppl (plist-put ppl :level
                                    (intern (downcase (match-string 2))))))
              ((string= "C" (match-string 1))
               (setq ppl (plist-put ppl :comment (match-string 2))))
              ((string= "U" (match-string 1))
               (setq ppl (plist-put ppl :url (match-string 2)))))
        (forward-line))
      ;; Parse board, puzzle itself and the state
      (cl-flet ((parse-board-inplace ()
               (cl-loop for i from 0 below 9
                  unless (looking-at "^[123456789.]\\{9,9\\}\r?$")
                  do (error "Invalid sdk file")
                  concat (subst-char-in-string
                          ?. ?0 (buffer-substring (point) (+ 9 (point))))
                  do (forward-line))))
        (when (looking-at "\\[Puzzle\\]\r?$")
          (forward-line))
        (setq brd (parse-board-inplace))
        (when (looking-at "\\[State\\]\r?$")
          (forward-line)
          (setq brd-state (parse-board-inplace)))))

    (sudoku-initialize (sudoku-make-puzzle (cons brd ppl))
                       no-select brd-state)))

(defun sudoku-load-puzzle-noselect (file)
  (sudoku-load-puzzle file t))

(defun sudoku-load-puzzle-collection (file level)
  "Load puzzle collection (sdm) file.
And append them into built-in puzzles table."
  (interactive (list (read-file-name "SDM file: ")
                     (intern (completing-read
                              "Puzzles Level: "
                              '(("easy") ("medium") ("hard") ("evil"))
                              nil t))))
  ;; TODO
  )

;;}}}


;;{{{ Built-in puzzles

(defun sudoku-builtin-puzzles (level)
  "Filter builtin puzzles by LEVEL."
  (declare (special sudoku-builtin-puzzles)) ;shutup compiler
  (cl-remove-if-not #'(lambda (bip)
                        (eq (plist-get (cdr bip) :level) level))
                    sudoku-builtin-puzzles))

(defun sudoku-string-to-board (nl)
  "Convert flat numbers list NL to sudoku board."
  (setq nl (mapcar #'(lambda (c) (- c 48)) (string-to-list nl)))
  (cl-loop for v on nl by #'(lambda (l) (nthcdr 9 l))
    collect (cl-subseq v 0 9)))

(defun sudoku-board-to-string (brd)
  "Convert board BRD to string."
  (mapconcat #'(lambda (r)
                 (mapconcat #'number-to-string r ""))
             brd ""))

(defun sudoku-make-puzzle (bif)
  "Make puzzle from built-in format BIF."
  (make-sudoku-puzzle
   :board (sudoku-string-to-board (car bif))
   :plist (cdr bif)))

(defun sudoku-puzzle-in-bif (puzzle)
  "Return PUZZLE in built-in format."
  (cons (sudoku-board-to-string (sudoku-puzzle-board puzzle))
        (sudoku-puzzle-plist puzzle)))

;;; 160 Built-in puzzles, from menneske and generated
(defconst sudoku-builtin-puzzles
  '(
    ;; Easy
    ("000640000040100009580900420000050001300706002900020000003007084008001050020098000" :level easy :url "http://www.menneske.no/sudoku/showpuzzle.html?number=1964058" :id "1964058")
    ("004070100608104302210000045060000010400060003050000080840000039307506801005080600" :level easy :url "http://www.menneske.no/sudoku/showpuzzle.html?number=3944128" :id "3944128")
    ("510300092000560000300012005002000074053000260760000300400280006000071000970003021" :level easy :url "http://www.menneske.no/sudoku/showpuzzle.html?number=2108729" :id "2108729")
    ("003010700000583000500706009094000830760000091051000670800201003000674000006050100" :level easy :url "http://www.menneske.no/sudoku/showpuzzle.html?number=6518282" :id "6518282")
    ("809602401010000030004103600703000509000040000105000208001206700050000020302907104" :level easy :url "http://www.menneske.no/sudoku/showpuzzle.html?number=6546221" :id "6546221")
    ("000143000900000005830509074709020803200304006305080401150206048400000002000435000" :level easy :url "http://www.menneske.no/sudoku/showpuzzle.html?number=805784" :id "805784")
    ("003417800000206000007000600410020058200805006380070092004000900000104000002983500" :level easy :url "http://www.menneske.no/sudoku/showpuzzle.html?number=6383546" :id "6383546")
    ("008701006000030050000465080209000703056000840703000509060147005070050000400906000" :level easy :url "http://www.menneske.no/sudoku/showpuzzle.html?number=6377725" :id "6377725")
    ("640900013000002000002450700080004902006000300407100080004095100000300000370008096" :level easy :url "http://www.menneske.no/sudoku/showpuzzle.html?number=4128027" :id "4128027")
    ("700000004004006700090048010058204000006000400000801590040950070007600200300000005" :level easy :url "http://www.menneske.no/sudoku/showpuzzle.html?number=5001643" :id "5001643")
    ("000309000050000010046805920903008107000000000805600204091204750070000040000701000" :level easy :url "http://www.menneske.no/sudoku/showpuzzle.html?number=6346833" :id "6346833")
    ("000702000702308901080040060120080035005000800430070029070090040206403108000607000" :level easy :url "http://www.menneske.no/sudoku/showpuzzle.html?number=3086077" :id "3086077")
    ("046902350107030908000704000501800402090000030702003805000507000905060203084301570" :level easy :url "http://www.menneske.no/sudoku/showpuzzle.html?number=3633583" :id "3633583")
    ("620000034010700090000900000300157008800604007050893040000300000040200080530000071" :level easy :url "http://www.menneske.no/sudoku/showpuzzle.html?number=6380805" :id "6380805")
    ("000594107700060090000307200407000506360000089509000703104809000090050002800426000" :level easy :url "http://www.menneske.no/sudoku/showpuzzle.html?number=793780" :id "793780")
    ("000804000037102680501306904369000248000000000815000763204907806093508470000201000" :level easy :url "http://www.menneske.no/sudoku/showpuzzle.html?number=3189112" :id "3189112")
    ("920000038054000190003090200007001600000000000060500040005080900046000780780000024" :level easy :url "http://www.menneske.no/sudoku/showpuzzle.html?number=3344162" :id "3344162")
    ("013000270070000060095732810007450600002809100006073900048326790020000050061000420" :level easy :url "http://www.menneske.no/sudoku/showpuzzle.html?number=2131248" :id "2131248")
    ("706150804020030010800020009000005002294000538300900000900070001040090080507016203" :level easy :url "http://www.menneske.no/sudoku/showpuzzle.html?number=1263072" :id "1263072")
    ("800000050540000006900210000007950800050804070400067001000630000300000010080000639" :level easy :url "http://www.menneske.no/sudoku/showpuzzle.html?number=6832450" :id "6832450")

    ;; Medium
    ("072051009008002000610300000780000600400000001006000097000003056000200108100560400" :level medium :url "http://www.menneske.no/sudoku/showpuzzle.html?number=6525674" :id "6525674")
    ("000065070000940000000070340000020013000006090630000405010000500900007830403090000" :level medium :url "http://www.menneske.no/sudoku/showpuzzle.html?number=398398" :id "398398")
    ("090010060301690504506400301100000000060000040000000008605900702207850409040070010" :level medium :url "http://www.menneske.no/sudoku/showpuzzle.html?number=6682129" :id "6682129")
    ("072001400000004070300875000930000120000000000081000067000749005060008001007006300" :level medium :url "http://www.menneske.no/sudoku/showpuzzle.html?number=4163612" :id "4163612")
    ("050807206004306000000010800410000079002000400870000012308060000000703600600905020" :level medium :url "http://www.menneske.no/sudoku/showpuzzle.html?number=4305130" :id "4305130")
    ("900802007004715900000040000180000024029000130540000098000060000001298500600401009" :level medium :url "http://www.menneske.no/sudoku/showpuzzle.html?number=3459130" :id "3459130")
    ("300006000002713000600000500960007020030201080020600037003000004000195300000300002" :level medium :url "http://www.menneske.no/sudoku/showpuzzle.html?number=3843275" :id "3843275")
    ("100000006005904200040050030030405020002030400010602080060040050009507300800000004" :level medium :url "http://www.menneske.no/sudoku/showpuzzle.html?number=538309" :id "538309")
    ("050200001200000000000047000085000400039020870700009005000400059070100604020000010" :level medium :url "http://www.menneske.no/sudoku/showpuzzle.html?number=1888830" :id "1888830")
    ("070800004006010082080004600004000300050060090390000026008009040010080900460500070" :level medium :url "http://www.menneske.no/sudoku/showpuzzle.html?number=753519" :id "753519")
    ("000000000307000109050640070062070000500104008000030590080950040204000901000000000" :level medium :url "http://www.menneske.no/sudoku/showpuzzle.html?number=5982355" :id "5982355")
    ("002600001000025040007400000970008500800006130160500000040000307080000000000001005" :level medium :url "http://www.menneske.no/sudoku/showpuzzle.html?number=1387987" :id "1387987")
    ("000000003093000070400800600712085000000900000008600000001000007040370020000540081" :level medium :url "http://www.menneske.no/sudoku/showpuzzle.html?number=1987048" :id "1987048")
    ("000084310004000000070000090080000735790000400500003009009010200102050000000046000" :level medium :url "http://www.menneske.no/sudoku/showpuzzle.html?number=4072228" :id "4072228")
    ("081000702420300006000000810000703060000000000050209000804000000009007028510000970" :level medium :url "http://www.menneske.no/sudoku/showpuzzle.html?number=2816695" :id "2816695")
    ("800200006200007005001005300090070000050030080000060030002900400500800003400006008" :level medium :url "http://www.menneske.no/sudoku/showpuzzle.html?number=5831271" :id "5831271")
    ("030010090600300005010600070000004100103090208009100000090006080500008001040050020" :level medium :url "http://www.menneske.no/sudoku/showpuzzle.html?number=1220430" :id "1220430")
    ("050003800030804000000270000008020509060000020309060700000038000000102095001500000" :level medium :url "http://www.menneske.no/sudoku/showpuzzle.html?number=2476065" :id "2476065")
    ("020000780098000006000000003000400000010080009000000620000075400070102300581030000" :level medium :url "http://www.menneske.no/sudoku/showpuzzle.html?number=3334859" :id "3334859")
    ("000003004541000200070401000009060500050309040800020009090807020000000876200006000" :level medium :url "http://www.menneske.no/sudoku/showpuzzle.html?number=1277401" :id "1277401")

    ;; Hard
    ("800000001900740002006300900050070000004983100000010030001800200500260003400000009" :level hard :url "http://www.menneske.no/sudoku/showpuzzle.html?number=4286730" :id "4286730")
    ("000000820800046070060300050040070001200000600000004008003050000010002400780001000" :level hard :url "http://www.menneske.no/sudoku/showpuzzle.html?number=3230804" :id "3230804")
    ("700600108000000000051040760006053800008000500005170300503060280004000000100200005" :level hard :url "http://www.menneske.no/sudoku/showpuzzle.html?number=3645162" :id "3645162")
    ("900308000000000000027004000000000100040030597850000600004205700000780006200016005" :level hard :url "http://www.menneske.no/sudoku/showpuzzle.html?number=3000921" :id "3000921")
    ("000100020400060000000593000005000601900000000014007080000400010209000356800000092" :level hard :url "http://www.menneske.no/sudoku/showpuzzle.html?number=3826455" :id "3826455")
    ("080000000002000000700060210000000100050700094830050000009100005008030700000607003" :level hard :url "http://www.menneske.no/sudoku/showpuzzle.html?number=6487221" :id "6487221")
    ("000000060000040010100835700005409100027010850003507400001254000030090000090000500" :level hard :url "http://www.menneske.no/sudoku/showpuzzle.html?number=3267880" :id "3267880")
    ("004000000780500001500002300218000070040900000000004000000100003060090800020080050" :level hard :url "http://www.menneske.no/sudoku/showpuzzle.html?number=1868979" :id "1868979")
    ("060100008009750000807000000004201000230080094000304600000000980000027001400005200" :level hard :url "http://www.menneske.no/sudoku/showpuzzle.html?number=6247788" :id "6247788")
    ("500030000002450000080006020000912005006000700800003100000000054070500013000600000" :level hard :url "http://www.menneske.no/sudoku/showpuzzle.html?number=3865286" :id "3865286")
    ("001009870000080006980000400000903000320000061050602080602000048500040000030006200" :level hard :url "http://www.menneske.no/sudoku/showpuzzle.html?number=4884254" :id "4884254")
    ("020000094100407000080500000007030600000072380000009000010060000000000402906000003" :level hard :url "http://www.menneske.no/sudoku/showpuzzle.html?number=159554" :id "159554")
    ("001000300020600080380700096070205600000000000008907020910008037040001060007000500" :level hard :url "http://www.menneske.no/sudoku/showpuzzle.html?number=5945336" :id "5945336")
    ("400006009670000045080040020160900082700000006030001050040050070210000068300200004" :level hard :url "http://www.menneske.no/sudoku/showpuzzle.html?number=3412690" :id "3412690")
    ("510000098007900600800005002600249003000000000000531000200600007008002500730000029" :level hard :url "http://www.menneske.no/sudoku/showpuzzle.html?number=6389704" :id "6389704")
    ("000040700000180206000200010000007340025003000060000005500000400086030000009002100" :level hard :url "http://www.menneske.no/sudoku/showpuzzle.html?number=4555972" :id "4555972")
    ("000000000063000270700309005009580100000402000006093800100205004038000920000000000" :level hard :url "http://www.menneske.no/sudoku/showpuzzle.html?number=796849" :id "796849")
    ("070300200000045600000260100030000706018000930607000050002019000003480007004003000" :level hard :url "http://www.menneske.no/sudoku/showpuzzle.html?number=2973960" :id "2973960")
    ("000000035900000007000510609043009000009008013001320000007060000000007860602000001" :level hard :url "http://www.menneske.no/sudoku/showpuzzle.html?number=3856134" :id "3856134")
    ("460000102020004000030020608000098700000500000090640080000001906000250000001030070" :level hard :url "http://www.menneske.no/sudoku/showpuzzle.html?number=1211258" :id "1211258")

    ;; Evil
    ("102006980000090060700000003070219040000705000000468000900000005010070000063001709" :level evil :url "http://www.menneske.no/sudoku/showpuzzle.html?number=4564760" :id "4564760")
    ("050000019109002400740601000005103600010000090006904500020308046900200108800000050" :level evil :url "http://www.menneske.no/sudoku/showpuzzle.html?number=4969663" :id "4969663")
    ("020109005800600200090008100400003010008060900060200004016002000000900401700305090" :level evil :url "http://www.menneske.no/sudoku/showpuzzle.html?number=5393864" :id "5393864")
    ("190580000000700200000000900000069000000000080450000000608400030530000000000070002" :level evil :url "http://www.menneske.no/sudoku/showpuzzle.html?number=6900818" :id "6900818")
    ("600004000004981000007600100470060820090502060062010073009007200000295700000100009" :level evil :url "http://www.menneske.no/sudoku/showpuzzle.html?number=2868868" :id "2868868")
    ("900806001007501900000090000350209064004000100670103052000050000006908300200607008" :level evil :url "http://www.menneske.no/sudoku/showpuzzle.html?number=5610262" :id "5610262")
    ("000050009000804750000907000075009230300000004092300680000501000046702000500030000" :level evil :url "http://www.menneske.no/sudoku/showpuzzle.html?number=1476840" :id "1476840")
    ("000760000030401060009003800092000085300000007580000140003500400020309070000024000" :level evil :url "http://www.menneske.no/sudoku/showpuzzle.html?number=2700138" :id "2700138")
    ("080960000002007040050002390705000010009000200010000905008200060023400500000056020" :level evil :url "http://www.menneske.no/sudoku/showpuzzle.html?number=2108674" :id "2108674")
    ("030000090005604200000892000081005670009000500026400130000783000003206700010000020" :level evil :url "http://www.menneske.no/sudoku/showpuzzle.html?number=765403" :id "765403")
    ("000506280605030000000901000908000705050000010706000302000803000000020901092104000" :level evil :url "http://www.menneske.no/sudoku/showpuzzle.html?number=6120845" :id "6120845")
    ("009800600080000090020790050004070000790406025000020700070310080050000010008900400" :level evil :url "http://www.menneske.no/sudoku/showpuzzle.html?number=5721293" :id "5721293")
    ("700861005000000000000904300207000409900307006604000107009602000000000000500713008" :level evil :url "http://www.menneske.no/sudoku/showpuzzle.html?number=6508279" :id "6508279")
    ("030209080068040910100006004901000803000000000053000460300600005047030290090407030" :level evil :url "http://www.menneske.no/sudoku/showpuzzle.html?number=2326578" :id "2326578")
    ("400009810008070093000000000174000000006000378000005000000000001900003500053400002" :level evil :url "http://www.menneske.no/sudoku/showpuzzle.html?number=5320309" :id "5320309")
    ("002001500060000020018020930200100009601030804050006010037050190090000080006009300" :level evil :url "http://www.menneske.no/sudoku/showpuzzle.html?number=6428933" :id "6428933")
    ("100070005604000907030010060000056000301207506000130000010060030706000201200040008" :level evil :url "http://www.menneske.no/sudoku/showpuzzle.html?number=1976203" :id "1976203")
    ("003050200000360000840200063000500740350706082027008000630002079000035000009080300" :level evil :url "http://www.menneske.no/sudoku/showpuzzle.html?number=5360557" :id "5360557")
    ("130502000060903010000010000290000056007000100350000072000030000080109060000604037" :level evil :url "http://www.menneske.no/sudoku/showpuzzle.html?number=1685129" :id "1685129")
    ("800005006006000200010806090080021304000307000607480020090708060003000800700100002" :level evil :url "http://www.menneske.no/sudoku/showpuzzle.html?number=2381052" :id "2381052")

    ;;; Generated by `sudoku-puzzle-generate'
    ("700600500000002100084000932050000409002050000090027005000000703500069000000078000" :level easy :url "http://github.com/zevlg/sudoku.el")
    ("000003001000100200000700800001060305920000100070510000400000030002000006009308020" :level easy :url "http://github.com/zevlg/sudoku.el")
    ("000000090020700100800009700400000003097000000300860005006020054000580000100000800" :level easy :url "http://github.com/zevlg/sudoku.el")
    ("000008000084700000300400007001000863490000070007000010060300020570040900000809000" :level easy :url "http://github.com/zevlg/sudoku.el")
    ("394000000000600003060090700005060200000070040081040006000800600047020000003100004" :level easy :url "http://github.com/zevlg/sudoku.el")
    ("900000000080000005201000003100090004004060700030700000000008006005010030890530207" :level easy :url "http://github.com/zevlg/sudoku.el")

    ("900000004006000070400007106100000500002000001040058000000000040307060002050800600" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("080020407200065100095000002400006900001300000007008003000000000350079000000200005" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("009000050000020004500030910300000001004100005000843700806000030700000509010005060" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("000000000000349000085000009000081000200000035954000600093400000002000801040650003" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("000020005000040000080900370060005040040000290900003700700000060500000080800069000" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("090060000007080030000000601010320000508000300000000079060007020004000008300008057" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("300000005080600700070098000504000070100057008000042003400100000012074000008000000" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("000005024820000000000930000000600000900000400500027030009050300000000068063004005" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("039000000400050002000000631270000005900200040000070000001600050093005000700004080" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("006000001004105026000070008050000030010600900008000004500000000000340000923000800" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("000825000908000204000400000004300600200050000090008000000000086026000053800700410" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("000500200000083000083004006004000000050090070802006009065000300009001600000402007" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("200530007905000008013000000500000601000003000308050020070000410002000000000704062" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("437500006060200037200000900000000090040000153000007000000010609001008000000300008" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("085000000000083000000020570000091000100000460430070000500000740000100300300008000" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("800000700006700950001960000400000006302009100000050000700200000009000008000400012" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("000020087200090010100006000050308000080000004041000370000003790500000000730069000" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("030400008807000000000060590000000084080000000540000109000043020600709800000108006" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("795004000000050000010000008007040000200087000000000610000008026500000170102600503" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("009000000004900050105400000060750008000290600002006000001030092046000105000001300" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("360007004005001000907000008000070015000000263000000000170030900040200050000048000" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("005000000000000508800020094000074020310008000000300000003009100200081000570000400" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("000400000083010000600002800010604000000000050000000304000087041000329000200000070" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("600010070000200040042005000007009000000000260005000034709100000000350920000040005" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("083000502700000000000105060002004780000700000105000023000300000000852009000090800" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("860020000000050600021400009080000400500060000006070008000003060000081070103000082" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("000000450000060003400250080000700300560002000004008069000400010058000030902005000" :level medium :url "http://github.com/zevlg/sudoku.el")
    ("000023000007006008609400300070800000052700000010504090000005004003080001000000700" :level medium :url "http://github.com/zevlg/sudoku.el")

    ("004000080631070900008000700000000000100009300000340060007030200000060001090002040" :level hard :url "http://github.com/zevlg/sudoku.el")
    ("500100000030000800000900206800000010270090000940073600000020700000300002700000049" :level hard :url "http://github.com/zevlg/sudoku.el")
    ("840001006000050000000206090000000008070000500051023600000130005005400081200000000" :level hard :url "http://github.com/zevlg/sudoku.el")
    ("080000900900050807005020000000030009307609500020000300150002000040060000000840006" :level hard :url "http://github.com/zevlg/sudoku.el")
    ("000400000900070000070008290008030000207905000090700002063000000080502900000000504" :level hard :url "http://github.com/zevlg/sudoku.el")
    ("000000007240010080930006500190000400000005800000000060009070340410053008002040600" :level hard :url "http://github.com/zevlg/sudoku.el")
    ("000000078024000100000000000053000804700009030000401500070200000012700006800050000" :level hard :url "http://github.com/zevlg/sudoku.el")
    ("000000802001006000006890000087001600000000903400000010500309000300120705070080000" :level hard :url "http://github.com/zevlg/sudoku.el")
    ("600503100010008030000020009780069300000000002000070000400000000008900700035100900" :level hard :url "http://github.com/zevlg/sudoku.el")
    ("000070003000302050001009000090004008007010600380000000050040960000580040000006500" :level hard :url "http://github.com/zevlg/sudoku.el")
    ("000070080008000000370250000000800060000300002006000074600430900200000005000105400" :level hard :url "http://github.com/zevlg/sudoku.el")
    ("000000059000020380004001002000904000000870000900210406500600008067000000000000091" :level hard :url "http://github.com/zevlg/sudoku.el")
    ("509800000070003090060000000008061000052700800300200000000000960024050003000106000" :level hard :url "http://github.com/zevlg/sudoku.el")
    ("020006089046008107010000000009100050000000006000007800000000730008302090004050000" :level hard :url "http://github.com/zevlg/sudoku.el")
    ("000097021000805000000000580108000300030400009004206000000000007805000000040780190" :level hard :url "http://github.com/zevlg/sudoku.el")
    ("050007000008000000702600050000003700006100002000004000047008009090500001200700085" :level hard :url "http://github.com/zevlg/sudoku.el")

    ("700920040000000700025600000000470000000300069300009080000090010064000805091005000" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("060400000004003900003005002002009014000000050001058300700600000000094000305000001" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("001098002000700090703050000000020100609000000100005870000000001000840000906100305" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("012030009000020004030091000097050000001000000060000008100000970005600300800300500" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("005000010800000000100620070000016000040900002670050040006700000200001003000390400" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("000097200820000000619080000040010006006003005500000800090020603000100009005008000" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("060100030002007900354009000000900000030008000090000502107000000000040706020000005" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("010050090380900000000600002208000403000001000005249006009006000020000040000870009" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("400000050000002000060491020003027000800004000010000700050000109000086500100009087" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("100360200908001000000000000300005010080000400000037506002000005010029007870003000" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("000001000700805004010200060000000070000604001009000058306050000800010500001736002" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("502080000006030040000000800008010007010020090000000650400908000000000000970150000" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("001300004000705090090006050002000000040000010300000760050007800800004005400800001" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("007000004000300000090000300000230900900806005038100402400050000080600100003001200" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("500000000000000060041802000002080500005000001400760000000048007100900030380070040" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("000000200507000008006090050050002000000360000863007020000610000005009400908450701" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("900002004000600090081000002140079030700000900000000086017006008600300000000007050" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("060080000007090000000005802000000600026400009050600030500008040701020090000003200" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("020000000060230001000070500000040000850000070400000389600000002040700150000801040" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("630100040005070000900602010000803900000000000010027300000045270004030000000000006" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("000007200000000005007210600000030090000600517905100006006009008000020000104700002" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("070004060905200000200600050030000608400700900081500000000840300000020000008000002" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("040080005000740010006001000060000003209005000004000070030000600007000021105060900" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("000070401070000000004009600000040030090017500000800016402005700050080090081000000" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("000047000000009000320000400030000057002098030010506020070081090100000800096000000" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("900007000041002000000400006005040020030870100800309000000003000080000470006500080" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("009403001400009000000020000003001850004800060080050920000000000130000400200070500" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("000000079605800000000400000070604080000003700004000001320005047000006900107000008" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("000000903083006200200030000000007040040109006070400019910008000005000800007600400" :level evil :url "http://github.com/zevlg/sudoku.el")
    ("170050000800200000045000600000500430010070000006090002000006000200000040000300180" :level evil :url "http://github.com/zevlg/sudoku.el")
    ))

;;}}}

(provide 'sudoku)

;;; sudoku.el ends here
