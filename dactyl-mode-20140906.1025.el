;;; dactyl-mode.el --- Major mode for editing Pentadactyl config files

;; Copyright (C) 2014 Olli Piepponen
;; Copyright (C) 2013 Andrew Pennebaker
;; Copyright (C) 2011 Alpha Tan

;; Authors: Olli Piepponen
;;          Mark Oteiza
;;          Andrew Pennebaker
;;          Alpha Tan <alphatan.zh@gmail.com>
;; URL: https://github.com/luxbock/dactyl-mode
;; Package-Version: 20140906.1025
;; Version: 0.0.1
;; Keywords: languages, vim
;; Package-Requires: ()

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This mode is heavily based on the vimrc-mode by Andrew Pennebaker. The
;; original vimrc-mode can be found at:
;; https://github.com/mcandre/vimrc-mode

;;; Code:

(require 'font-lock)

(defgroup dactyl-mode nil
  "Major mode for editing Pentadactyl config files."
  :group 'languages)

(defcustom dactyl-mode-hook nil
  "Normal hook run when entering dactyl-mode."
  :type 'hook
  :group 'dactyl-mode)

(defgroup dactyl-faces nil
  "Faces used for Pentadactyl script."
  :group 'faces)

(defface dactyl-option
  '((default :inherit font-lock-variable-name-face))
  "Face used for Pentadactyl's configuration options.")

(defface dactyl-command
  '((default :inherit font-lock-keyword-face))
  "Face used for Pentadactyl Ex commands.")


;; Font lock linking
(defvar dactyl-font-lock-keywords
  `(
    ;; Line comment
    ("^[\t ]*\\(\"\\)\\(.*\\)$"
     (1 font-lock-comment-delimiter-face)
     (2 font-lock-comment-face))
     
    ;; String start:
    ("\\(\"[^\n\r\"]*\"\\)\\|\\('[^\n\r]*'\\)"
     (0 font-lock-string-face)) ;; String end;

    ;; Variables
    ("\\<[bwglsav]:[a-zA-Z_][a-zA-Z0-9#_]*\\>"
     (0 font-lock-variable-name-face))
    ("\\(let[ \t]+\\)\\<\\([a-zA-Z_][a-zA-Z0-9#_]*\\)\\>"
     (2 font-lock-variable-name-face))

    ;; Commands
    ("\\(^\\|[ \t]*\\)\\(command \\)\\<\\([A-Za-z0-9-_]+\\)\\>"
     (3 font-lock-variable-name-face))

    ;; Dactyl js-invocations
    ("\\<\\(dactyl\\)\\>\\(.\\)\\([A-Za-z0-9-_]+\\)?"
     1 '(face font-lock-type-face))

    ;; EOF
    ("<<EOF\\|EOF" . font-lock-reference-face)

    ;; Arguments
    ("\\([ \t]+\\)\\(-\\)\\([A-Za-z0-9-_]+\\)"
     (3 font-lock-variable-name-face))

    ;; Functions
    (,(concat "\\(^\\|[ \t]*\\)"
              (regexp-opt  '("abbreviate" "ab"
                             "addons" "ao" "addo"
                             "autocmd" "au"
                             "back" "ba"
                             "background" "bg"
                             "bdelete" "bd"
                             "blistkeys"
                             "bmap"
                             "bmark"
                             "bmarks" "bma"
                             "bnoremap"
                             "buffer"
                             "buffers"
                             "bunmap"
                             "cabbreviate" "ca"
                             "caretlistkeys"
                             "caretmap"
                             "caretnoremap"
                             "caretunmap"
                             "cd"
                             "clistkeys"
                             "Clistkeys"
                             "cmap"
                             "Cmap"
                             "cnoremap"
                             "Cnoremap"
                             "colorscheme" "colo"
                             "command" "com"
                             "completions" "comp"
                             "contexts"
                             "cookies"
                             "cunabbreviate"
                             "cunmap"
                             "Cunmap"
                             "delbmarks" "delbm"
                             "delcommand" "delc"
                             "delgroup" "delg"
                             "delmacros" "delmac"
                             "delmarks" "delm"
                             "delqmarks" "delqm"
                             "delstyle"
                             "dialog" "dia"
                             "dlclear" "dlc"
                             "doautoall" "doautoa"
                             "doautocmd" "do"
                             "downloads" "dl" "downl"
                             "echo"
                             "echoerr"
                             "echomsg"
                             "else"
                             "endif"
                             "elseif"
                             "elseif"
                             "else"
                             "endif"
                             "emenu"
                             "endif"
                             "if"
                             "elseif"
                             "else"
                             "execute" "exe"
                             "exit"
                             "extadd" "exta"
                             "extdelete" "extrm" "extde"
                             "extdisable" "extd"
                             "extenable" "exte"
                             "extoptions" "exto"
                             "extrehash" "extr"
                             "exttoggle" "extt"
                             "extupdate" "extu"
                             "feedkeys" "fk"
                             "finish" "fini"
                             "forward" "fw" "fo"
                             "frameonly" "frameo"
                             "group" "gr"
                             "hardcopy" "ha"
                             "help" 
                             "helpall" "helpa"
                             "highlight" "hl"
                             "history" "hs" "hist"
                             "iabbreviate"
                             "if"
                             "elseif"
                             "else"
                             "endif"
                             "ilistkeys"
                             "Ilistkeys"
                             "imap"
                             "Imap"
                             "inoremap"
                             "Inoremap"
                             "iunabbreviate"
                             "iunmap"
                             "Iunmap"
                             "javascript" "js"
                             "jumps" "ju"
                             "keepalt" "keepa"
                             "let"
                             "listcommands" "lc"
                             "listkeys" "lk" "listk"
                             "listoptions" "lo" "listo"
                             "loadplugins" "lpl"
                             "macros" "macr"
                             "map"
                             "mark" "ma"
                             "marks"
                             "messages" "mes"
                             "messclear" "messc"
                             "mkpentadactylrc" "mkp"
                             "mkvimruntime" "mkv"
                             "mlistkeys" 
                             "mmap"
                             "mnoremap"
                             "munmap"
                             "nlistkeys"
                             "nmap"
                             "nnoremap"
                             "nohlfind"
                             "noremap"
                             "normal"
                             "nunmap"
                             "olistkeys"
                             "omap"
                             "onoremap"
                             "open"
                             "ounmap"
                             "pageinfo"
                             "pagestyle"
                             "pintab"
                             "preferences"
                             "private" "pr" "pr0n" "porn"
                             "pwd"
                             "qmark" "qma"
                             "qmarks"
                             "quit" 
                             "quitall" "qa" "qall"
                             "redraw" "redr"
                             "registers"
                             "rehash" "reh"
                             "reload" "re"
                             "reloadall" "reloada"
                             "restart" "res"
                             "runtime" "runt"
                             "sanitize" "sa"
                             "saveas" "sav" "write" "writ"
                             "sbclose"
                             "scriptnames" "scrip"
                             "set"
                             "setglobal" "setg"
                             "setlocal" "setl"
                             "sidebar" "sideb"
                             "silent" "sil"
                             "source" "so"
                             "stop" "st"
                             "stopall" "stopa"
                             "style" "sty"
                             "styledisable" "styled"
                             "styleenable" "stylee"
                             "styletoggle" "stylet"
                             "tab"
                             "tabattach" "taba"
                             "tabclose" "tabc"
                             "tabdetach" "tabde"
                             "tabdo" "tabd" 
                             "tabduplicate" "tabdu"
                             "tablast" "tabl" "bl"
                             "tabmove" "tabm"
                             "tabnext" "tabn"
                             "tabonly" "tabo"
                             "tabopen" 
                             "tabprevious" "tp" "tabp"
                             "tabrewind" "br" "tabr" "tabfir"
                             "time" 
                             "tlistkeys"
                             "tmap"
                             "tnoremap"
                             "toolbarhide" "tbh"
                             "toolbarshow" "tbs"
                             "toolbartoggle" "tbt"
                             "tunmap"
                             "unabbreviate" "una"
                             "undo"
                             "undoall" "undoa"
                             "unlet" "unl"
                             "unmap"
                             "unpintab" "unpin"
                             "verbose" "verb" "verbo"
                             "version" "ve"
                             "viewsource" "vie"
                             "vlistkeys"
                             "vmap"
                             "vnoremap"
                             "vunmap"
                             "winclose" "wc" "winc"
                             "window" "wind"
                             "winonly" "winon"
                             "winopen" "wopen"
                             "wqall" "wqa" "wq"
                             "yank"
                             "zoom") 'words)
              "\\([^_]\\|$\\)")
     2 '(face dactyl-command))


    ;; Options
    (,(concat "\\(set \\)"
              (regexp-opt
               '("activate"
                 "altwildmode"
                 "autocomplete"
                 "banghist"
                 "cdpath"
                 "complete"
                 "cookieaccept"
                 "cookielifetime"
                 "cookies"
                 "defsearch"
                 "downloadsort"
                 "editor"
                 "encoding"
                 "s"
                 "errorbells"
                 "eventignore"
                 "exrc"
                 "extendedhinttags"
                 "fileencoding"
                 "findcase"
                 "findflags"
                 "followhints"
                 "fullscreen"
                 "guioptions"
                 "helpfile"
                 "hintinputs"
                 "hintkeys"
                 "hintmatching"
                 "hinttags"
                 "hinttimeout"
                 "history"
                 "hlfind"
                 "incfind"
                 "insertmode"
                 "iskeyword"
                 "jsdebugger"
                 "jumptags"
                 "linenumbers"
                 "loadplugins"
                 "maxitems"
                 "messages"
                 "more"
                 "newtab"
                 "nextpattern"
                 "online"
                 "pageinfo"
                 "passkeys"
                 "passunknown"
                 "popups"
                 "previouspattern"
                 "runtimepath"
                 "sanitizeitems"
                 "sanitizeshutdown"
                 "sanitizetimespan"
                 "scroll"
                 "scrollsteps"
                 "scrolltime"
                 "shell"
                 "shellcmdflag"
                 "showmode"
                 "showstatuslinks"
                 "showtabline"
                 "spelllang"
                 "strictfocus"
                 "suggestengines"
                 "timeout"
                 "timeoutlen"
                 "titlestring"
                 "urlseparator"
                 "usermode"
                 "verbose"
                 "visualbell"
                 "wildanchor"
                 "wildcase"
                 "wildignore"
                 "wildmode"
                 "wildsort"
                 "wordseparators"
                 "yankshort") 'words))
     2 '(face dactyl-option))

    ;; Operators start:
    (,(concat "\\("
              ;; word char
              "\\|" "\\(![=~]?[#?]?\\)"
              "\\|" "\\(>[#\\\\?=]?[#?]?\\)"
              "\\|" "\\(<[#\\\\?=]?[#?]?\\)" 
              "\\|" "\\(\\+=?\\)"
              "\\|" "\\(-=?\\)"
              "\\|" "\\(=[=~]?[#?]?\\)"
              "\\|" "\\(||\\)"
              "\\|" "\\(&&\\)"
              "\\|" "\\(EOF\\)"
              "\\|" "\\(<<EOF\\)"

              "\\|" "\\(\\.\\)"
              "\\)"
              )
     1 font-lock-constant-face) ;; Operators end;
    )
  "Default expressions to highlight in dactyl-mode.")

;; Support for Pentadactyl script

(defvar dactyl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "." table)
    table))

(defalias 'dactyl--parent-mode
  (if (fboundp 'prog-mode) #'prog-mode #'fundamental-mode))

;;;###autoload
(define-derived-mode dactyl-mode dactyl--parent-mode "dactyl"
  "Major mode for editing Pentadactyl configuration files."
  :group 'dactyl-mode
  :syntax-table dactyl-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults) '(dactyl-font-lock-keywords))
  (set (make-local-variable 'comment-start) "\"")
  (set (make-local-variable 'comment-end) ""))

(provide 'dactyl-mode)

;;; dactyl-mode.el ends here
