;;; sunny-day-theme.el --- Emacs24 theme with a light background.

;; Author: Martin Haesler
;; URL: http://github.com/mswift42/sunny-day-theme
;; Package-Version: 20140413.1425
;; Version: 0.2

;; Emacs24 theme with a light-yellow background.
;; Copyright (C) 2013 , Martin Haesler

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

;; This file is not part of Emacs.

(deftheme sunny-day)

(custom-theme-set-faces
  'sunny-day 
        '(default ((t (:background "#f2f1c0" :foreground "#000000"))))
        '(font-lock-builtin-face ((t (:foreground "#9e0045" :bold t))))
        '(region ((t (:background "#3c414c" :foreground "#faf4c6"))))
        '(highlight ((t (:background "#f5e98f"))))
	'(hl-line ((t (:background "#f5e98f"))))
	'(fringe ((t (:background "#f2f1c0" :foreground "#4c4c4c"))))
	'(cursor ((t (:background "#626262"))))
        '(show-paren-match-face ((t (:background "#727170"))))
        '(isearch ((t (:bold t :foreground "#f03f3f" :background "#e2e2e5"))))
        '(mode-line ((t (:bold t :foreground "#121212" :background "#f8f1b1"))))
        '(mode-line-inactive ((t (:foreground "#616161" :background "#fcf9de"))))
        '(mode-line-buffer-id ((t (:bold t :foreground "#424242" :background "#f8f0b0"))))
	'(mode-line-highlight ((t (:background "#d3d2d1"))))
	'(minibuffer-prompt ((t (:bold t :foreground "#212122"))))
        '(default-italic ((t (:italic t))))
	'(font-lock-comment-face ((t (:foreground "#808080"))))
	'(font-lock-negation-char-face ((t (:foreground "#f03f3f"))))
	'(font-lock-reference-face ((t (:foreground "#1d4200"))))
	'(font-lock-constant-face ((t (:foreground "#5c0002"))))
        '(font-lock-doc-face ((t (:foreground "#304050"))))
        '(font-lock-function-name-face ((t (:foreground "#014d00" :bold t))))
        '(font-lock-keyword-face ((t (:bold t :foreground "#2e0a00"))))
	'(font-lock-reference-face ((t (:bold t :foreground "#b998df"))))
        '(font-lock-string-face ((t (:foreground "#10144c"))))
        '(font-lock-type-face ((t (:foreground "#5c0002"))))
        '(font-lock-variable-name-face ((t (:foreground "#014d00"))))
        '(font-lock-warning-face ((t (:foreground "#ffffff" :background "#ff6523"))))
	'(link ((t (:foreground "#f03f3f"))))
	'(org-hide ((t (:foreground "#304050"))))
        '(org-level-1 ((t (:bold t :foreground "#212221" ))))
        '(org-level-2 ((t (:bold nil :foreground "#292929"))))
        '(org-level-3 ((t (:bold t :foreground "#343434"))))
        '(org-level-4 ((t (:bold nil :foreground "#393939"))))
        '(org-date ((t (:underline t :foreground "#3450a2") )))
        '(org-footnote  ((t (:underline t :foreground "#e20a00"))))
        '(org-link ((t (:underline t :foreground "#a82e4d" ))))
        '(org-special-keyword ((t (:foreground "#5c0002"))))
        '(org-verbatim ((t (:foreground "#f03f3f" :underline t :slant italic))))
        '(org-block ((t (:foreground "#121212"))))
        '(org-quote ((t (:inherit org-block :slant italic))))
        '(org-verse ((t (:inherit org-block :slant italic))))
        '(org-todo ((t (:bold t :foreground "#000000"))))
        '(org-done ((t (:bold t :foreground "#212121"))))
        '(org-warning ((t (:underline t :foreground "#ff0000"))))
        '(org-agenda-structure ((t (:weight bold :foreground "#5c0002" :box (:line-width 1 ) :background "#fbf7d7"))))
        '(org-agenda-date ((t (:foreground "#025c00" :height 1.2))))
        '(org-agenda-date-weekend ((t (:weight normal :foreground "#4c4c4c"))))
        '(org-agenda-date-today ((t (:weight bold :foreground "#025c00" :height 1.4))))
	'(org-scheduled ((t (:foreground "#3450a2"))))
	'(font-latex-bold-face ((t (:foreground "#cd8b00"))))
	'(font-latex-italic-face ((t (:foreground "#808bed" :italic t))))
	'(font-latex-string-face ((t (:foreground "#708090"))))
	'(font-latex-match-reference-keywords ((t (:foreground "#708090"))))
	'(font-latex-match-variable-keywords ((t (:foreground "#708090"))))
	'(ido-only-match ((t (:foreground "#f03f3f"))))
	'(org-sexp-date ((t (:foreground "#808080"))))
	'(ido-first-match ((t (:foreground "#2e31ff"))))
	'(gnus-header-content ((t (:foreground "#e3e3e3"))))
	'(gnus-header-from ((t (:foreground "#424140"))))
	'(gnus-header-name ((t (:foreground "#708090"))))
	'(gnus-header-subject ((t (:foreground "#4c4c4c"))))
	'(gnus-header-to ((t (:foreground "#708090"))))
	'(gnus-header-content ((t (:foreground "#708090"))))
	'(gnus-server-agent-face ((t (:foreground "#626160"))))
	'(magit-item-highlight ((t (:background "#f5e98f"))))
	'(ac-completion-face ((t (:underline t :foreground "#525252"))))
	'(change-log-acknowledgement ((t (:foreground "#616161"))))
	'(diff-context ((t (:foreground "#525252"))))
	'(js2-external-variable ((t (:foreground "#f03f3f"))))
	'(js2-private-member ((t (:foreground "#708090"))))
	'(js2-jsdoc-value    ((t (:foreground "#808080"))))
        '(trailing-whitespace ((t (:foreground nil :background "#f2d8c0" ))))
	'(slime-repl-inputed-output-face ((t (:foreground "#232323")))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'sunny-day)

;;; sunny-day-theme.el ends here












