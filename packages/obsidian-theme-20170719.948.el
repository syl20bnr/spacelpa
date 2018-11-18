;;; obsidian-theme.el --- port of the eclipse obsidian theme
;; Author: martin haesler
;; URL: http://github.com/mswift42/obsidian-theme
;; Package-Version: 20170719.948
;;; Version: 0.1

;; original eclipse theme by Morinar
;;(http://eclipsecolorthemes.org/?view=theme&id=21)

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

;;; Commentary:

;;; Code:

(deftheme obsidian)

(custom-theme-set-faces
 'obsidian
 '(default ((t (:background "#293134" :foreground "#e0e2e4"))))
 '(font-lock-builtin-face ((t (:foreground "#808080"))))
 '(region ((t (:background "#804000" :foreground "#e0e2e4"))))
 '(highlight ((t (:background "#3c414c"))))
 '(hl-line ((t (:background "#2f393c"))))
 '(fringe ((t (:background "#232323" :foreground "#cfbfad"))))
 '(cursor ((t (:background "#626262"))))
 '(show-paren-match-face ((t (:background "#e8e2b7"))))
 '(isearch ((t (:bold t :foreground "#616161" :background "#e2e2e5"))))
 '(mode-line ((t (:foreground "#b2b2b2" :background "#293134" :box nil))))
 '(mode-line-inactive ((t
                        (:foreground "#949494" :background "#293134" :box nil))))
 '(mode-line-buffer-id ((t (:foreground "#898989" :background "#293134"))))

 
 '(minibuffer-prompt ((t (:bold t :foreground "#708090"))))
 '(default-italic ((t (:italic t))))
 '(font-lock-comment-face ((t (:foreground "#7d8c93"))))
 '(font-lock-negation-char-face ((t (:foreground "#e8e2b7"))))
 '(font-lock-reference-face ((t (:foreground "#678cb1"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#7d8c93"))))
 '(font-lock-constant-face ((t (:foreground "#A082bd"))))
 '(font-lock-doc-face ((t (:foreground "#7d8c93"))))
 '(font-lock-function-name-face ((t (:foreground "#e0e2e4"))))
 '(font-lock-keyword-face ((t (:bold t :foreground "#93c763"))))
 '(font-lock-preprocessor-face ((t (:foreground "#a082bd"))))
 '(font-lock-reference-face ((t (:bold t :foreground "#678cb1"))))
 '(font-lock-string-face ((t (:foreground "#ec7600"))))
 '(font-lock-type-face ((t (:foreground "#93c763"))))
 '(font-lock-variable-name-face ((t (:foreground "#678cb1"))))
 '(font-lock-warning-face ((t (:foreground "#ffffff" :background "#ff6523"))))
 '(link ((t (:foreground "#678cb1"))))
 '(org-hide ((t (:foreground "#708090"))))
 '(org-level-1 ((t (:bold t :foreground "#808080" :height 1.1))))
 '(org-level-2 ((t (:bold nil :foreground "#7e8aa2" :height 1.1))))
 '(org-level-3 ((t (:bold t :foreground "#df9f2d" :height 1.1))))
 '(org-level-4 ((t (:bold nil :foreground "#af4f4b" :height 1.0))))
 '(org-date ((t (:underline t :foreground "#f0ad6d") :height 1.1)))
 '(org-footnote  ((t (:underline t :foreground "#ad600b"))))
 '(org-link ((t (:underline t :foreground "#e0e2e4" ))))
 '(org-special-keyword ((t (:foreground "#e0e2e4"))))
 '(org-verbatim ((t (:foreground "#eeeeec" :underline t :slant italic))))
 '(org-block ((t (:foreground "#7d8c93"))))
 '(org-quote ((t (:inherit org-block :slant italic))))
 '(org-verse ((t (:inherit org-block :slant italic))))
 '(org-todo ((t (:bold t :foreground "#e0e2e4"))))
 '(org-done ((t (:bold t :foreground "#708090"))))
 '(org-warning ((t (:underline t :foreground "#ff0000"))))
 '(org-agenda-structure ((t (:weight bold :foreground "#df9f2d"))))
 '(org-agenda-date ((t (:foreground "#e0e2e4" :height 1.2))))
 '(org-agenda-date-weekend ((t (:weight normal :foreground "#808bed"))))
 '(org-agenda-date-today ((t (:weight bold :foreground "#e0e2e4" :height 1.4))))
 '(org-scheduled ((t (:foreground "#eeeeec"))))
 '(font-latex-bold-face ((t (:foreground "#cd8b00"))))
 '(font-latex-italic-face ((t (:foreground "#808bed" :italic t))))
 '(font-latex-string-face ((t (:foreground "#708090"))))
 '(font-latex-match-reference-keywords ((t (:foreground "#708090"))))
 '(font-latex-match-variable-keywords ((t (:foreground "#708090"))))
 '(ido-only-match ((t (:foreground "#e0e2e4"))))
 '(org-sexp-date ((t (:foreground "#808080"))))
 '(ido-first-match ((t (:foreground "#b1d631"))))
 '(gnus-header-content ((t (:foreground "#ff9810"))))
 '(gnus-header-from ((t (:foreground "#f0e16a"))))
 '(gnus-header-name ((t (:foreground "#e0e2e4"))))
 '(gnus-header-subject ((t (:foreground "#ff8800"))))
 '(magit-item-highlight ((t (:background "#2f393c"))))
 '(mu4e-view-url-number-face ((t (:foreground "#e0e2e4"))))
 '(mu4e-cited-1-face ((t (:foreground "#7d8c93"))))
 '(mu4e-cited-7-face ((t (:foreground "#df9f2d"))))
 '(trailing-whitespace ((t :foreground nil :background "#3f4c51" )))
 '(slime-repl-inputed-output-face ((t (:foreground "#e0e2e4")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'obsidian)

;;; obsidian-theme.el ends here
