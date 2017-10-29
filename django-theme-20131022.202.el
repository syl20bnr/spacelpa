;;; django-theme.el --- Custom face theme for Emacs

;; Copyright (C) 2012 Andrzej Sliwa

;; Author: Andrzej Sliwa
;; URL: http://github/anrzejsliwa/django-theme
;; Package-Version: 20131022.202
;; Version: 1.4.0
;;
;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(deftheme django "The Django color theme")

(custom-theme-set-faces
    'django
    '(default ((t (:background "#00291d" :foreground "#f8f8f8"))))
    '(cursor ((t (:foreground "#336442"))))
    '(region ((t (:foreground "#f8f8f8" :background "#245032"))))
    '(highlight ((t (:background "#245032"))))
    '(match ((t (:background "#96dd3b" :foreground "#000000"))))
    '(mode-line ((t (:background "#1c482b" :foreground "#000000" :box nil))))
    '(mode-line-inactive ((t (:background "#1a382b" :foreground "#000000" :box nil))))
    '(fringe ((t (:background "#001a10"))))
    '(linum ((t (:background "#001a10" :foreground "#245032"))))
    '(isearch ((t (:background "#96dd3b" :foreground "#000000"))))
    '(isearch-fail ((t (:background "#ff6800" :foreground "#f8f8f8"))))
    '(vertical-border ((t (:foreground "#001a10"))))
    '(lazy-highlight ((t (:background "#497958" :foreground "#f8f8f8"))))
    '(ido-first-match ((t (:foreground "#FFC557"))))
    '(ido-subdir ((t (:foreground "#3B7C55"))))
    '(hl-line ((t (:background "#001a10"))))
    '(flycheck-error    ((t (:foreground "#002116" :background "#FF6800" :weight bold :underline nil))))
    '(flycheck-warning  ((t (:foreground "#002116" :background "#084Eb9" :weight bold :underline nil))))
    '(flymake-errline  ((t (:foreground "#002116" :background "#FF6800" :weight bold :underline nil))))
    '(flymake-warnline ((t (:foreground "#002116" :background "#084Eb9" :weight bold :underline nil))))
    '(minibuffer-prompt ((t (:foreground "#f8f8f8" :weight bold))))
    '(font-lock-builtin-face ((t (:foreground "#96dd3b"))))
    '(font-lock-comment-face ((t (:slant italic :foreground "#245032"))))
    '(font-lock-comment-delimiter-face ((t (:slant italic :foreground "#245032"))))
    '(font-lock-constant-face ((t (:foreground "#497958"))))
    '(font-lock-function-name-face ((t (:foreground "#fec758"))))
    '(font-lock-keyword-face ((t (:foreground "#96dd3b"))))
    '(font-lock-string-face ((t (:foreground "#91bb9e"))))
    '(font-lock-type-face ((t (:foreground "#ead47a"))))
    '(font-lock-variable-name-face ((t (:foreground "#497958"))))
    '(font-lock-warning-face ((t (:foreground "#ff6800" :weight bold))))
    '(font-lock-doc-string-face ((t (:foreground "#91BB9E"))))
    '(font-lock-preprocessor-face ((t (:foreground "#497958"))))
    '(link ((t (:underline t :foreground "#245032"))))
    '(link-visited ((t (:underline t :foreground "#245032"))))
    '(button ((t (:underline t))))
    '(dired-directory ((t (:foreground "#245032"))))
    '(dired-header ((t (:foreground "#91bb9e"))))
    '(nxml-attribute-local-name ((t (:foreground "#497958"))))
    '(nxml-element-local-name ((t (:foreground "#497958"))))
    '(nxml-tag-delimiter ((t (:foreground "#497958"))))
    '(nxml-prolog-keyword ((t (:foreground "#497958"))))
    '(nxml-markup-declaration-delimiter ((t (:foreground "#497958"))))
    '(mumamo-border-face-in ((t (:italic nil :underline nil :foreground "#497958"))))
    '(mumamo-border-face-out ((t (:italic nil :underline nil :foreground "#497958"))))
    '(mumamo-background-chunk-major ((t (:background "#002116"))))
    '(mumamo-background-chunk-submode1 ((t (:background "#002116"))))
    '(whitespace-space ((t (:background "#002116" :slant italic :foreground "#001a10"))))
    '(whitespace-hspace ((t (:background "#002116" :slant italic :foreground "#001a10"))))
    '(whitespace-tab ((t (:background "#002116" :slant italic :foreground "#001a10"))))
    '(whitespace-newline ((t (:background "#002116" :slant italic :foreground "#001a10"))))
    '(whitespace-trailing ((t (:background "#FF6800" :slant italic :foreground "#001a10"))))
    '(whitespace-line ((t (:background "#001a10" :slant italic :foreground "#FF6800"))))
    '(whitespace-space-before-tab ((t (:background "#FF6800" :slant italic :foreground "#001a10"))))
    '(whitespace-indentation ((t (:background "#FF6800" :slant italic :foreground "#001a10"))))
    '(whitespace-empty ((t (:background "#FF6800" :slant italic :foreground "#001a10"))))
    '(whitespace-space-after-tab ((t (:background "#FF6800" :slant italic :foreground "#001a10"))))
    '(magit-section-title ((t (:background "#002116" :foreground "#497958" :box nil))))
    '(magit-item-highlight ((t (:background nil :foreground nil :inherit nil))))
    '(magit-branch ((t (:background "#002116" :foreground "#497958" :box nil))))
    '(magit-diff-add ((t (:foreground "#f8f8f8" :background "#084Eb9" :weight bold :underline nil))))
    '(magit-diff-del ((t (:foreground "#f8f8f8" :background "#FF6800" :weight bold :underline nil))))
    '(magit-diff-none ((t (:foreground "#497958" :background "#002116" :inherit nil))))
    '(header-line ((t (:background "#1c482b" :foreground "#000000" :box nil))))
    '(sp-show-pair-match-face ((t (:background "#1c482b" :box nil))))
    '(sp-pair-overlay-face ((t (:background "#1c482b" :box nil))))
    '(compilation-info ((t (:foreground "#91bb9e"))))
    '(anzu-mode-line ((t (:foreground "#91bb9e"))))
    '(which-func ((t (:foreground "#91bb9e"))))
)

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'django)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; django-theme.el ends here
