;;; organic-green-theme.el --- Low-contrast green color theme.
;; Package-Version: 20180522.1620

;;; Copyright © 2009-2018 - Kostafey <kostafey@gmail.com>

;; This file is not [yet] part of GNU Emacs, but is distributed under
;; the same terms.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; (load-theme 'organic-green t)

;;; Code:

(make-face 'mode-line-default-face)
(make-face 'mode-line-header)
(make-face 'tcl-substitution-char-face)

(deftheme organic-green
  "Low-contrast green color theme.
Basic, Font Lock, Isearch, Jabber, rst, magit, Web faces are included.")

(let* ((class '((class color) (min-colors 89)))
       ;; Organic-green misc palette colors.
       (geep-sea-green "#339966")
       (sea-eye        "#00A86B")
       (cham-3         "#4e9a06")
       (blue-0         "#8cc4ff")
       (blue-2         "#3465a4")
       (blue-3         "#204a87")
       (orange-3       "#ce5c00")
       (alum-1         "#eeeeec")
       (alum-2         "#d3d7cf")
       (alum-3         "#babdb6")
       (alum-6         "#2e3436")
       (plum-1         "#ad7fa8")
       (red-1          "#ef2929")
       (red-2          "#cc0000")
       (red-3          "#a40000")

       ;; basic colors
       (organic-fg "#326B6B")
       (organic-bg "#F0FFF0")
       (organic-cursor-fg "#225522")
       (organic-comment-fg "gray50")
       (organic-string-fg "#119911")
       (organic-constant-fg "#3465BD")
       (organic-builtin-fg "#009292")
       (minor-green-highlight-background "#D5F0D5")
       (tiny-green-highlight-background "#E3FFE1")
       (minor-grey-highlight-background "#DAEADA")
       (minor-yellow-highlight-background "#F2FFC0") ;#E3F2A1
       (minor-blue-highlight-background "#C0E0FF")
       (minor-red-highlight-background "#FFF0F0"))

  (custom-theme-set-faces
   'organic-green
   `(default ((,class (:foreground ,organic-fg :background ,organic-bg))))
   `(cursor ((,class (:background ,organic-cursor-fg))))
   `(hl-line ((,class (:background "#A0F0A0" :inverse-video nil))))

   `(mode-line-default-face ((,class (:foreground ,organic-fg))))
   `(mode-line-header ((t (:foreground "gray25" :weight bold))))

   ;; Highlighting faces
   `(fringe ((,class (:background "#E5E5E5" :foreground "gray40"))))
   `(highlight ((,class (:background ,minor-green-highlight-background))))
   `(region ((,class (:foreground ,organic-fg :background ,"#EEEEA0"))))
   `(cua-rectangle ((,class (:foreground ,organic-fg :background ,"#BFFF00"))))
   `(secondary-selection ((,class (:background ,blue-0))))
   `(isearch ((,class (:foreground ,organic-fg :background "yellow" :inverse-video nil))))
   `(lazy-highlight ((,class (:background "#DDEE00" :inverse-video nil))))
   `(trailing-whitespace ((,class (:background ,red-1))))

   ;; Mode line faces
   `(mode-line ((,class (:box (:line-width -1 :style released-button)
                              :background ,alum-2 :foreground ,alum-6))))
   `(mode-line-inactive ((,class (:box (:line-width -1 :style released-button)
                                       :background ,alum-1 :foreground ,alum-6))))

   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:weight bold :foreground ,blue-3))))
   `(escape-glyph ((,class (:foreground ,sea-eye))))
   `(error ((,class (:foreground ,red-3 :weight bold))))
   `(warning ((,class (:foreground ,orange-3))))
   `(success ((,class (:foreground ,cham-3))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,organic-builtin-fg))))
   `(font-lock-comment-face ((,class (:foreground ,organic-comment-fg))))
   `(font-lock-constant-face ((,class (:foreground ,organic-constant-fg))))
   `(font-lock-function-name-face ((,class (:weight extra-bold :foreground "#3063EA")))) ;"blue" "#1155CF" "#3032FF" "#3063EA"
   `(font-lock-keyword-face ((,class (:weight semi-bold :foreground "purple"))))
   `(font-lock-string-face ((t (:foreground ,organic-string-fg))) t) ; "ForestGreen"
   `(font-lock-type-face ((t (:foreground ,organic-builtin-fg :weight bold))))
   `(font-lock-variable-name-face ((,class (:width condensed :foreground "DarkGoldenrod"))))
   `(font-lock-warning-face ((,class (:foreground "#AA0000" :weight bold))))

   ;; Button and link faces
   `(link ((,class (:underline t :foreground ,blue-3))))
   `(link-visited ((,class (:underline t :foreground ,blue-2))))

   ;; Jabber
   '(jabber-roster-user-chatty ((t (:inherit font-lock-type-face :bold tx))))
   '(jabber-roster-user-online ((t (:inherit font-lock-keyword-face :bold t))))
   `(jabber-roster-user-offline ((t (:foreground ,organic-fg :background ,organic-bg))))
   '(jabber-roster-user-away ((t (:inherit font-lock-doc-face))))
   '(jabber-roster-user-xa ((t (:inherit font-lock-doc-face))))
   '(jabber-roster-user-dnd ((t (:inherit font-lock-comment-face))))
   '(jabber-roster-user-error ((t (:inherit font-lock-warning-face))))

   '(jabber-title-small ((t (:height 1.2 :weight bold))))
   '(jabber-title-medium ((t (:inherit jabber-title-small :height 1.2))))
   '(jabber-title-large ((t (:inherit jabber-title-medium :height 1.2))))

   '(jabber-chat-prompt-local ((t (:inherit font-lock-string-face :bold t))))
   '(jabber-chat-prompt-foreign ((t (:inherit font-lock-function-name-face :bold nil))))
   '(jabber-chat-prompt-system ((t (:inherit font-lock-comment-face :bold t))))
   '(jabber-rare-time-face ((t (:inherit font-lock-function-name-face :bold nil))))

   '(jabber-activity-face ((t (:inherit jabber-chat-prompt-foreign))))
   '(jabber-activity-personal-face ((t (:inherit jabber-chat-prompt-local :bold t))))

   ;; LaTeX
   '(font-latex-bold-face ((t (:bold t :foreground "DarkOliveGreen"))))
   '(font-latex-italic-face ((t (:italic t :foreground "DarkOliveGreen"))))
   '(font-latex-math-face ((t (:foreground "DarkGoldenrod"))))
   '(font-latex-sedate-face ((t (:foreground "DimGray"))))
   '(font-latex-string-face ((t (nil))))
   '(font-latex-warning-face ((t (:bold t :weight semi-bold :foreground "#00CC00"))))

   ;; quack
   '(quack-pltish-paren-face ((((class color) (background light)) (:foreground "#53AD2F"))))
   '(quack-pltish-keyword-face ((t (:foreground "#A020F0" :weight bold))))

   ;; js2-mode
   '(js2-external-variable ((t (:foreground "DodgerBlue3"))))
   `(js2-function-param ((t (:foreground ,organic-builtin-fg)))) ;"SeaGreen" ,sea-eye

   ;; clojure/CIDER
   `(cider-result-overlay-face ((t (:background ,organic-bg :box (:line-width -1 :color "#F0F0A1")))))

   ;; java
   `(jdee-java-properties-font-lock-comment-face ((t (:foreground ,organic-comment-fg))))
   '(jdee-java-properties-font-lock-equal-face ((t (:foreground "DodgerBlue3"))))
   '(jdee-java-properties-font-lock-substitution-face ((t (:inherit font-lock-function-name-face :bold nil))))
   '(jdee-java-properties-font-lock-class-name-face ((t (:inherit font-lock-constant-face :bold nil))))
   '(jdee-java-properties-font-lock-value-face ((t (:inherit font-lock-string-face :bold nil))))
   `(jdee-java-properties-font-lock-backslash-face ((t (:foreground ,sea-eye))))

   ;; scala
   `(scala-font-lock:var-face ((t (:foreground ,orange-3))))
   `(ensime-result-overlay-face ((t (:background ,organic-bg :foreground ,organic-comment-fg :box (:line-width -1 :color "#F0F0A1")))))

   ;; Tcl
   '(tcl-substitution-char-face ((t (:foreground "OliveDrab4"))))

   ;; erc
   '(erc-action-face ((t (:foreground "gray" :weight bold))))
   '(erc-command-indicator-face ((t (:foreground "black" :weight bold))))
   '(erc-nick-default-face ((t (:foreground "SlateBlue" :weight bold))))
   '(erc-input-face ((t (:foreground "#000099"))))
   '(erc-notice-face ((t (:foreground "dark sea green" :weight bold))))
   '(erc-timestamp-face ((t (:foreground "#32CD32" :weight bold))))

   ;; circe
   '(circe-server-face ((t (:foreground "dark sea green"))))
   '(circe-prompt-face ((t (:foreground "gray25" :background "LightSeaGreen" :weight bold))))
   '(lui-time-stamp-face ((t (:foreground "#32CD32"))))
   '(circe-highlight-nick-face ((t (:foreground "DarkTurquoise")))) ;"#0445b7"

   ;; rst
   '(rst-definition ((t (:inherit font-lock-constant-face))) t)
   `(rst-level-1 ((t (:background ,minor-green-highlight-background))) t)
   `(rst-level-2 ((t (:background ,minor-grey-highlight-background))))
   `(rst-level-3 ((t (:background ,minor-grey-highlight-background))))
   `(rst-level-4 ((t (:background ,minor-grey-highlight-background))))
   `(rst-level-5 ((t (:background ,minor-grey-highlight-background))))
   `(rst-level-6 ((t (:background ,minor-grey-highlight-background))))
   '(rst-block ((t (:inherit font-lock-function-name-face :bold t))) t)
   '(rst-external ((t (:inherit font-lock-constant-face))) t)
   '(rst-directive ((t (:inheit font-lock-builtin-face))) t)
   '(rst-literal ((t (:inheit font-lock-string-face))))
   '(rst-emphasis1 ((t (:inherit italic))) t)
   `(rst-adornment ((t (:bold t :foreground ,blue-2))))

   ;; whitespace-mode
   `(whitespace-empty ((t (:background ,organic-bg :foreground ,alum-3))) t)
   `(whitespace-indentation ((t (:background ,organic-bg :foreground ,alum-3))) t)
   `(whitespace-newline ((t (:background ,organic-bg :foreground ,alum-3))) t)
   `(whitespace-space-after-tab ((t (:background ,organic-bg :foreground ,alum-3))) t)
   `(whitespace-tab ((t (:background ,organic-bg :foreground ,alum-3))) t)
   `(whitespace-hspace ((t (:background ,organic-bg :foreground ,alum-3))) t)
   `(whitespace-line ((t (:background ,organic-bg :foreground ,alum-3))) t)
   `(whitespace-space ((t (:background ,organic-bg :foreground ,alum-3))) t)
   `(whitespace-space-before-tab ((t (:background ,organic-bg :foreground ,alum-3))) t)
   `(whitespace-trailing ((t (:background ,organic-bg :foreground ,plum-1))) t)

   ;; diff
   '(diff-indicator-added ((t (:foreground "#339933"))) t)
   '(diff-added ((t (:foreground "#339933"))) t)
   `(diff-indicator-removed ((t (:foreground ,red-2))) t)
   `(diff-removed ((t (:foreground ,red-2))) t)

   ;; magit
   '(magit-diff-add ((t (:foreground "#339933"))) t)
   `(magit-diff-del ((t (:foreground ,red-2))) t)
   `(magit-diff-added ((t (:foreground "#22aa22" :background "#ddffdd"))) t)
   `(magit-diff-removed ((t (:foreground "#aa2222" :background "#ffdddd"))) t)
   `(magit-diff-added-highlight ((t (:foreground "#22aa22" :background "#cceecc"))) t)
   `(magit-diff-removed-highlight ((t (:foreground "#aa2222" :background "#eecccc"))) t)
   `(magit-diff-context-highlight ((t (:background ,organic-bg :foreground "grey50"))) t)
   `(magit-diff-file-heading-highlight ((t (:background ,minor-green-highlight-background))) t)
   '(magit-item-highlight ((t (:background "#E3F2E1"))) t)
   '(magit-log-author ((t (:foreground "SpringGreen4"))) t)
   '(magit-popup-argument ((t (:foreground "#3032FF"))) t)
   `(magit-process-ok ((t (:foreground ,organic-string-fg))) t)
   `(magit-section-highlight ((t (:background ,minor-green-highlight-background))) t)
   '(magit-branch-remote ((t (:foreground "DarkOliveGreen4"))) t)
   '(magit-section-heading ((t (:bold t :foreground "DarkGoldenrod4"))) t)

   ;; git-gutter
   '(git-gutter:added ((t (:foreground "#339933"))) t)
   `(git-gutter:deleted ((t (:foreground ,red-2))) t)
   '(git-gutter:modified ((t (:foreground "DodgerBlue3"))) t)

   ;; git-gutter-fringe
   '(git-gutter-fr:added ((t (:foreground "#339933"))) t)
   `(git-gutter-fr:deleted ((t (:foreground ,red-2))) t)
   `(git-gutter-fr:modified ((t (:foreground ,organic-comment-fg))) t)

   ;; org-mode
   `(org-table ((t (:foreground ,organic-builtin-fg))) t)
   `(org-level-4 ((t (:foreground ,sea-eye))) t)

   ;; misc
   '(nxml-element-local-name ((t (:foreground "#0066CC" :weight normal))) t)
   '(speedbar-tag-face ((t (:foreground "DarkSlateGray4"))))
   '(yas-field-highlight-face ((t (:background "#DDEE00"))))
   `(idle-highlight ((t (:foreground ,organic-fg :background ,minor-yellow-highlight-background))) t)
   `(comint-highlight-prompt ((t (:foreground ,organic-constant-fg :weight bold))) t)
   `(speedbar-selected-face ((t (:foreground ,geep-sea-green :underline t))) t)

   '(flx-highlight-face  ((t (:foreground "#0066CC" :bold t :underline t))) t)

   ;; powerline
   `(powerline-active1 ((t (:background ,alum-3 :inherit mode-line))) t)
   `(powerline-active2 ((t (:background ,alum-2 :inherit mode-line))) t)
   '(powerline-inactive1  ((t (:background "grey70" :inherit mode-line-inactive))) t)
   '(powerline-inactive2  ((t (:background "grey80" :inherit mode-line-inactive))) t)

   ;; tabbar
   `(tabbar-modified ((t (:inherit tabbar-default :foreground "#118811"
                          :bold t
                          :box (:line-width 1 :color "white"
                                :style released-button)))))
   `(tabbar-selected ((t :inherit tabbar-default
                         :box (:line-width 1 :color "white" :style pressed-button)
                         :foreground ,alum-6 :bold t)))
   `(tabbar-selected-modified ((t :inherit tabbar-selected)))

   ;; web-mode
   `(web-mode-current-element-highlight-face
     ((,class (:background ,minor-green-highlight-background))))
   '(web-mode-html-tag-face ((t (:foreground "grey28"))) t)
   '(web-mode-html-attr-name-face ((t (:foreground "#4045F0"))) t)
   `(web-mode-doctype-face ((t (:foreground ,organic-constant-fg))) t)
   `(web-mode-comment-face ((t (:foreground ,organic-comment-fg))) t)
   `(web-mode-css-selector-face ((t (:foreground ,organic-builtin-fg))) t)
   `(web-mode-function-call-face ((t :inherit organic-fg)))
   `(web-mode-function-name-face ((t :inherit font-lock-function-name-face)))

   `(eldoc-highlight-function-argument
     ((t (:foreground ,organic-string-fg :weight bold))) t)

   `(table-cell ((t (:foreground ,organic-fg :background ,tiny-green-highlight-background))) t)

   ;; dired
   `(diredp-dir-heading ((t (:background ,tiny-green-highlight-background))))
   `(diredp-dir-name ((t (:foreground ,alum-6))))
   `(diredp-file-name ((t (:foreground ,organic-fg))))
   `(diredp-file-suffix ((t (:foreground ,organic-builtin-fg))))

   ;;Highlight pair parentheses
   `(show-paren-match ((t (:background "#F0F0A1"))))
   `(show-paren-mismatch ((t (:background ,minor-red-highlight-background))))

   ;; rainbow-delimiters
   ;; (1 (2 (3 (4 (5 (6 (7 (8 (9 (10 (11 (12))))))))))))
   `(rainbow-delimiters-depth-1-face ((t (:foreground "#666666" :background ,organic-bg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground "#5544EE" :background ,organic-bg))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground "#2265DC" :background ,organic-bg))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground "#00A89B" :background ,organic-bg))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground "#229900" :background ,organic-bg))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground "#999900" :background ,organic-bg))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground "#F57900" :background ,organic-bg))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground "#EE66E8" :background ,organic-bg))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground "purple"  :background ,organic-bg))))
   ))

(custom-theme-set-variables
 'organic-green

 ;; lisp parentheses rainbow
 `(hl-paren-colors '("#326B6B"))
 `(hl-paren-background-colors
   '("#00FF99" "#CCFF99" "#FFCC99" "#FF9999" "#FF99CC"
     "#CC99FF" "#9999FF" "#99CCFF" "#99FFCC" "#7FFF00"))

 ;; fill-column-indicator
 `(fci-rule-color "gray80")

 ;; marker
 `(highlight-symbol-colors
   '("#EFFF00" "#73CD4F" "#83DDFF" "MediumPurple1" "#66CDAA"
     "DarkOrange" "HotPink1" "#809FFF" "#ADFF2F"))

 ;; org-mode code blocks
 `(org-src-block-faces '(("emacs-lisp" (:background "#F0FFF0")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'organic-green)

;;; organic-green-theme.el ends here
