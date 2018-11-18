;;; jbeans-theme.el --- Jbeans theme for GNU Emacs 24 (deftheme)

;; Author: Adam Olsen <arolsen@gmail.com>
;; URL: <https://github.com/synic/jbeans-emacs>
;; Version: 1.3
;; Package-Requires: ((emacs "24"))
;; Based On: ujelly by Mark Tran
;; Package-Version: 20180309.1625
;; Package-X-Original-Version: 20151208.2136
;; "What do you mean, no one calls you JBeans?!  I call you JBeans!"
;; -- Wilbur

;; Based on ujelly by Mark Tran <mark.tran@gmail.com>
;; Orig-URL: http://github.com/marktran/color-theme-ujelly
;; Inspired by jellybeans: http://www.vim.org/scripts/script.php?script_id=2555.


;; Copyright (c) 2017 Adam Olsen <arolsen@gmail.com>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(deftheme jbeans "The jbeans color theme")

(let ((class '((class color) (min-colors 89)))
      ;;                                            GUI       TER
      (jbeans-fg         (if (display-graphic-p) "#cccccc" "#cccccc"))
      (jbeans-bg         (if (display-graphic-p) "#151515" "#151515"))
      (jbeans-grey-0     (if (display-graphic-p) "#151515" "#151515"))
      (jbeans-grey-1     (if (display-graphic-p) "#112433" "#112433"))
      (jbeans-grey-2     (if (display-graphic-p) "#222222" "#222222"))
      (jbeans-grey-3     (if (display-graphic-p) "#333344" "#444455"))
      (jbeans-grey-4     (if (display-graphic-p) "#363636" "#363636"))
      (jbeans-grey-5     (if (display-graphic-p) "#444444" "#444444"))
      (jbeans-grey-6     (if (display-graphic-p) "#7f7f7f" "#7f7f7f"))
      (jbeans-grey-7     (if (display-graphic-p) "#888888" "#888888"))
      (jbeans-purple-0   (if (display-graphic-p) "#ff73fd" "#ff73fd"))
      (jbeans-purple-1   (if (display-graphic-p) "#cd00cd" "#cd00cd"))
      (jbeans-purple-2   (if (display-graphic-p) "#a40073" "#a40073"))
      (jbeans-purple-3   (if (display-graphic-p) "#540063" "#540063"))
      (jbeans-purple-4   (if (display-graphic-p) "#474e90" "#474e90"))
      (jbeans-purple-5   (if (display-graphic-p) "#202025" "#202025"))
      (jbeans-blue-0     (if (display-graphic-p) "#8197bf" "#8197bf"))
      (jbeans-blue-1     (if (display-graphic-p) "#8fbfdc" "#8fbfdc"))
      (jbeans-blue-2     (if (display-graphic-p) "#b2e2fe" "#b2e2fe"))
      (jbeans-blue-3     (if (display-graphic-p) "#447799" "#447799"))
      (jbeans-blue-4     (if (display-graphic-p) "#0b24fb" "#0b24fb"))
      (jbeans-blue-5     (if (display-graphic-p) "#2dfffe" "#2dfffe"))
      (jbeans-green-0    (if (display-graphic-p) "#ddffdd" "#ddffdd"))
      (jbeans-green-1    (if (display-graphic-p) "#b6edb6" "#b6edb6"))
      (jbeans-green-2    (if (display-graphic-p) "#448844" "#448844"))
      (jbeans-green-3    (if (display-graphic-p) "#556a32" "#556a32"))
      (jbeans-green-4    (if (display-graphic-p) "#335533" "#335533"))
      (jbeans-green-5    (if (display-graphic-p) "#99ad6a" "#99ad6a"))
      (jbeans-green-6    (if (display-graphic-p) "#a8ff60" "#a8ff60"))
      (jbeans-green-7    (if (display-graphic-p) "#29fd2f" "#29fd2f"))
      (jbeans-yellow-0   (if (display-graphic-p) "#ffffcc" "#ffffcc"))
      (jbeans-yellow-1   (if (display-graphic-p) "#ffff00" "#ffff00"))
      (jbeans-yellow-2   (if (display-graphic-p) "#eddb87" "#eddb87"))
      (jbeans-yellow-3   (if (display-graphic-p) "#fad07a" "#fad07a"))
      (jbeans-orange-0   (if (display-graphic-p) "#ffb964" "#ffb964"))
      (jbeans-orange-1   (if (display-graphic-p) "#ff8c00" "#ebbd87"))
      (jbeans-orange-2   (if (display-graphic-p) "#b78521" "#b78521"))
      (jbeans-red-0      (if (display-graphic-p) "#cb8165" "#cb8165"))
      (jbeans-red-1      (if (display-graphic-p) "#cf6a4c" "#cf6a4c"))
      (jbeans-red-2      (if (display-graphic-p) "#de5577" "#de5577"))
      (jbeans-red-3      (if (display-graphic-p) "#fc644d" "#fc644d"))
      (jbeans-red-4      (if (display-graphic-p) "#dd0093" "#dd0093"))
      (jbeans-red-5      (if (display-graphic-p) "#aa4444" "#aa4444"))
      (jbeans-red-6      (if (display-graphic-p) "#8a3b3c" "#8a3b3c"))
      (jbeans-red-7      (if (display-graphic-p) "#663333" "#663333"))
      (jbeans-red-8      (if (display-graphic-p) "#fc0d1b" "#fc0d1b"))
      (jbeans-red-9      (if (display-graphic-p) "#ffdddd" "#ffdddd"))
      )

      (custom-theme-set-variables
        'jbeans
        `(linum-format " %3i "))

      (custom-theme-set-faces
       'jbeans
       `(default                             ((,class (:foreground ,jbeans-fg :background ,jbeans-bg))))
;;;;; ACE/Avy
       `(aw-leading-char-face                ((,class (:foreground ,jbeans-red-3 :height 1.2))))
;;;;; Alchemist
       `(alchemist-test--failed-face         ((,class (:foreground ,jbeans-red-1))))
       `(alchemist-test--success-face        ((,class (:foreground ,jbeans-green-5))))
;;;;; Company
       `(company-preview-common              ((,class (:foreground nil :background ,jbeans-purple-4))))
       `(company-scrollbar-bg                ((,class (:background ,jbeans-grey-0))))
       `(company-scrollbar-fg                ((,class (:background ,jbeans-grey-7))))
       `(company-tooltip                     ((,class (:background ,jbeans-grey-0 :foreground ,jbeans-fg :weight bold))))
       `(company-tooltip-annotation          ((,class (:inherit company-tooltip :foreground ,jbeans-blue-0))))
       `(company-tooltip-common              ((,class (:inherit company-tooltip :weight bold :underline nil))))
       `(company-tooltip-common-selection    ((,class (:inherit company-tooltip-selection :foreground ,jbeans-fg :underline nil :weight bold))))
       `(company-tooltip-selection           ((,class (:background ,jbeans-purple-4))))
;;;;; Compilation
       `(compilation-error                   ((,class (:foreground ,jbeans-red-1))))
       `(compilation-info                    ((,class (:foreground ,jbeans-yellow-3))))
       `(compilation-line-number             ((,class (:foreground ,jbeans-grey-7))))
       `(compilation-mode-line-exit          ((,class (:foreground ,jbeans-green-5))))
       `(compilation-mode-line-fail          ((,class (:foreground ,jbeans-red-1))))
       `(compilation-mode-line-run           ((,class (:foreground ,jbeans-yellow-3))))
;;;;; Dired
       `(diredp-compressed-file-name         ((,class (:foreground ,jbeans-red-7))))
       `(diredp-compressed-file-suffix       ((,class (:foreground ,jbeans-fg))))
       `(diredp-date-time                    ((,class (:foreground ,jbeans-green-5))))
       `(diredp-deletion                     ((,class (:foreground ,jbeans-red-1 :background ,jbeans-bg))))
       `(diredp-dir-heading                  ((,class (:foreground ,jbeans-yellow-3 :background ,jbeans-bg))))
       `(diredp-dir-priv                     ((,class (:foreground ,jbeans-purple-2 :background ,jbeans-bg))))
       `(diredp-exec-priv                    ((,class (:foreground ,jbeans-orange-2 :background ,jbeans-bg))))
       `(diredp-file-name                    ((,class (:foreground ,jbeans-green-0))))
       `(diredp-file-suffix                  ((,class (:foreground ,jbeans-fg))))
       `(diredp-flag-mark                    ((,class (:foreground ,jbeans-blue-0 :weight bold))))
       `(diredp-flag-mark-line               ((,class (:foreground ,jbeans-purple-4 :weight bold))))
       `(diredp-link-priv                    ((,class (:foreground ,jbeans-fg))))
       `(diredp-number                       ((,class (:foreground ,jbeans-grey-6))))
       `(diredp-no-priv                      ((,class (:foreground ,jbeans-fg :background ,jbeans-bg))))
       `(diredp-rare-priv                    ((,class (:foreground ,jbeans-red-1 :background ,jbeans-bg))))
       `(diredp-read-priv                    ((,class (:foreground ,jbeans-yellow-3 :background ,jbeans-bg))))
       `(diredp-symlink                      ((,class (:foreground ,jbeans-blue-0))))
       `(diredp-dir-name                     ((,class (:foreground ,jbeans-blue-2))))
       `(diredp-write-priv                   ((,class (:foreground ,jbeans-blue-0 :background ,jbeans-bg))))
;;;;; Emmet
       `(emmet-preview-output                ((,class (:background ,jbeans-purple-4))))
;;;;; Elixir
       `(elixir-atom-face                    ((,class (:foreground ,jbeans-blue-1))))
;;;;; ERC
       `(erc-notice-face                     ((,class (:foreground ,jbeans-yellow-3))))
       `(erc-prompt-face                     ((,class (:foreground ,jbeans-fg))))
       `(erc-timestamp-face                  ((,class (:foreground ,jbeans-blue-0))))
;;;;;; EShell
       `(eshell-prompt                       ((,class (:foreground ,jbeans-red-1))))
       `(eshell-ls-directory                 ((,class (:weight normal :foreground ,jbeans-green-6))))
       `(eshell-ls-executable                ((,class (:weight normal :foreground ,jbeans-red-1))))
       `(eshell-ls-product                   ((,class (:foreground ,jbeans-fg))))
       `(eshell-ls-symlink                   ((,class (:weight normal :foreground ,jbeans-purple-1))))
;;;;; Evil
       `(evil-visual-mark-face               ((,class (:weight ultra-bold :box ,jbeans-blue-0 :foreground ,jbeans-green-7))))
;;;;; FCI Ruler
       ;; As of now, this does nothing, because fci-rule-color is not a face yet.
       `(fci-rule-color                      ((,class (:foreground ,jbeans-grey-4 :background ,jbeans-grey-4))))
;;;;; Fonts
       `(font-lock-builtin-face              ((,class (:foreground ,jbeans-blue-1))))
       `(font-lock-comment-face              ((,class (:slant italic :foreground ,jbeans-grey-7))))
       `(font-lock-constant-face             ((,class (:foreground ,jbeans-blue-3))))
       `(font-lock-doc-face                  ((,class (:foreground ,jbeans-green-5))))
       `(font-lock-function-name-face        ((,class (:foreground ,jbeans-yellow-3))))
       `(font-lock-keyword-face              ((,class (:foreground ,jbeans-blue-0))))
       `(font-lock-preprocessor-face         ((,class (:foreground ,jbeans-fg))))
       `(font-lock-string-face               ((,class (:foreground ,jbeans-green-5))))
       `(font-lock-type-face                 ((,class (:foreground ,jbeans-orange-0))))
       `(font-lock-variable-name-face        ((,class (:foreground ,jbeans-red-1))))
       `(font-lock-warning-face              ((,class (:foreground ,jbeans-red-4))))
       `(font-lock-regexp-grouping-construct ((,class (:foreground ,jbeans-yellow-3 :bold t))))
       `(font-lock-regexp-grouping-backslash ((,class (:foreground ,jbeans-red-1 :bold t))))
;;;;; Fringe
       `(fringe                              ((,class (:foreground ,jbeans-fg :background ,jbeans-bg))))
;;;;; Header
       `(header-line                         ((,class (:foreground ,jbeans-fg))))
;;;;; Helm
       `(helm-visible-mark                   ((,class (:background ,jbeans-green-4 :foreground ,jbeans-bg))))
       `(helm-buffer-file                    ((,class (:foreground ,jbeans-fg))))
       `(helm-buffer-directory               ((,class (:foreground ,jbeans-blue-0))))
       `(helm-buffer-process                 ((,class (:foreground ,jbeans-yellow-3))))
       `(helm-buffer-size                    ((,class (:foreground ,jbeans-fg))))
       `(helm-candidate-number               ((,class (:foreground ,jbeans-fg :background ,jbeans-bg))))
       `(helm-grep-lineno                    ((,class (:foreground ,jbeans-fg))))
       `(helm-grep-finish                    ((,class (:foreground ,jbeans-blue-2))))
       `(helm-match                          ((,class (:foreground ,jbeans-red-4 :background ,jbeans-bg))))
       `(helm-moccur-buffer                  ((,class (:foreground ,jbeans-yellow-3))))
       `(helm-selection                      ((,class (:background ,jbeans-grey-3))))
       `(helm-source-header                  ((,class (:foreground ,jbeans-yellow-3 :background ,jbeans-grey-0))))
       `(helm-swoop-target-line-face         ((,class (:foreground ,jbeans-fg :background ,jbeans-grey-4))))
       `(helm-swoop-target-word-face         ((,class (:foreground ,jbeans-red-4))))
       `(helm-ff-file                        ((,class (:foreground ,jbeans-fg))))
       `(helm-ff-directory                   ((,class (:foreground ,jbeans-blue-2))))
       `(helm-ff-executable                  ((,class (:foreground ,jbeans-green-5))))
;;;;; Highlight
       `(highlight                           ((,class (:background ,jbeans-grey-3))))
       `(hl-line                             ((,class (:background ,jbeans-purple-5))))
;;;;; iSearch
       `(isearch                             ((,class (:foreground ,jbeans-fg :background ,jbeans-red-4))))
       `(isearch-fail                        ((,class (:background ,jbeans-red-4))))
;;;;; Ido
       `(ido-first-match                     ((,class (:foreground ,jbeans-yellow-3))))
       `(ido-only-match                      ((,class (:foreground ,jbeans-green-5))))
       `(ido-subdir                          ((,class (:foreground ,jbeans-fg))))
       `(ido-virtual                         ((,class (:foreground
                                                       ,jbeans-blue-0))))
;;;;; Ivy
       `(ivy-current-match                   ((,class (:background ,jbeans-grey-3))))
       `(ivy-minibuffer-match-face-1         ((,class (:foreground ,jbeans-yellow-3))))
       `(ivy-minibuffer-match-face-2         ((,class (:foreground ,jbeans-orange-0))))
       `(ivy-minibuffer-match-face-3         ((,class (:foreground ,jbeans-blue-1))))
       `(ivy-minibuffer-match-face-4         ((,class (:foreground ,jbeans-purple-1))))
       `(ivy-subdir                          ((,class (:foreground ,jbeans-green-4))))
       `(ivy-modified-buffer                 ((,class (:foreground ,jbeans-red-2))))
;;;;; Jabber
       `(jabber-activity-personal-face       ((,class (:foreground ,jbeans-green-6 :weight bold))))
       `(jabber-activity-face                ((,class (:foreground ,jbeans-red-3 :weight bold))))
       `(jabber-roster-user-online           ((,class (:foreground ,jbeans-blue-0))))
       `(jabber-roster-user-dnd              ((,class (:foreground ,jbeans-red-3))))
       `(jabber-chat-prompt-system           ((,class (:foreground ,jbeans-green-6))))
       `(jabber-chat-prompt-local            ((,class (:foreground ,jbeans-blue-0))))
       `(jabber-chat-prompt-foreign          ((,class (:foreground ,jbeans-green-2))))
;;;;; Lazy highlight
       `(lazy-highlight                      ((,class (:foreground ,jbeans-red-4 :background nil))))
;;;;; Linum
       `(linum                               ((,class (:foreground ,jbeans-grey-5 :background ,jbeans-grey-2))))
;;;;; Display line numbers
       `(line-number                         ((,class (:foreground ,jbeans-grey-5 :background ,jbeans-grey-2))))
;;;;; Ediff
       `(ediff-even-diff-A                   ((,class (:background ,jbeans-grey-2 :foreground ,jbeans-fg))))
       `(ediff-even-diff-B                   ((,class (:background ,jbeans-grey-3 :foreground ,jbeans-fg))))
       `(ediff-even-diff-C                   ((,class (:background ,jbeans-grey-3 :foreground ,jbeans-fg))))
       `(ediff-odd-diff-A                    ((,class (:background ,jbeans-grey-2 :foreground ,jbeans-fg))))
       `(ediff-odd-diff-B                    ((,class (:background ,jbeans-grey-3 :foreground ,jbeans-fg))))
       `(ediff-odd-diff-C                    ((,class (:background ,jbeans-grey-5 :foreground ,jbeans-fg))))
       `(powerline-active1                   ((,class (:inherit mode-line :background ,jbeans-bg))))
       `(powerline-active2                   ((,class (:inherit mode-line :background ,jbeans-grey-3))))
       `(powerline-inactive1                 ((,class (:inherit mode-line :background ,jbeans-bg))))
       `(powerline-inactive2                 ((,class (:inherit mode-line :background ,jbeans-grey-2))))
;;;;; flycheck
       `(flycheck-error                      ((((supports :underline (:style wave)))
                                               (:underline (:style wave :color ,jbeans-red-4) :inherit unspecified))
                                              (,class (:underline (:style line :color ,jbeans-red-4)))))
       `(flycheck-warning                    ((((supports :underline (:style wave)))
                                               (:underline (:style wave :color ,jbeans-yellow-3) :inherit unspecified))
                                              (,class (:underline (:style line :color ,jbeans-yellow-3)))))
       `(flycheck-info                       ((((supports :underline (:style wave)))
                                               (:underline (:style wave :color ,jbeans-blue-5) :inherit unspecified))
                                              (,class (:underline (:style line :color ,jbeans-blue-5)))))
       `(flycheck-fringe-error               ((,class (:foreground ,jbeans-red-4 :weight bold))))
       `(flycheck-fringe-warning             ((,class (:foreground ,jbeans-yellow-3 :weight bold))))
       `(flycheck-fringe-info                ((,class (:foreground ,jbeans-blue-5 :weight bold))))
;;;;; flyspell
       `(flyspell-duplicate                  ((((supports :underline (:style wave)))
                                               (:underline (:style wave :color ,jbeans-orange-1) :inherit unspecified))
                                              (,class (:underline (:style line :color ,jbeans-orange-1)))))
       `(flyspell-incorrect                  ((((supports :underline (:style wave)))
                                               (:underline (:style wave :color ,jbeans-red-8) :inherit unspecified))
                                              (,class (:underline (:style line :color ,jbeans-red-8)))))
;;;;; Git
       `(git-commit-comment-file             ((,class (:background ,jbeans-bg :foreground ,jbeans-fg))))
       `(git-commit-summary                  ((,class (:background ,jbeans-bg :foreground ,jbeans-blue-0))))
       `(git-commit-comment-heading          ((,class (:foreground ,jbeans-yellow-3))))
;;;;; Git-gutter
       `(git-gutter-fr+-modified             ((,class (:foreground ,jbeans-blue-3 :background ,jbeans-bg))))
       `(git-gutter-fr+-added                ((,class (:foreground ,jbeans-green-2 :background ,jbeans-bg))))
       `(git-gutter-fr+-deleted              ((,class (:foreground
                                                       ,jbeans-red-5 :background ,jbeans-bg))))
;;;;; Highlighting
       `(hi-yellow                            ((,class (:box ,jbeans-yellow-1 :foreground ,jbeans-yellow-1 :background, jbeans-grey-4))))
;;;;; Magit
       `(magit-blame-heading                 ((,class (:background ,jbeans-grey-2 :box ,jbeans-purple-4 :weight bold :foreground ,jbeans-fg))))
       `(magit-blame-date                    ((,class (:background ,jbeans-grey-0 :box ,jbeans-green-3 :weight bold :foreground ,jbeans-green-5))))
       `(magit-blame-name                    ((,class (:background ,jbeans-grey-0 :box ,jbeans-green-3 :weight bold :foreground ,jbeans-red-0))))
       `(magit-blame-hash                    ((,class (:background ,jbeans-grey-0 :box ,jbeans-green-3 :weight bold :foreground ,jbeans-blue-3))))
       `(magit-bisect-bad                    ((,class (:foreground ,jbeans-red-6))))
       `(magit-bisect-good                   ((,class (:foreground ,jbeans-green-3))))
       `(magit-bisect-skip                   ((,class (:foreground ,jbeans-orange-2))))
       `(magit-blame-summary                 ((,class (:inherit magit-blame-heading))))
       `(magit-branch-current                ((,class (:inherit magit-branch-local :box 1))))
       `(magit-branch-local                  ((,class (:foreground ,jbeans-blue-2))))
       `(magit-branch-remote                 ((,class (:foreground ,jbeans-green-5))))
       `(magit-cherry-equivalent             ((,class (:foreground ,jbeans-purple-1))))
       `(magit-cherry-unmatched              ((,class (:foreground ,jbeans-blue-5))))
       `(magit-diff-added                    ((,class (:background ,jbeans-green-4 :foreground ,jbeans-green-0))))
       `(magit-diff-added-highlight          ((,class (:background ,jbeans-green-4 :foreground ,jbeans-green-0))))
       `(magit-diff-base                     ((,class (:background ,jbeans-green-3 :foreground ,jbeans-yellow-0))))
       `(magit-diff-base-highlight           ((,class (:background ,jbeans-green-3 :foreground ,jbeans-yellow-0))))
       `(magit-diff-conflict-heading         ((,class (:inherit magit-diff-hunk-heading))))
       `(magit-diff-context                  ((,class (:background ,jbeans-bg :foreground ,jbeans-fg))))
       `(magit-diff-context-highlight        ((,class (:background ,jbeans-bg :foreground ,jbeans-fg))))
       `(magit-diff-file-heading             ((,class (:foreground ,jbeans-blue-0 :weight bold))))
       `(magit-diff-file-heading-highlight   ((,class (:foreground ,jbeans-blue-0 :weight normal))))
       `(magit-diff-file-heading-selection   ((,class (:background ,jbeans-bg :foreground ,jbeans-fg))))
       `(magit-diff-hunk-heading             ((,class (:background ,jbeans-grey-3 :box ,jbeans-grey-3  :foreground ,jbeans-fg :weight bold))))
       `(magit-diff-hunk-heading-highlight   ((,class (:background ,jbeans-grey-3 :box ,jbeans-grey-7 :weight bold :foreground ,jbeans-fg))))
       `(magit-diff-hunk-heading-selection   ((,class (:inherit magit-diff-hunk-heading-highlight :foreground ,jbeans-red-0))))
       `(magit-diff-lines-boundary           ((,class (:inherit magit-diff-lines-heading))))
       `(magit-diff-lines-heading            ((,class (:inherit magit-diff-hunk-heading-highlight :background ,jbeans-red-6 :foreground ,jbeans-fg))))
       `(magit-diff-our                      ((,class (:inherit magit-diff-removed))))
       `(magit-diff-our-highlight            ((,class (:inherit magit-diff-removed-highlight))))
       `(magit-diff-removed                  ((,class (:background ,jbeans-red-7 :foreground ,jbeans-red-9))))
       `(magit-diff-removed-highlight        ((,class (:background ,jbeans-red-7 :foreground ,jbeans-red-9))))
       `(magit-diff-their                    ((,class (:inherit magit-diff-added))))
       `(magit-diff-their-highlight          ((,class (:inherit magit-diff-added-highlight))))
       `(magit-diff-whitespace-warning       ((,class (:inherit trailing-whitespace))))
       `(magit-diffstat-added                ((,class (:foreground ,jbeans-green-2))))
       `(magit-diffstat-removed              ((,class (:foreground ,jbeans-red-5))))
       `(magit-dimmed                        ((,class (:background ,jbeans-bg :foreground ,jbeans-grey-6))))
       `(magit-filename                      ((,class (:foreground ,jbeans-orange-2 :weight normal))))
       `(magit-hash                          ((,class (:foreground ,jbeans-grey-5))))
       `(magit-head                          ((,class (:inherit magit-branch-local))))
       `(magit-header-line                   ((,class (:inherit magit-section-heading))))
       `(magit-log-author                    ((,class (:foreground ,jbeans-blue-0))))
       `(magit-log-date                      ((,class (:foreground ,jbeans-green-5))))
       `(magit-log-graph                     ((,class (:foreground ,jbeans-fg))))
       `(magit-popup-argument                ((,class (:inherit font-lock-warning-face))))
       `(magit-popup-disabled-argument       ((,class (:inherit shadow))))
       `(magit-popup-heading                 ((,class (:inherit font-lock-keyword-face))))
       `(magit-popup-key                     ((,class (:inherit font-lock-builtin-face))))
       `(magit-popup-option-value            ((,class (:inherit font-lock-string-face))))
       `(magit-process-ng                    ((,class (:inherit magit-section-heading :foreground ,jbeans-red-8))))
       `(magit-process-ok                    ((,class (:inherit magit-section-heading :foreground ,jbeans-green-7))))
       `(magit-reflog-amend                  ((,class (:foreground ,jbeans-purple-1))))
       `(magit-reflog-checkout               ((,class (:foreground ,jbeans-blue-5))))
       `(magit-reflog-cherry-pick            ((,class (:foreground ,jbeans-green-7))))
       `(magit-reflog-commit                 ((,class (:foreground ,jbeans-green-7))))
       `(magit-reflog-merge                  ((,class (:foreground ,jbeans-green-7))))
       `(magit-reflog-other                  ((,class (:foreground ,jbeans-blue-5))))
       `(magit-reflog-rebase                 ((,class (:foreground ,jbeans-purple-1))))
       `(magit-reflog-remote                 ((,class (:foreground ,jbeans-blue-5))))
       `(magit-reflog-reset                  ((,class (:foreground ,jbeans-red-8))))
       `(magit-refname                       ((,class (:background ,jbeans-bg :foreground ,jbeans-fg))))
       `(magit-refname-stash                 ((,class (:inherit magit-refname))))
       `(magit-refname-wip                   ((,class (:inherit magit-refname))))
       `(magit-section-heading               ((,class (:background ,jbeans-bg :foreground ,jbeans-yellow-3))))
       `(magit-section-highlight             ((,class (:background ,jbeans-bg))))
       `(magit-section-secondary-heading     ((,class (:background ,jbeans-bg :weight bold))))
       `(magit-sequence-done                 ((,class (:inherit magit-hash))))
       `(magit-sequence-drop                 ((,class (:foreground ,jbeans-red-5))))
       `(magit-sequence-head                 ((,class (:foreground ,jbeans-blue-2))))
       `(magit-sequence-onto                 ((,class (:inherit magit-sequence-done))))
       `(magit-sequence-part                 ((,class (:foreground ,jbeans-yellow-2))))
       `(magit-sequence-pick                 ((,class (:inherit default))))
       `(magit-sequence-stop                 ((,class (:foreground ,jbeans-green-1))))
       `(magit-signature-bad                 ((,class (:foreground ,jbeans-red-8))))
       `(magit-signature-good                ((,class (:foreground ,jbeans-green-7))))
       `(magit-signature-untrusted           ((,class (:foreground ,jbeans-blue-5))))
       `(magit-tag                           ((,class (:foreground ,jbeans-yellow-2))))
;;;;; Match
       `(match                               ((,class (:background ,jbeans-red-4))))
;;;;; Minibuffer
       `(minibuffer-prompt                   ((,class (:foreground ,jbeans-yellow-3))))
;;;;; Modeline
       `(mode-line                           ((,class (:foreground ,jbeans-fg :background ,jbeans-grey-3))))
       `(mode-line-inactive                  ((,class (:foreground ,jbeans-grey-6 :background ,jbeans-grey-2))))
;;;;; NeoTree
       `(neo-dir-link-face                   ((,class (:foreground ,jbeans-blue-0))))
       `(neo-file-link-face                  ((,class (:foreground ,jbeans-fg))))
;;;;; Org
       `(org-checkbox                        ((,class (:foreground ,jbeans-green-5))))
       `(org-date                            ((,class (:foreground ,jbeans-blue-0))))
       `(org-document-title                  ((,class (:foreground ,jbeans-red-9))))
       `(org-done                            ((,class (:foreground ,jbeans-green-2))))
       `(org-level-1                         ((,class (:foreground ,jbeans-orange-0 :weight bold))))
       `(org-level-2                         ((,class (:foreground ,jbeans-green-5 :weight bold))))
       `(org-level-3                         ((,class (:foreground ,jbeans-red-0))))
       `(org-link                            ((,class (:foreground ,jbeans-blue-1))))
       `(org-special-keyword                 ((,class (:foreground ,jbeans-blue-0))))
       `(org-table                           ((,class (:foreground ,jbeans-orange-0))))
       `(org-todo                            ((,class (:foreground ,jbeans-red-1))))
;;;;; Region
       `(region                              ((,class (:background ,jbeans-grey-3))))
;;;;; SHM
       `(shm-current-face                    ((,class (:background ,jbeans-grey-4))))
       `(shm-quarantine-face                 ((,class (:background ,jbeans-red-4))))
;;;;; Smerge
       `(smerge-markers                      ((,class (:foreground ,jbeans-yellow-3 :background ,jbeans-grey-0))))
       `(smerge-refined-change               ((,class (:foreground ,jbeans-green-5))))
;;;;; SmartParens
       `(sp-pair-overlay-face                ((((class color) (min-colors 89)) (:background ,jbeans-grey-2))))
       `(sp-show-pair-match-face             ((((class color) (min-colors 89)) (:background ,jbeans-grey-5))))
;;;;; Spaceline
       `(spaceline-evil-normal               ((,class (:foreground ,jbeans-bg :background ,jbeans-orange-2))))
       `(spaceline-evil-motion               ((,class (:foreground ,jbeans-bg :background ,jbeans-purple-2))))
       `(spaceline-evil-insert               ((,class (:foreground ,jbeans-bg :background ,jbeans-green-2))))
       `(spaceline-evil-visual               ((,class (:foreground ,jbeans-bg :background ,jbeans-grey-5))))
       `(spaceline-evil-replace              ((,class (:foreground ,jbeans-bg :background ,jbeans-red-1))))
       `(spaceline-evil-emacs                ((,class (:foreground ,jbeans-bg :background ,jbeans-blue-5))))
;;;;; Spacemacs
       `(spacemacs-normal-face               ((,class (:foreground ,jbeans-bg :background ,jbeans-orange-2))))
       `(spacemacs-motion-face               ((,class (:foreground ,jbeans-bg :background ,jbeans-purple-2))))
       `(spacemacs-insert-face               ((,class (:foreground ,jbeans-bg :background ,jbeans-green-2))))
       `(spacemacs-visual-face               ((,class (:foreground ,jbeans-bg :background ,jbeans-grey-5))))
       `(spacemacs-lisp-face                 ((,class (:foreground ,jbeans-bg :background ,jbeans-purple-1))))
       `(spacemacs-replace-face              ((,class (:foreground ,jbeans-bg :background ,jbeans-red-1))))
       `(spacemacs-iedit-face                ((,class (:foreground ,jbeans-bg :background ,jbeans-red-8))))
       `(spacemacs-iedit-insert-face         ((,class (:foreground ,jbeans-bg :background ,jbeans-red-8))))
       `(spacemacs-evilified-face            ((,class (:foreground ,jbeans-bg :background ,jbeans-green-3))))
       `(spacemacs-emacs-face                ((,class (:foreground ,jbeans-bg :background ,jbeans-blue-5))))
;;;;; TabBar
       `(tabbar-default                      ((,class (:inherit variable-pitch :background ,jbeans-bg :foreground ,jbeans-fg :height 0.8))))
       `(tabbar-modified                     ((,class (:inherit tabbar-default :foreground ,jbeans-green-5 :box (:line-width 1 :color ,jbeans-grey-5 style: released-button)))))
       `(tabbar-selected                     ((,class (:inherit tabbar-default :foreground ,jbeans-blue-0 :box (:line-width 1 :color ,jbeans-fg style: released-button)))))
       `(tabbar-unselected                   ((,class (:inherit tabbar-default :box (:line-width 1 :color ,jbeans-grey-6 style: released-button)))))
;;;;; Term
       `(term-color-black                    ((,class (:foreground ,jbeans-bg :background ,jbeans-bg))))
       `(term-color-red                      ((,class (:foreground ,jbeans-red-2 :background ,jbeans-red-3))))
       `(term-color-green                    ((,class (:foreground ,jbeans-green-2 :background ,jbeans-green-3))))
       `(term-color-yellow                   ((,class (:foreground ,jbeans-yellow-3 :background ,jbeans-yellow-2))))
       `(term-color-blue                     ((,class (:foreground ,jbeans-blue-0 :background ,jbeans-blue-1))))
       `(term-color-magenta                  ((,class (:foreground ,jbeans-purple-0 :background ,jbeans-purple-3))))
       `(term-color-white                    ((,class (:foreground ,jbeans-fg :background ,jbeans-fg))))
       `(term-default-fg-color               ((,class (:inherit term-color-white))))
       `(term-default-bg-color               ((,class (:inherit term-color-black))))
;;;;; Whitespace
       `(trailing-whitespace                 ((,class (:background ,jbeans-red-4))))
;;;;; Vertical border
       `(vertical-border                     ((,class (:foreground ,jbeans-grey-3))))
;;;;; Web Mode
       `(web-mode-builtin-face               ((,class (:foreground ,jbeans-blue-1))))
       `(web-mode-html-attr-name-face        ((,class (:foreground ,jbeans-blue-0))))
       `(web-mode-html-tag-face              ((,class (:foreground ,jbeans-orange-0))))
       `(web-mode-symbol-face                ((,class (:foreground ,jbeans-blue-3))))
       `(web-mode-function-name-face         ((,class (:foreground ,jbeans-orange-0))))
       `(web-mode-block-control-face         ((,class (:foreground ,jbeans-red-1))))
       `(web-mode-variable-name-face         ((,class (:foreground ,jbeans-blue-2))))
;;;;; More Whitespace
       `(whitespace-trailing                 ((,class (:background ,jbeans-red-4))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'jbeans)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; jbeans-theme.el ends here
