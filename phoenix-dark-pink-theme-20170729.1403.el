;;; phoenix-dark-pink-theme.el --- Originally a port of the Sublime Text 2 theme

;; Copyright 2013-2017 J Irving

;; Author: J Irving <j@lollyshouse.ca>
;; URL: http://github.com/j0ni/phoenix-dark-pink
;; Package-Version: 20170729.1403
;; Version: 2

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Org-mode mods from Rikard Glans - https://github.com/darrik/phoenix-dark-pink

;; Code:

(unless (>= emacs-major-version 24)
  (error "phoenix-dark-pink-theme requires Emacs 24 or later."))

(defmacro define-phoenix-dark-pink-theme (&rest body)
  `(let ((p0 "#ffcfff")
         (p1 "#efbfef")
         (p2 "#e1b1ed")
         (p3 "#dfafdf")
         (p4 "#d1afdd")
         (p5 "#d1a1dd")
         (p6 "#c19fcf")
         (p7 "#bf8fbf")
         (p8 "#b294bb")
         (p9 "#a582a3")
         (p10 "#9f6f9f")
         (p11 "#8f5f8f")
         (p12 "#815f8d")
         (p13 "#755273")

         (fg "#cccccc")
         (bg "#101010")

         (plegit "pink")

         (damned "red")
         (alarmed "#f582a3")
         (warned "#87003f")
         (weirdyellow "#c0af7f")

         (dp1 "#31182d")
         (dp2 "#412b3f")
         (dp3 "#714161")

         (offpink1 "#f0dfff")
         (offpink2 "#d0bfdf")
         (offpink3 "thistle")

         (offwhite1 "#efefef")
         (offwhite2 "#e0e0e0")
         (offwhite3 "#dddddd")
         (offwhite4 "#b3b3b3")

         (silverfox "#787878")
         (sadsilverfox "#585858")

         (b1 "#202020")
         (b2 "#2d2d2d")
         (b3 "#393939"))

     (deftheme phoenix-dark-pink
       "Phoenix Dark Pink color theme")

     (custom-theme-set-faces 'phoenix-dark-pink ,@body)))

(define-phoenix-dark-pink-theme
  `(default                                   ((t (:inherit nil
                                                   :stipple nil
                                                   :background ,bg
                                                   :foreground ,fg
                                                   :inverse-video nil
                                                   :box nil
                                                   :strike-through nil
                                                   :overline nil
                                                   :underline nil
                                                   :slant normal
                                                   :weight normal
                                                   :width normal))))

  ;; Another special face is the cursor face. On graphical displays, the
  ;; background color of this face is used to draw the text cursor. None of the
  ;; other attributes of this face have any effect
  ;;
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Faces.html#Faces
  `(cursor                                    ((t (:background ,fg))))

  `(fixed-pitch                               ((t (:weight normal
                                                   :underline nil))))

  `(variable-pitch                            ((t (:weight normal
                                                   :underline nil
                                                   :family "Sans Serif"))))

  `(escape-glyph                              ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,p3))))

  `(minibuffer-prompt                         ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,p3))))

  `(highlight                                 ((t (:weight normal
                                                   :underline nil
                                                   :background ,dp1))))

  `(region                                    ((t (:weight normal
                                                   :underline nil
                                                   :background ,dp2))))

  `(shadow                                    ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,offwhite4))))

  `(secondary-selection                       ((t (:weight normal
                                                   :underline nil
                                                   :background ,b3))))

  `(trailing-whitespace                       ((t (:weight normal
                                                   :underline nil
                                                   :background ,p3))))

  `(custom-state                              ((t (:foreground ,alarmed))))

  `(font-lock-builtin-face                    ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,offwhite3))))

  `(font-lock-comment-face                    ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,silverfox
                                                   :slant italic))))

  `(font-lock-comment-delimiter-face          ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,sadsilverfox
                                                   :inherit font-lock-comment-face))))

  `(font-lock-doc-face                        ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,damned
                                                   :inherit (font-lock-string-face)))))

  `(font-lock-string-face                     ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,offpink3))))

  `(font-lock-constant-face                   ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,p8))))

  `(font-lock-function-name-face              ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,offpink1))))

  `(font-lock-keyword-face                    ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,p3))))

  `(font-lock-negation-char-face              ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,fg))))

  `(font-lock-preprocessor-face               ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,p3
                                                   :inherit (font-lock-builtin-face)))))

  `(font-lock-regexp-grouping-backslash       ((t (:weight normal
                                                   :underline nil
                                                   :inherit (bold)))))

  `(font-lock-regexp-grouping-construct       ((t (:weight normal
                                                   :underline nil
                                                   :inherit (bold)))))

  `(font-lock-type-face                       ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,offwhite2))))

  `(font-lock-variable-name-face              ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,offwhite1))))

  `(font-lock-warning-face                    ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,damned
                                                   :inherit (error)))))

  `(compilation-info                          ((t (:weight normal
                                                   :foreground ,offpink1))))

  `(compilation-mode-line-exit                ((t (:weight normal
                                                   :foreground ,offpink1))))

  `(compilation-mode-line-fail                ((t (:weight normal
                                                   :foreground ,p2))))

  `(compilation-mode-line-run                 ((t (:weight normal
                                                   :foreground ,silverfox))))

  `(warning                                   ((t (:weight normal
                                                   :foreground ,offpink1))))

  `(link                                      ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,offpink1))))

  `(link-visited                              ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,offpink2
                                                   :inherit (link)))))

  `(button                                    ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,offpink1))))

  `(fringe                                    ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,sadsilverfox
                                                   :background ,b1))))

  `(header-line                               ((t (:weight normal
                                                   :underline nil
                                                   :inherit (mode-line)))))

  `(tooltip                                   ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,p3
                                                   :background ,b2
                                                   :inherit (variable-pitch)))))

  ;; Mode line

  `(mode-line                                 ((t (:weight normal
                                                   :underline nil
                                                   :box (:style released-button :color ,dp3 :line-width 2)
                                                   :foreground ,p3
                                                   :background ,b2))))

  `(mode-line-inactive                        ((t (:weight normal
                                                   :underline nil
                                                   :box (:style released-button :color ,dp1 :line-width 2)
                                                   :foreground ,p7
                                                   :background ,b1
                                                   :inherit (mode-line)))))

  `(mode-line-buffer-id                       ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,offpink1))))

  `(mode-line-emphasis                        ((t (:weight normal
                                                   :underline nil))))

  `(mode-line-highlight                       ((t (:weight normal
                                                   :underline nil
                                                   :box nil))))

  `(cider-error-highlight-face                ((t (:underline (:style wave :color ,alarmed)))))

  `(cider-warning-highlight-face              ((t (:underline (:style wave :color ,warned)))))

  `(cider-repl-input-face                     ((t (:weight normal))))

  `(isearch                                   ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,b2
                                                   :background ,p3))))

  `(isearch-fail                              ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,p3
                                                   :background ,alarmed))))

  `(lazy-highlight                            ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,p2
                                                   :background ,b2))))

  `(highlight-symbol-face                     ((t (:underline t
                                                   :background ,b2))))

  `(grep-context-face                         ((t (:foreground ,fg))))

  `(grep-error-face                           ((t (:underline t
                                                   :foreground ,p2))))

  `(grep-hit-face                             ((t (:foreground ,p2))))

  `(grep-match-face                           ((t (:foreground ,p2))))

  `(match                                     ((t (:weight normal
                                                   :underline nil
                                                   :foreground ,p2
                                                   :background ,b2))))

  `(next-error                                ((t (:weight normal
                                                   :underline nil
                                                   :inherit (region)))))

  `(query-replace                             ((t (:weight normal
                                                   :underline nil
                                                   :inherit (isearch)))))

  `(ido-first-match                           ((t (:weight bold
                                                   :foreground ,p2))))

  `(ido-only-match                            ((t (:weight bold
                                                   :foreground ,p2))))

  `(ido-subdir                                ((t (:foreground ,p7))))

  `(flx-highlight-face                        ((t (:weight bold
                                                   :foreground ,offpink1))))

  `(linum                                     ((t (:foreground ,silverfox))))

  `(ac-candidate-face                         ((t (:background ,b2
                                                   :foreground ,p7))))

  `(ac-candidate-mouse-face                   ((t (:background ,b3
                                                   :foreground ,p3))))

  `(ac-selection-face                         ((t (:background ,b3
                                                   :foreground ,p3))))

  `(ac-yasnippet-selection-face               ((t (:background ,b3
                                                   :foreground ,p3))))

  `(ac-yasnippet-candidate-face               ((t (:background ,b2
                                                   :foreground ,p7))))

  `(popup-tip-face                            ((t (:background ,b2
                                                   :foreground ,p7))))

  `(popup-scroll-bar-foreground-face          ((t (:background ,b3))))
  `(popup-scroll-bar-background-face          ((t (:background ,bg))))

  `(company-tooltip                           ((t (:background ,b2
                                                   :foreground ,p7))))

  `(company-tooltip-selection                 ((t (:background ,b3
                                                   :foreground ,plegit))))

  `(company-tooltip-mouse                     ((t (:inherit (company-tooltip-selection)))))

  `(company-tooltip-common                    ((t (:background ,b3
                                                   :foreground ,p3))))

  `(company-tooltip-common-selection          ((t (:background ,b3
                                                   :foreground ,plegit))))

  `(company-tooltip-annotation                ((t (:foreground ,offpink1
                                                   :inherit (company-tooltip)))))

  `(company-scrollbar-fg                      ((t (:inherit (company-tooltip)))))

  `(company-scrollbar-bg                      ((t (:inherit (company-tooltip)))))

  `(company-preview                           ((t (:inherit (company-tooltip-selection)))))

  `(company-preview-common                    ((t (:inherit (company-tooltip-common-selection)))))

  `(company-preview-search                    ((t (:foreground ,offpink1
                                                   :inherit (company-preview)))))

  `(company-echo                              ((t (:foreground ,p3))))

  `(company-echo-common                       ((t (:foreground ,offpink1))))

  `(speedbar-button-face                      ((t (:foreground ,p3))))

  `(speedbar-directory-face                   ((t (:foreground ,offwhite1))))

  `(speedbar-file-face                        ((t (:foreground ,fg))))

  `(speedbar-highlight-face                   ((t (:foreground ,b1
                                                   :background ,p8))))

  `(speedbar-selected-face                    ((t (:foreground ,offpink1))))

  `(speedbar-tag-face                         ((t (:foreground ,p7))))

  `(eval-sexp-fu-flash                        ((t (:background ,bg
                                                   :foreground ,offpink1))))

  `(nrepl-eval-sexp-fu-flash                  ((t (:background ,bg
                                                   :foreground ,offpink1))))

  ;; Magit

  `(magit-header                              ((t (:foreground ,p2
                                                   :background ,b2
                                                   :box (:line-width 1 :color ,b3)))))

  `(magit-header-line                         ((t (:foreground ,p2
                                                   :background ,b2
                                                   :box (:line-width 1 :color ,b3)))))

  `(magit-log-sha1                            ((t (:foreground ,p2
                                                   :background ,b2))))

  `(magit-section-title                       ((t (:foreground ,p2
                                                   :background ,bg))))

  `(magit-section-heading                     ((t (:foreground ,p0
                                                   :background ,bg))))

  `(magit-branch                              ((t (:foreground ,p3))))

  `(magit-branch-remote                       ((t (:foreground ,p7))))

  `(magit-branch-local                        ((t (:foreground ,p1))))

  `(magit-item-highlight                      ((t (:inherit (highlight-parentheses)))))

  `(magit-diff-add                            ((t (:foreground ,p3
                                                   :background ,b3))))

  `(magit-diff-del                            ((t (:foreground ,p9
                                                   :background ,b1))))

  `(magit-diff-none                           ((t (:background ,bg))))

  `(magit-diff-hunk-header                    ((t (:background ,b2))))

  `(magit-diff-file-header                    ((t (:background ,b3))))

  `(magit-diff-removed-highlight              ((t (:background ,sadsilverfox))))

  `(magit-diff-added-highlight                ((t (:background ,dp3))))

  `(magit-diff-removed                        ((t (:background ,b1))))

  `(magit-diff-added                          ((t (:background ,dp2))))

  `(diff-refine-added                         ((t (:background ,p4
                                                   :foreground ,dp3))))

  `(diff-refine-removed                       ((t (:background ,offwhite4
                                                   :foreground ,b1))))

  `(magit-log-author                          ((t (:foreground ,offpink1))))

  `(magit-log-head-label-remote               ((t (:foreground ,offpink1
                                                   :box t))))

  `(magit-log-head-label-local                ((t (:foreground ,p1
                                                   :box t))))

  `(window-number-face                        ((t (:background ,b2
                                                   :foreground ,offpink1))))

  ;; Git Gutter

  `(git-gutter:separator                      ((t (:foreground ,bg
                                                   :background ,bg))))

  `(git-gutter:modified                       ((t (:foreground ,p3
                                                   :background ,bg))))

  `(git-gutter:added                          ((t (:foreground ,offpink1
                                                   :background ,bg))))

  `(git-gutter:deleted                        ((t (:foreground ,damned
                                                   :background ,bg))))

  `(git-gutter:unchanged                      ((t (:foreground ,bg
                                                   :background ,bg))))

  ;; Parentheses

  `(hl-paren-face                             ((t (:inherit highlight))))

  `(show-paren-match                          ((t (:inherit highlight
                                                   :foreground ,p0
                                                   :background ,p7))))

  ;; ERB (Ruby templates)

  `(erb-face                                  ((t (:foreground ,fg
                                                   :background ,bg))))

  `(erb-exec-face                             ((t (:inherit erb-face))))

  `(erb-out-face                              ((t (:inherit erb-face))))

  `(erb-delim-face                            ((t (:inherit erb-face
                                                   :foreground ,p1
                                                   :background ,bg))))

  `(erb-exec-delim-face                       ((t (:inherit erb-delim-face))))

  `(erb-out-delim-face                        ((t (:inherit erb-delim-face
                                                   :foreground ,p1
                                                   :background ,bg))))

  `(erb-comment-face                          ((t (:inherit erb-face
                                                   :foreground ,p13
                                                   :background ,bg))))

  `(erb-comment-delim-face                    ((t (:inherit erb-face
                                                   :foreground ,sadsilverfox
                                                   :background ,bg))))

  ;; Rainbow delimiters

  `(rainbow-delimiters-depth-9-face           ((t (:foreground ,p1))))
  `(rainbow-delimiters-depth-8-face           ((t (:foreground ,p2))))
  `(rainbow-delimiters-depth-7-face           ((t (:foreground ,p3))))
  `(rainbow-delimiters-depth-6-face           ((t (:foreground ,p4))))
  `(rainbow-delimiters-depth-5-face           ((t (:foreground ,p5))))
  `(rainbow-delimiters-depth-4-face           ((t (:foreground ,p6))))
  `(rainbow-delimiters-depth-3-face           ((t (:foreground ,p7))))
  `(rainbow-delimiters-depth-2-face           ((t (:foreground ,p8))))
  `(rainbow-delimiters-depth-1-face           ((t (:foreground ,p9))))

  ;; Javascript

  `(js2-warning                               ((t (:foreground ,p2))))
  `(js2-error                                 ((t (:foreground ,p1))))
  `(js2-jsdoc-tag                             ((t (:foreground ,sadsilverfox))))
  `(js2-jsdoc-type                            ((t (:foreground ,silverfox))))
  `(js2-jsdoc-value                           ((t (:foreground ,silverfox))))
  `(js2-function-param                        ((t (:foreground ,p8))))
  `(js2-external-variable                     ((t (:foreground ,p0))))

  ;; ERC (IRC)

  `(erc-action-face                           ((t (:inherit erc-default-face))))

  `(erc-bold-face                             ((t (:weight bold))))

  `(erc-current-nick-face                     ((t (:foreground ,fg
                                                   :weight bold))))

  `(erc-dangerous-host-face                   ((t (:inherit font-lock-warning))))

  `(erc-default-face                          ((t (:foreground ,fg))))

  `(erc-direct-msg-face                       ((t (:inherit erc-default))))

  `(erc-error-face                            ((t (:inherit font-lock-warning))))

  `(erc-fool-face                             ((t (:inherit erc-default))))

  `(erc-highlight-face                        ((t (:inherit hover-highlight))))

  `(erc-input-face                            ((t (:foreground ,p3))))

  `(erc-keyword-face                          ((t (:foreground ,p2
                                                   :weight bold))))

  `(erc-nick-default-face                     ((t (:foreground ,p2
                                                   :weight bold))))

  `(erc-my-nick-face                          ((t (:foreground ,offwhite1
                                                   :weight bold))))

  `(erc-nick-msg-face                         ((t (:inherit erc-default))))

  `(erc-notice-face                           ((t (:foreground ,p7
                                                   :background ,bg))))

  `(erc-pal-face                              ((t (:foreground ,p1
                                                   :weight bold))))

  `(erc-prompt-face                           ((t (:foreground ,p3
                                                   :background ,bg
                                                   :weight bold))))

  `(erc-timestamp-face                        ((t (:foreground ,silverfox))))

  `(erc-underline-face                        ((t (:underline t))))

  ;; CIRCE (IRC)

  `(circe-prompt-face                         ((t (:weight bold
                                                   :foreground ,p3))))

  `(circe-server-face                         ((t (:foreground ,p7))))

  `(circe-highlight-nick-face                 ((t (:weight bold
                                                   :inherit hover-highlight))))

  `(circe-my-message-face                     ((t (:foreground ,offpink3))))

  `(circe-originator-face                     ((t (:foreground ,p2))))

  `(circe-topic-diff-new-face                 ((t (:inherit git-gutter:added))))

  `(circe-topic-diff-removed-face             ((t (:inherit git-gutter:deleted))))

  `(circe-fool-face                           ((t (:foreground ,sadsilverfox))))

  `(lui-button-face                           ((t (:inherit link))))

  `(lui-time-stamp-face                       ((t (:inherit erc-timestamp-face))))

  `(lui-highlight-face                        ((t (:inherit hover-highlight))))

  ;; w3m

  `(w3m-anchor                                ((t (:inherit link))))

  `(w3m-arrived-anchor                        ((t (:foreground ,p0))))

  `(w3m-form                                  ((t (:foreground ,p7
                                                   :underline t))))

  `(w3m-header-line-location-title            ((t (:foreground ,offwhite1
                                                   :underline t
                                                   :weight bold))))

  `(w3m-history-current-url                   ((t (:inherit match))))

  `(w3m-lnum                                  ((t (:foreground ,silverfox))))

  `(w3m-lnum-match                            ((t (:background ,p7
                                                   :foreground ,bg))))

  `(w3m-lnum-minibuffer-prompt                ((t (:foreground ,p1))))

  `(highlight-indentation-face                ((t (:inherit highlight))))

  `(highlight-indentation-current-column-face ((t (:inherit highlight))))

  ;; org-mode

  `(org-level-1                               ((t (:foreground ,p1))))
  `(org-level-2                               ((t (:foreground ,p2))))
  `(org-level-3                               ((t (:foreground ,p3))))
  `(org-level-4                               ((t (:foreground ,p4))))
  `(org-level-5                               ((t (:foreground ,p5))))
  `(org-level-6                               ((t (:foreground ,p6))))
  `(org-level-7                               ((t (:foreground ,p7))))
  `(org-level-8                               ((t (:foreground ,p8))))
  `(org-level-9                               ((t (:foreground ,p9))))
  `(org-meta-line                             ((t (:foreground ,dp3))))
  `(org-table                                 ((t (:foreground ,p5))))
  `(org-document-info-keyword                 ((t (:foreground ,p7))))
  `(org-document-title                        ((t (:foreground ,p2))))
  `(org-date                                  ((t (:foreground ,p7))))

  ;; ivy-mode

  `(ivy-current-match                         ((t (:background ,p2
                                                   :foreground ,bg))))

  `(ivy-minibuffer-match-face-1               ((t (:background ,p10
                                                   :foreground ,p0))))

  `(ivy-minibuffer-match-face-2               ((t (:background ,p10
                                                   :foreground ,p0))))

  `(ivy-minibuffer-match-face-3               ((t (:background ,p10
                                                   :foreground ,p0))))

  `(ivy-minibuffer-match-face-4               ((t (:background ,p10
                                                   :foreground ,p0))))

  `(ivy-confirm-face                          ((t (:foreground ,p0))))

  `(ivy-match-required-face                   ((t (:foreground ,p0))))

  `(ivy-subdir                                ((t (:foreground ,fg))))

  `(ivy-remote                                ((t (:foreground ,plegit))))

  `(ivy-virtual                               ((t (:foreground ,plegit))))

  ;; mu4e -- email

  `(mu4e-unread-face                          ((t (:foreground ,offpink1))))
  ;; `(mu4e-moved-face ((t ())))
  ;; `(mu4e-trashed-face ((t ())))
  ;; `(mu4e-draft-face ((t ())))
  `(mu4e-flagged-face                         ((t (:foreground ,b1))))

  `(mu4e-replied-face                         ((t (:foreground ,fg))))

  `(mu4e-forwarded-face                       ((t (:foreground ,fg))))

  ;; `(mu4e-header-face ((t ())))
  ;; `(mu4e-header-title-face ((t ())))

  `(mu4e-header-highlight-face                ((t (:foreground ,offwhite1
                                                   :background ,dp2))))

  ;; `(mu4e-header-marks-face ((t ())))

  `(mu4e-header-key-face                      ((t (:foreground ,p6))))

  `(mu4e-header-value-face                    ((t (:foreground ,offpink2))))

  `(mu4e-special-header-value-face            ((t (:foreground ,offpink1))))

  `(mu4e-link-face                            ((t (:foreground ,p0))))

  ;; `(mu4e-contact-face ((t ())))
  ;; `(mu4e-highlight-face ((t ())))
  ;; `(mu4e-title-face ((t ())))
  ;; `(mu4e-context-face ((t ())))
  ;; `(mu4e-modeline-face ((t ())))
  ;; `(mu4e-view-body-face ((t ())))
  ;; `(mu4e-footer-face ((t ())))

  `(mu4e-url-number-face                      ((t (:foreground ,p6))))
  `(mu4e-attach-number-face                   ((t (:foreground ,p6))))

  `(mu4e-cited-1-face                         ((t (:foreground ,p7))))
  `(mu4e-cited-2-face                         ((t (:foreground ,p12))))
  `(mu4e-cited-3-face                         ((t (:foreground ,silverfox))))
  `(mu4e-cited-4-face                         ((t (:foreground ,p7))))
  `(mu4e-cited-5-face                         ((t (:foreground ,p12))))
  `(mu4e-cited-6-face                         ((t (:foreground ,silverfox))))
  `(mu4e-cited-7-face                         ((t (:foreground ,p7))))

  ;; `(mu4e-system-face ((t (:foreground , offwhite3))))
  ;; `(mu4e-ok-face ((t (:foreground))))
  ;; `(mu4e-warning-face ((t (:foreground ,offwhite3))))

  `(mu4e-compose-separator-face               ((t (:foreground ,silverfox))))

  ;; `(mu4e-compose-header-face ((t (:foreground ,offwhite3))))
  ;; `(mu4e-region-code ((t (:foreground ,offwhite3))))

  `(message-mml                               ((t (:foreground ,p0))))

  `(indent-guide-face                         ((t (:foreground ,dp3)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'phoenix-dark-pink)

;; Local Variables:
;; no-byte-compile: t
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:

;;; phoenix-dark-pink-theme.el ends here
