;;; madhat2r-theme.el --- dark color theme that is easy on the eyes

;; Copyright (C) 2015-2016 Micah Duke

;; Author: Micah Duke
;; URL: <https://github.com/madhat2r/madhat2r-theme>
;; Package-Version: 20170203.30
;;
;; Version: 0.1
;; Keywords: color, theme
;; Package-Requires: ((emacs "24"))

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

;;; Credits:

;; Thanks to Nasser Alshammari of the Spacemacs Theme which was used as the skeleton:
;; https://github.com/nashamri/spacemacs-theme

;;; Code:

(deftheme madhat2r "Madhat2r theme")
;; (defmacro madhat2r-dyn-let (varlist fn setfaces setvars)
;;   (list 'let (append varlist (funcall fn)) setfaces setvars))

(defgroup madhat2r-theme nil
  "Madhat2r-theme options."
  :group 'faces)

(defcustom madhat2r-theme-org-agenda-height t
  "Use varying text heights for org agenda."
  :type 'boolean
  :group 'madhat2r-theme)

(defcustom madhat2r-theme-org-height nil
  "Use varying text heights for org headings."
  :type 'boolean
  :group 'madhat2r-theme)


(defcustom madhat2r-theme-custom-colors nil
  "Specify a list of custom colors."
  :type 'alist
  :group 'madhat2r-theme)


(defun madhat2r-custom-colors-override ()
  (mapcar (lambda (x) (list (car x) (cdr x)))
          madhat2r-theme-custom-colors))

(let*
    (;; Color Classes
     (madhat2r-class '((class color)
                       (min-colors 257)))
     (madhat2r-term-class '((class color)
                           (min-colors 89)))
     ;; 256 up colors
     (act1 "#373d3f")
     (act2 "#131516")
     (base "#c1c7c9")
     (base-dim "#e0e0e0")
     (bg1 "#131516")
     (bg2 "#1b1e1f")
     (bg3 "#232628")
     (bg4 "#6f7c80")
     (complement-bg "#1b1e1f")
     (border "#0d3c55")
     (cblk "#c1c7c9")
     (cblk-bg "#131516")
     (cblk-ln "#a7afb2")
     (cblk-ln-bg "#232628")
     (cursor "#e3dedd")
     (const "#f16c20")
     (comment "#555f61")
     (comment-bg "#373d3f")
     (comp "#ebc844")
     (err "#c02e1d")
     (func "#ecaa38")
     (head1 "#ebc844")
     (head2 "#0f5b78")
     (head3 "#5ca793")
     (head4 "#1395ba")
     (highlight "#0d3c55")
     (keyword "#1395ba")
     (lnum "#44505c")
     (mat "#f3df91")
     (meta "#555f61")
     (str "#a2b86c")
     (suc "#9bda34")
     (ttip "#c1c7c9")
     (ttip-sl "#1395ba")
     (ttip-bg "#0d3c55")
     (type "#ef8b2c")
     (var "#5ca793")
     (war "#d94e1f")
     (link "#f2f3f4")
     (over-bg "#8c979a")
     ;; colors
     (aqua "#10A585")
     (aqua-bg "#293235")
     (green "#40805b")
     (green-bg "#293235")
     (green-bg-s "#2a5748")
     (cyan "#22aae1")
     (red "#d93334")
     (red-bg "#3c2a2c")
     (red-bg-s "#59355a")
     (blue "#4f97d7")
     (blue-bg "#293239")
     (magenta "#c24160")
     (yellow "#b1951d")
     (yellow-bg "#32322c")
     ;; term colors
     (act1-term "brightblack")
     (act2-term "black")
     (base-term "#afd7d7")
     (base-dim-term "#d7d7d7")
     (bg1-term "#121212")
     (bg2-term "black")
     (bg3-term "black")
     (bg4-term "#5f8787")
     (complement-bg-term "#121212")
     (border-term "brightmagenta")
     (cblk-term "#afd7d7")
     (cblk-bg-term "#121212")
     (cblk-ln-term "#afafaf")
     (cblk-ln-bg-term "#232628")
     (cursor-term "#d0d0d0")
     (const-term "#ff5f00")
     (comment-term "#5f5f5f")
     (comment-bg-term "#080808")
     (comp-term "#ffd75f")
     (err-term "#af0000")
     (func-term "#ffff00")
     (head1-term "#ffd75f")
     (head2-term "#005f87")
     (head3-term "#5faf87")
     (head4-term "#0087af")
     (highlight-term "magenta")
     (keyword-term "#0087af")
     (lnum-term "#5f5f5f")
     (mat-term "#ffd787")
     (meta-term "#5f5f5f")
     (str-term "green")
     (suc-term "#afd75f")
     (ttip-term "#afd7d7")
     (ttip-sl-term "#0087af")
     (ttip-bg-term "magenta")
     (type-term "#ff8700")
     (var-term "#5faf87")
     (war-term "#d75f00")
     (link-term "#eeeeee")
     (over-bg-term "#878787")
     ;; colors
     (aqua-term "#10A585")
     (aqua-bg-term "#262626")
     (green-term "#40805b")
     (green-bg-term "#262626")
     (green-bg-s-term "#2a5748")
     (cyan-term "#22aae1")
     (red-term "#d93334")
     (red-bg-term "#262626")
     (red-bg-s-term "#59355a")
     (blue-term "#268bd2")
     (blue-bg-term "#262626")
     (magenta-term "#c24160")
     (yellow-term "#875f00")
     (yellow-bg-term "#262626"))

  (madhat2r-custom-colors-override)
  (custom-theme-set-faces
   'madhat2r
   ;;;;; basics

   `(cursor
     ((,madhat2r-class (:background ,cursor))
      (,madhat2r-term-class (:background ,cursor-term))))

   `(custom-button
     ((,madhat2r-class :background ,bg2 :foreground ,base :box (:line-width 2 :style released-button))
      (,madhat2r-term-class :background ,bg2 :foreground ,base-term :box (:line-width 2 :style released-button))))

   `(default
     ((,madhat2r-class (:background ,bg1 :foreground ,base))
      (,madhat2r-term-class (:background ,bg1 :foreground ,base-term))))

   `(default-italic
     ((,madhat2r-class (:italic t))
      (,madhat2r-term-class (:italic t))))

   `(error
     ((,madhat2r-class (:foreground ,err))
      (,madhat2r-term-class (:foreground ,err-term))))

   `(eval-sexp-fu-flash
     ((,madhat2r-class (:background ,suc :foreground ,bg1))
      (,madhat2r-term-class (:background ,suc-term :foreground ,bg1))))

   `(eval-sexp-fu-flash-error
     ((,madhat2r-class (:background ,err :foreground ,bg1))
      (,madhat2r-term-class (:background ,err-term :foreground ,bg1))))

   `(font-lock-builtin-face
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(font-lock-comment-face
     ((,madhat2r-class (:foreground ,comment ))
      (,madhat2r-term-class (:foreground ,comment-term ))))

   `(font-lock-constant-face
     ((,madhat2r-class (:foreground ,const))
      (,madhat2r-term-class (:foreground ,const-term))))

   `(font-lock-doc-face
     ((,madhat2r-class (:foreground ,comment))
      (,madhat2r-term-class (:foreground ,comment-term))))

   `(font-lock-function-name-face
     ((,madhat2r-class (:foreground ,func :inherit bold))
      (,madhat2r-term-class (:foreground ,func-term :inherit bold))))

   `(font-lock-keyword-face
     ((,madhat2r-class (:inherit bold :foreground ,keyword))
      (,madhat2r-term-class (:inherit bold :foreground ,keyword-term))))

   `(font-lock-negation-char-face
     ((,madhat2r-class (:foreground ,const))
      (,madhat2r-term-class (:foreground ,const-term))))

   `(font-lock-preprocessor-face
     ((,madhat2r-class (:foreground ,func))
      (,madhat2r-term-class (:foreground ,func-term))))

   `(font-lock-reference-face
     ((,madhat2r-class (:foreground ,const))
      (,madhat2r-term-class (:foreground ,const-term))))

   `(font-lock-string-face
     ((,madhat2r-class (:foreground ,str))
      (,madhat2r-term-class (:foreground ,str-term))))

   `(font-lock-type-face
     ((,madhat2r-class (:foreground ,type :inherit bold))
      (,madhat2r-term-class (:foreground ,type-term :inherit bold))))

   `(font-lock-variable-name-face
     ((,madhat2r-class (:foreground ,var))
      (,madhat2r-term-class (:foreground ,var-term))))

   `(font-lock-warning-face
     ((,madhat2r-class (:foreground ,war :background ,bg1))
      (,madhat2r-term-class (:foreground ,war-term :background ,bg1))))

   `(fringe
     ((,madhat2r-class (:background ,bg1 :foreground ,base))
      (,madhat2r-term-class (:background ,bg1 :foreground ,base-term))))

   `(header-line
     ((,madhat2r-class :background ,highlight)
      (,madhat2r-term-class :background ,highlight-term)))

   `(highlight
     ((,madhat2r-class (:foreground ,base :background ,highlight))
      (,madhat2r-term-class (:foreground ,base-term :background ,highlight))))

   `(hl-line
     ((,madhat2r-class (:background ,bg2))
      (,madhat2r-term-class (:background ,bg2))))

   `(isearch
     ((,madhat2r-class (:foreground ,bg1 :background ,mat))
      (,madhat2r-term-class (:foreground ,bg1 :background ,mat-term))))

   `(lazy-highlight
     ((,madhat2r-class (:underline t :foreground ,mat :weight bold))
      (,madhat2r-term-class (:underline t :foreground ,mat-term :weight bold))))

   `(link
     ((,madhat2r-class (:foreground ,link :underline t))
      (,madhat2r-term-class (:foreground ,link-term :underline t))))

   `(link-visited
     ((,madhat2r-class (:foreground ,comp :underline t))
      (,madhat2r-term-class (:foreground ,comp-term :underline t))))

   `(match
     ((,madhat2r-class (:background ,highlight :foreground ,mat))
      (,madhat2r-term-class (:background ,highlight-term :foreground ,mat))))

   `(minibuffer-prompt
     ((,madhat2r-class (:inherit bold :foreground ,keyword))
      (,madhat2r-term-class (:inherit bold :foreground ,keyword-term))))

   `(page-break-lines
     ((,madhat2r-class (:foreground ,act2))
      (,madhat2r-term-class (:foreground ,act2))))

   `(region
     ((,madhat2r-class (:background ,highlight))
      (,madhat2r-term-class (:background ,highlight-term))))

   `(secondary-selection
     ((,madhat2r-class (:background ,bg3))
      (,madhat2r-term-class (:background ,bg3))))

   `(success
     ((,madhat2r-class (:foreground ,suc))
      (,madhat2r-term-class (:foreground ,suc-term))))

   `(tooltip
     ((,madhat2r-class (:background ,bg3 :foreground ,base :bold nil :italic nil :underline nil))
      (,madhat2r-term-class (:background ,bg3-term :foreground ,base-term :bold nil :italic nil :underline nil))))

   `(vertical-border
     ((,madhat2r-class (:foreground ,bg4))
      (,madhat2r-term-class (:foreground ,bg4))))

   `(warning
     ((,madhat2r-class (:foreground ,war))
      (,madhat2r-term-class (:foreground ,war-term))))

   ;;;;; ahs

   `(ahs-face
     ((,madhat2r-class (:underline t :foreground ,mat))
      (,madhat2r-term-class (:underline t :foreground ,mat-term))))

   `(ahs-plugin-whole-buffer-face
     ((,madhat2r-class (:background ,mat :foreground ,bg1))
      (,madhat2r-term-class (:background ,mat-term :foreground ,bg1))))

   ;;;;; Message-mode

   `(notmuch-message-summary-face
     ((,madhat2r-class (:background ,bg3))
      (,madhat2r-term-class (:background ,bg3))))

   `(notmuch-wash-cited-text
     ((,madhat2r-class (:foreground ,comment))
      (,madhat2r-term-class (:foreground ,comment-term))))

   `(notmuch-wash-toggle-button
     ((,madhat2r-class (:background ,bg3))
      (,madhat2r-term-class (:background ,bg3))))

   `(message-cited-text
     ((,madhat2r-class (:foreground ,comment))
      (,madhat2r-term-class (:foreground ,comment-term))))

   `(message-header-other
     ((,madhat2r-class (:foreground ,base :background nil :weight normal))
      (,madhat2r-term-class (:foreground ,base-term :background nil :weight normal))))

   `(message-header-subject
     ((,madhat2r-class (:inherit message-header-other :weight bold :foreground ,head3))
      (,madhat2r-term-class (:inherit message-header-other :weight bold :foreground ,head3))))

   `(message-header-to
     ((,madhat2r-class (:inherit message-header-other :weight bold :foreground ,const))
      (,madhat2r-term-class (:inherit message-header-other :weight bold :foreground ,const-term))))

   `(message-header-cc
     ((,madhat2r-class (:inherit message-header-to ))
      (,madhat2r-term-class (:inherit message-header-to ))))

   `(message-header-name
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(message-header-newsgroups
     ((,madhat2r-class (:foreground ,cyan  :slant normal))
      (,madhat2r-term-class (:foreground ,cyan-term  :slant normal))))

   `(message-separator
     ((,madhat2r-class (:foreground ,comment))
      (,madhat2r-term-class (:foreground ,comment-term))))

   `(message-mml
     ((,madhat2r-class (:foreground ,bg4 :background ,bg3))
      (,madhat2r-term-class (:foreground ,bg4 :background ,bg3))))

   ;;;;; anzu-mode

   `(anzu-mode-line
     ((,madhat2r-class (:foreground ,yellow :inherit bold))
      (,madhat2r-term-class (:foreground ,yellow-term :inherit bold))))

   ;;;;; auto-complete

   `(ac-completion-face
     ((,madhat2r-class (:background ,ttip-bg :foreground ,ttip))
      (,madhat2r-term-class (:background ,ttip-bg :foreground ,ttip-term))))

   ;;;;; avy

   `(avy-lead-face
     ((,madhat2r-class (:background "#43339e" :foreground ,comp))
      (,madhat2r-term-class (:background "#43339e" :foreground ,comp-term))))

   `(avy-lead-face-0
     ((,madhat2r-class (:background "#43339e" :foreground ,suc))
      (,madhat2r-term-class (:background "#43339e" :foreground ,suc-term))))

   `(avy-lead-face-1
     ((,madhat2r-class (:background "#43339e" :foreground ,comp))
      (,madhat2r-term-class (:background "#43339e" :foreground ,comp-term))))

   `(avy-lead-face-2
     ((,madhat2r-class (:background "#43339e" :foreground ,suc))
      (,madhat2r-term-class (:background "#43339e" :foreground ,suc-term))))

   ;;;;; cider

   `(cider-enlightened
     ((,madhat2r-class (:background nil :box (:color ,yellow :line-width -1 :style nil) :foreground ,yellow))
      (,madhat2r-term-class (:background nil :box (:color ,yellow-term :line-width -1 :style nil) :foreground ,yellow))))

   `(cider-enlightened-local
     ((,madhat2r-class (:foreground ,yellow))
      (,madhat2r-term-class (:foreground ,yellow-term))))

   `(cider-instrumented-face
     ((,madhat2r-class (:background nil :box (:color ,red :line-width -1 :style nil) :foreground ,red))
      (,madhat2r-term-class (:background nil :box (:color ,red-term :line-width -1 :style nil) :foreground ,red))))

   `(cider-result-overlay-face
     ((,madhat2r-class (:background nil :box (:color ,blue :line-width -1 :style nil) :foreground ,blue))
      (,madhat2r-term-class (:background nil :box (:color ,blue-term :line-width -1 :style nil) :foreground ,blue))))

   `(cider-test-error-face
     ((,madhat2r-class (:background ,war :foreground ,bg1))
      (,madhat2r-term-class (:background ,war-term :foreground ,bg1))))

   `(cider-test-failure-face
     ((,madhat2r-class (:background ,err :foreground ,bg1))
      (,madhat2r-term-class (:background ,err-term :foreground ,bg1))))

   `(cider-test-success-face
     ((,madhat2r-class (:background ,suc :foreground ,bg1))
      (,madhat2r-term-class (:background ,suc-term :foreground ,bg1))))


   `(cider-traced-face
     ((,madhat2r-class :box (:color ,cyan :line-width -1 :style nil))
      (,madhat2r-term-class :box (:color ,cyan-term :line-width -1 :style nil))))

   ;;;;; company

   `(company-echo-common
     ((,madhat2r-class (:background ,base :foreground ,bg1))
      (,madhat2r-term-class (:background ,base-term :foreground ,bg1))))

   `(company-preview
     ((,madhat2r-class (:background ,bg3 :foreground ,base))
      (,madhat2r-term-class (:background ,bg3-term :foreground ,base-term))))

   `(company-preview-common
     ((,madhat2r-class (:background ,bg3 :foreground ,comp))
      (,madhat2r-term-class (:background ,bg3-term :foreground ,comp-term))))

   `(company-preview-search
     ((,madhat2r-class (:foreground ,const))
      (,madhat2r-term-class (:foreground ,const-term))))

   `(company-scrollbar-bg
     ((,madhat2r-class (:background ,bg2))
      (,madhat2r-term-class (:background ,bg2-term))))

   `(company-scrollbar-fg
     ((,madhat2r-class (:background ,act2))
      (,madhat2r-term-class (:background ,act2-term))))

   `(company-template-field
     ((,madhat2r-class (:inherit region))
      (,madhat2r-term-class (:inherit region))))

   `(company-tooltip
     ((,madhat2r-class (:background ,bg3 :foreground ,base))
      (,madhat2r-term-class (:background ,bg3-term :foreground ,base-term))))

   `(company-tooltip-annotation
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(company-tooltip-annotation-selection
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(company-tooltip-common
     ((,madhat2r-class (:background ,bg3 :foreground ,comp))
      (,madhat2r-term-class (:background ,bg3-term :foreground ,comp-term))))

   `(company-tooltip-common-selection
     ((,madhat2r-class (:foreground ,comp))
      (,madhat2r-term-class (:foreground ,comp-term))))

   `(company-tooltip-mouse
     ((,madhat2r-class (:inherit highlight))
      (,madhat2r-term-class (:inherit highlight-term))))

   `(company-tooltip-search
     ((,madhat2r-class (:foreground ,const))
      (,madhat2r-term-class (:foreground ,const-term))))

   `(company-tooltip-search-selection
     ((,madhat2r-class (:foreground ,const))
      (,madhat2r-term-class (:foreground ,const-term))))

   `(company-tooltip-selection
     ((,madhat2r-class (:background ,lnum :foreground ,base))
      (,madhat2r-term-class (:background ,lnum-term :foreground ,base-term))))

   ;;;;; diff

   `(diff-added
     ((,madhat2r-class :background nil :foreground ,green)
      (,madhat2r-term-class :background nil :foreground ,green-term)))

   `(diff-changed
     ((,madhat2r-class :background nil :foreground ,keyword)
      (,madhat2r-term-class :background nil :foreground ,keyword-term)))

   `(diff-header
     ((,madhat2r-class :background ,cblk-ln-bg :foreground ,func)
      (,madhat2r-term-class :background ,cblk-ln-bg :foreground ,func-term)))

   `(diff-indicator-added
     ((,madhat2r-class :background nil :foreground ,green)
      (,madhat2r-term-class :background nil :foreground ,green-term)))

   `(diff-indicator-changed
     ((,madhat2r-class :background nil :foreground ,keyword)
      (,madhat2r-term-class :background nil :foreground ,keyword-term)))

   `(diff-indicator-removed
     ((,madhat2r-class :background nil :foreground ,red)
      (,madhat2r-term-class :background nil :foreground ,red-term)))

   `(diff-refine-added
     ((,madhat2r-class :background ,green :foreground ,bg4)
      (,madhat2r-term-class :background ,green-term :foreground ,bg4)))

   `(diff-refine-changed
     ((,madhat2r-class :background ,keyword :foreground ,bg4)
      (,madhat2r-term-class :background ,keyword-term :foreground ,bg4)))

   `(diff-refine-removed
     ((,madhat2r-class :background ,red :foreground ,bg4)
      (,madhat2r-term-class :background ,red-term :foreground ,bg4)))

   `(diff-removed
     ((,madhat2r-class :background nil :foreground ,red)
      (,madhat2r-term-class :background nil :foreground ,red-term)))

   ;;;;; diff-hl

   `(diff-hl-change
     ((,madhat2r-class :background ,blue-bg :foreground ,blue)
      (,madhat2r-term-class :background ,blue-bg :foreground ,blue-term)))

   `(diff-hl-delete
     ((,madhat2r-class :background ,red-bg :foreground ,red)
      (,madhat2r-term-class :background ,red-bg :foreground ,red-term)))

   `(diff-hl-insert
     ((,madhat2r-class :background ,green-bg :foreground ,green)
      (,madhat2r-term-class :background ,green-bg :foreground ,green-term)))

   ;;;;; dired

   `(dired-directory
     ((,madhat2r-class (:foreground ,keyword :background ,bg1 :inherit bold))
      (,madhat2r-term-class (:foreground ,keyword-term :background ,bg1 :inherit bold))))

   `(dired-flagged
     ((,madhat2r-class (:foreground ,red))
      (,madhat2r-term-class (:foreground ,red-term))))

   `(dired-header
     ((,madhat2r-class (:foreground ,comp :inherit bold))
      (,madhat2r-term-class (:foreground ,comp-term :inherit bold))))

   `(dired-ignored
     ((,madhat2r-class (:inherit shadow))
      (,madhat2r-term-class (:inherit shadow))))

   `(dired-mark
     ((,madhat2r-class (:foreground ,comp :inherit bold))
      (,madhat2r-term-class (:foreground ,comp-term :inherit bold))))

   `(dired-marked
     ((,madhat2r-class (:foreground ,magenta :inherit bold))
      (,madhat2r-term-class (:foreground ,magenta-term :inherit bold))))

   `(dired-perm-write
     ((,madhat2r-class (:foreground ,base :underline t))
      (,madhat2r-term-class (:foreground ,base-term :underline t))))

   `(dired-symlink
     ((,madhat2r-class (:foreground ,cyan :background ,bg1 :inherit bold))
      (,madhat2r-term-class (:foreground ,cyan-term :background ,bg1 :inherit bold))))

   `(dired-warning
     ((,madhat2r-class (:foreground ,war))
      (,madhat2r-term-class (:foreground ,war-term))))

   ;;;;; ediff

   `(ediff-current-diff-A
     ((,madhat2r-class (:background ,red-bg-s :foreground ,red))
      (,madhat2r-term-class (:background ,red-bg-s :foreground ,red-term))))

   `(ediff-current-diff-Ancestor
     ((,madhat2r-class (:background ,aqua-bg :foreground ,aqua))
      (,madhat2r-term-class (:background ,aqua-bg :foreground ,aqua-term))))

   `(ediff-current-diff-B
     ((,madhat2r-class (:background ,green-bg-s :foreground ,green))
      (,madhat2r-term-class (:background ,green-bg-s :foreground ,green-term))))

   `(ediff-current-diff-C
     ((,madhat2r-class (:background ,blue-bg :foreground ,blue))
      (,madhat2r-term-class (:background ,blue-bg :foreground ,blue-term))))

   `(ediff-even-diff-A
     ((,madhat2r-class (:background ,bg3))
      (,madhat2r-term-class (:background ,bg3))))

   `(ediff-even-diff-Ancestor
     ((,madhat2r-class (:background ,bg3))
      (,madhat2r-term-class (:background ,bg3))))

   `(ediff-even-diff-B
     ((,madhat2r-class (:background ,bg3))
      (,madhat2r-term-class (:background ,bg3))))

   `(ediff-even-diff-C
     ((,madhat2r-class (:background ,bg3))
      (,madhat2r-term-class (:background ,bg3))))

   `(ediff-fine-diff-A
     ((,madhat2r-class (:background nil :inherit bold))
      (,madhat2r-term-class (:background nil :inherit bold))))

   `(ediff-fine-diff-Ancestor
     ((,madhat2r-class (:background nil :inherit bold))
      (,madhat2r-term-class (:background nil :inherit bold))))

   `(ediff-fine-diff-B
     ((,madhat2r-class (:background nil :inherit bold))
      (,madhat2r-term-class (:background nil :inherit bold))))

   `(ediff-fine-diff-C
     ((,madhat2r-class (:background nil :inherit bold))
      (,madhat2r-term-class (:background nil :inherit bold))))

   `(ediff-odd-diff-A
     ((,madhat2r-class (:background ,bg4))
      (,madhat2r-term-class (:background ,bg4))))

   `(ediff-odd-diff-Ancestor
     ((,madhat2r-class (:background ,bg4))
      (,madhat2r-term-class (:background ,bg4))))

   `(ediff-odd-diff-B
     ((,madhat2r-class (:background ,bg4))
      (,madhat2r-term-class (:background ,bg4))))

   `(ediff-odd-diff-C
     ((,madhat2r-class (:background ,bg4))
      (,madhat2r-term-class (:background ,bg4))))

   ;;;;; ein

   `(ein:cell-input-area
     ((,madhat2r-class (:background ,bg2))
      (,madhat2r-term-class (:background ,bg2))))

   `(ein:cell-input-prompt
     ((,madhat2r-class (:foreground ,suc))
      (,madhat2r-term-class (:foreground ,suc-term))))

   `(ein:cell-output-prompt
     ((,madhat2r-class (:foreground ,err))
      (,madhat2r-term-class (:foreground ,err-term))))

   `(ein:notification-tab-normal
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(ein:notification-tab-selected
     ((,madhat2r-class (:foreground ,suc :inherit bold))
      (,madhat2r-term-class (:foreground ,suc-term :inherit bold))))

   ;;;;; eldoc

   `(eldoc-highlight-function-argument
     ((,madhat2r-class (:foreground ,mat :inherit bold))
      (,madhat2r-term-class (:foreground ,mat-term :inherit bold))))

   ;;;;; elfeed

   `(elfeed-search-title-face
     ((,madhat2r-class (:foreground ,base-dim))
      (,madhat2r-term-class (:foreground ,base-dim))))

   `(elfeed-search-unread-title-face
     ((,madhat2r-class (:foreground ,base))
      (,madhat2r-term-class (:foreground ,base-term))))

   `(elfeed-search-feed-face
     ((,madhat2r-class (:foreground ,blue))
      (,madhat2r-term-class (:foreground ,blue-term))))

   `(elfeed-search-tag-face
     ((,madhat2r-class (:foreground ,func))
      (,madhat2r-term-class (:foreground ,func-term))))

   ;;;;; enh-ruby

   `(enh-ruby-string-delimiter-face
     ((,madhat2r-class (:foreground ,str))
      (,madhat2r-term-class (:foreground ,str-term))))

   `(enh-ruby-op-face
     ((,madhat2r-class (:background ,bg1 :foreground ,base))
      (,madhat2r-term-class (:background ,bg1 :foreground ,base-term))))

   ;;;;; erc

   `(erc-input-face
     ((,madhat2r-class (:foreground ,func))
      (,madhat2r-term-class (:foreground ,func-term))))

   `(erc-my-nick-face
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(erc-nick-default-face
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(erc-nick-prefix-face
     ((,madhat2r-class (:foreground ,yellow))
      (,madhat2r-term-class (:foreground ,yellow-term))))

   `(erc-notice-face
     ((,madhat2r-class (:foreground ,str))
      (,madhat2r-term-class (:foreground ,str-term))))

   `(erc-prompt-face
     ((,madhat2r-class (:foreground ,mat :inherit bold))
      (,madhat2r-term-class (:foreground ,mat-term :inherit bold))))

   `(erc-timestamp-face
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   ;;;;; eshell

   `(eshell-ls-archive
     ((,madhat2r-class (:foreground ,red :inherit bold))
      (,madhat2r-term-class (:foreground ,red-term :inherit bold))))

   `(eshell-ls-backup
     ((,madhat2r-class (:inherit font-lock-comment-face))
      (,madhat2r-term-class (:inherit font-lock-comment-face))))

   `(eshell-ls-clutter
     ((,madhat2r-class (:inherit font-lock-comment-face))
      (,madhat2r-term-class (:inherit font-lock-comment-face))))

   `(eshell-ls-directory
     ((,madhat2r-class (:foreground ,keyword :inherit bold))
      (,madhat2r-term-class (:foreground ,keyword-term :inherit bold))))

   `(eshell-ls-executable
     ((,madhat2r-class (:foreground ,suc :inherit bold))
      (,madhat2r-term-class (:foreground ,suc-term :inherit bold))))

   `(eshell-ls-missing
     ((,madhat2r-class (:inherit font-lock-warning-face))
      (,madhat2r-term-class (:inherit font-lock-warning-face))))

   `(eshell-ls-product
     ((,madhat2r-class (:inherit font-lock-doc-face))
      (,madhat2r-term-class (:inherit font-lock-doc-face))))

   `(eshell-ls-special
     ((,madhat2r-class (:foreground ,yellow :inherit bold))
      (,madhat2r-term-class (:foreground ,yellow-term :inherit bold))))

   `(eshell-ls-symlink
     ((,madhat2r-class (:foreground ,cyan :inherit bold))
      (,madhat2r-term-class (:foreground ,cyan-term :inherit bold))))

   `(eshell-ls-unreadable
     ((,madhat2r-class (:foreground ,base))
      (,madhat2r-term-class (:foreground ,base-term))))

   `(eshell-prompt
     ((,madhat2r-class (:foreground ,keyword :inherit bold))
      (,madhat2r-term-class (:foreground ,keyword-term :inherit bold))))

   ;;;;; evil

   `(evil-ex-substitute-matches
     ((,madhat2r-class (:background ,red-bg :foreground ,red))
      (,madhat2r-term-class (:background ,red-bg :foreground ,red-term))))

   `(evil-ex-substitute-replacement
     ((,madhat2r-class (:background ,green-bg :foreground ,green))
      (,madhat2r-term-class (:background ,green-bg :foreground ,green-term))))

   ;;;;; flycheck

   `(flycheck-error
     ((,(append '((supports :underline (:style line))) madhat2r-class)
            (:underline (:style line :color ,err)))
              (,madhat2r-class (:foreground ,base :background ,err :inherit bold :underline t))))

   `(flycheck-error-list-checker-name
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(flycheck-fringe-error
     ((,madhat2r-class (:foreground ,err :inherit bold))
      (,madhat2r-term-class (:foreground ,err-term :inherit bold))))

   `(flycheck-fringe-info
     ((,madhat2r-class (:foreground ,keyword :inherit bold))
      (,madhat2r-term-class (:foreground ,keyword-term :inherit bold))))

   `(flycheck-fringe-warning
     ((,madhat2r-class (:foreground ,war :inherit bold))
      (,madhat2r-term-class (:foreground ,war-term :inherit bold))))

   `(flycheck-info
            ((,(append '((supports :underline (:style line))) madhat2r-class)
            (:underline (:style line :color ,keyword)))
              (,madhat2r-class (:foreground ,base :background ,keyword :inherit bold :underline t))))

   `(flycheck-warning
            ((,(append '((supports :underline (:style line))) madhat2r-class)
            (:underline (:style line :color ,war)))
              (,madhat2r-class (:foreground ,base :background ,war :inherit bold :underline t))))

   ;;;;; jabber

   `(jabber-activity-face
     ((,madhat2r-class (:inherit bold :foreground ,red))
      (,madhat2r-term-class (:inherit bold :foreground ,red-term))))

   `(jabber-activity-personal-face
     ((,madhat2r-class (:inherit bold :foreground ,blue))
      (,madhat2r-term-class (:inherit bold :foreground ,blue-term))))

   `(jabber-chat-error
     ((,madhat2r-class (:inherit bold :foreground ,red))
      (,madhat2r-term-class (:inherit bold :foreground ,red-term))))

   `(jabber-chat-prompt-foreign
     ((,madhat2r-class (:inherit bold :foreground ,red))
      (,madhat2r-term-class (:inherit bold :foreground ,red-term))))

   `(jabber-chat-prompt-local
     ((,madhat2r-class (:inherit bold :foreground ,blue))
      (,madhat2r-term-class (:inherit bold :foreground ,blue-term))))

   `(jabber-chat-prompt-system
     ((,madhat2r-class (:inherit bold :foreground ,green))
      (,madhat2r-term-class (:inherit bold :foreground ,green-term))))

   `(jabber-chat-text-foreign
     ((,madhat2r-class (:foreground ,base))
      (,madhat2r-term-class (:foreground ,base-term))))

   `(jabber-chat-text-local
     ((,madhat2r-class (:foreground ,base))
      (,madhat2r-term-class (:foreground ,base-term))))

   `(jabber-rare-time-face
     ((,madhat2r-class (:foreground ,green))
      (,madhat2r-term-class (:foreground ,green-term))))

   `(jabber-roster-user-away
     ((,madhat2r-class (:foreground ,yellow))
      (,madhat2r-term-class (:foreground ,yellow-term))))

   `(jabber-roster-user-chatty
     ((,madhat2r-class (:inherit bold :foreground ,green))
      (,madhat2r-term-class (:inherit bold :foreground ,green-term))))

   `(jabber-roster-user-dnd
     ((,madhat2r-class (:foreground ,red))
      (,madhat2r-term-class (:foreground ,red-term))))

   `(jabber-roster-user-error
     ((,madhat2r-class (:foreground ,err))
      (,madhat2r-term-class (:foreground ,err-term))))

   `(jabber-roster-user-offline
     ((,madhat2r-class (:foreground ,base))
      (,madhat2r-term-class (:foreground ,base-term))))

   `(jabber-roster-user-online
     ((,madhat2r-class (:inherit bold :foreground ,green))
      (,madhat2r-term-class (:inherit bold :foreground ,green-term))))

   `(jabber-roster-user-xa
     ((,madhat2r-class (:foreground ,aqua))
      (,madhat2r-term-class (:foreground ,aqua-term))))

   ;;;;; git-gutter-fr

   `(git-gutter-fr:added
     ((,madhat2r-class (:foreground ,green :inherit bold))
      (,madhat2r-term-class (:foreground ,green-term :inherit bold))))

   `(git-gutter-fr:deleted
     ((,madhat2r-class (:foreground ,war :inherit bold))
      (,madhat2r-term-class (:foreground ,war-term :inherit bold))))

   `(git-gutter-fr:modified
     ((,madhat2r-class (:foreground ,keyword :inherit bold))
      (,madhat2r-term-class (:foreground ,keyword-term :inherit bold))))

   ;;;;; git-timemachine

   `(git-timemachine-minibuffer-detail-face
     ((,madhat2r-class (:foreground ,blue :inherit bold :background ,blue-bg))
      (,madhat2r-term-class (:foreground ,blue-term :inherit bold :background ,blue-bg))))

   ;;;;; gnus

   `(gnus-emphasis-highlight-words
     ((,madhat2r-class (:background ,suc :foreground ,bg1))
      (,madhat2r-term-class (:background ,suc-term :foreground ,bg1))))

   `(gnus-header-content
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(gnus-header-from
     ((,madhat2r-class (:foreground ,var))
      (,madhat2r-term-class (:foreground ,var-term))))

   `(gnus-header-name
     ((,madhat2r-class (:foreground ,comp))
      (,madhat2r-term-class (:foreground ,comp-term))))

   `(gnus-header-subject
     ((,madhat2r-class (:foreground ,func :inherit bold))
      (,madhat2r-term-class (:foreground ,func-term :inherit bold))))

   `(gnus-summary-cancelled
     ((,madhat2r-class (:background ,war :foreground ,bg1))
      (,madhat2r-term-class (:background ,war-term :foreground ,bg1))))

   ;;;;; guide-key

   `(guide-key/highlight-command-face
     ((,madhat2r-class (:foreground ,base))
      (,madhat2r-term-class (:foreground ,base-term))))

   `(guide-key/key-face
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(guide-key/prefix-command-face
     ((,madhat2r-class (:foreground ,keyword :inherit bold))
      (,madhat2r-term-class (:foreground ,keyword-term :inherit bold))))

   ;;;;; helm

   `(helm-bookmark-directory
     ((,madhat2r-class (:inherit helm-ff-directory))
      (,madhat2r-term-class (:inherit helm-ff-directory))))

   `(helm-bookmark-file
     ((,madhat2r-class (:foreground ,base))
      (,madhat2r-term-class (:foreground ,base-term))))

   `(helm-bookmark-gnus
     ((,madhat2r-class (:foreground ,comp))
      (,madhat2r-term-class (:foreground ,comp-term))))

   `(helm-bookmark-info
     ((,madhat2r-class (:foreground ,comp))
      (,madhat2r-term-class (:foreground ,comp-term))))

   `(helm-bookmark-man
     ((,madhat2r-class (:foreground ,comp))
      (,madhat2r-term-class (:foreground ,comp-term))))

   `(helm-bookmark-w3m
     ((,madhat2r-class (:foreground ,comp))
      (,madhat2r-term-class (:foreground ,comp-term))))

   `(helm-buffer-directory
     ((,madhat2r-class (:foreground ,base :background ,bg1))
      (,madhat2r-term-class (:foreground ,base-term :background ,bg1))))

   `(helm-buffer-file
     ((,madhat2r-class (:foreground ,base :background ,bg1))
      (,madhat2r-term-class (:foreground ,base-term :background ,bg1))))

   `(helm-buffer-not-saved
     ((,madhat2r-class (:foreground ,comp :background ,bg1))
      (,madhat2r-term-class (:foreground ,comp-term :background ,bg1))))

   `(helm-buffer-process
     ((,madhat2r-class (:foreground ,keyword :background ,bg1))
      (,madhat2r-term-class (:foreground ,keyword-term :background ,bg1))))

   `(helm-buffer-saved-out
     ((,madhat2r-class (:foreground ,base :background ,bg1))
      (,madhat2r-term-class (:foreground ,base-term :background ,bg1))))

   `(helm-buffer-size
     ((,madhat2r-class (:foreground ,base :background ,bg1))
      (,madhat2r-term-class (:foreground ,base-term :background ,bg1))))

   `(helm-candidate-number
     ((,madhat2r-class (:background ,bg1 :foreground ,keyword :inherit bold))
      (,madhat2r-term-class (:background ,bg1 :foreground ,keyword-term :inherit bold))))

   `(helm-ff-directory
     ((,madhat2r-class (:foreground ,keyword :background ,bg1 :inherit bold))
      (,madhat2r-term-class (:foreground ,keyword-term :background ,bg1 :inherit bold))))

   `(helm-ff-dotted-directory
     ((,madhat2r-class (:foreground ,keyword :background ,bg1 :inherit bold))
      (,madhat2r-term-class (:foreground ,keyword-term :background ,bg1 :inherit bold))))

   `(helm-ff-dotted-symlink-directory
     ((,madhat2r-class (:foreground ,cyan :background ,bg1 :inherit bold))
      (,madhat2r-term-class (:foreground ,cyan-term :background ,bg1 :inherit bold))))

   `(helm-ff-executable
     ((,madhat2r-class (:foreground ,suc :background ,bg1 :weight normal))
      (,madhat2r-term-class (:foreground ,suc-term :background ,bg1 :weight normal))))

   `(helm-ff-file
     ((,madhat2r-class (:foreground ,base :background ,bg1 :weight normal))
      (,madhat2r-term-class (:foreground ,base-term :background ,bg1 :weight normal))))

   `(helm-ff-invalid-symlink
     ((,madhat2r-class (:foreground ,red :background ,bg1 :inherit bold))
      (,madhat2r-term-class (:foreground ,red-term :background ,bg1 :inherit bold))))

   `(helm-ff-prefix
     ((,madhat2r-class (:foreground ,bg1 :background ,keyword :weight normal))
      (,madhat2r-term-class (:foreground ,bg1 :background ,keyword-term :weight normal))))

   `(helm-ff-symlink
     ((,madhat2r-class (:foreground ,cyan :background ,bg1 :inherit bold))
      (,madhat2r-term-class (:foreground ,cyan-term :background ,bg1 :inherit bold))))

   `(helm-grep-cmd-line
     ((,madhat2r-class (:foreground ,base :background ,bg1))
      (,madhat2r-term-class (:foreground ,base-term :background ,bg1))))

   `(helm-grep-file
     ((,madhat2r-class (:foreground ,base :background ,bg1))
      (,madhat2r-term-class (:foreground ,base-term :background ,bg1))))

   `(helm-grep-finish
     ((,madhat2r-class (:foreground ,base :background ,bg1))
      (,madhat2r-term-class (:foreground ,base-term :background ,bg1))))

   `(helm-grep-lineno
     ((,madhat2r-class (:foreground ,type :background ,bg1 :inherit bold))
      (,madhat2r-term-class (:foreground ,type-term :background ,bg1 :inherit bold))))

   `(helm-grep-match
     ((,madhat2r-class (:foreground nil :background nil :inherit helm-match))
      (,madhat2r-term-class (:foreground nil :background nil :inherit helm-match))))

   `(helm-header
     ((,madhat2r-class (:foreground ,base :background ,bg1 :underline nil :box nil))
      (,madhat2r-term-class (:foreground ,base-term :background ,bg1 :underline nil :box nil))))

   `(helm-header-line-left-margin
     ((,madhat2r-class (:foreground ,keyword :background ,nil))
      (,madhat2r-term-class (:foreground ,keyword-term :background ,nil))))

   `(helm-match
     ((,madhat2r-class (:background ,complement-bg :foreground ,head1))
      (,madhat2r-term-class (:background ,complement-bg :foreground ,head1))))

   `(helm-match-item
     ((,madhat2r-class (:background ,complement-bg :foreground ,head1))
      (,madhat2r-term-class (:background ,complement-bg :foreground ,head1))))

   `(helm-moccur-buffer
     ((,madhat2r-class (:foreground ,var :background ,bg1))
      (,madhat2r-term-class (:foreground ,var-term :background ,bg1))))

   `(helm-selection
     ((,madhat2r-class (:background ,highlight))
      (,madhat2r-term-class (:background ,highlight-term))))

   `(helm-selection-line
     ((,madhat2r-class (:background ,bg2))
      (,madhat2r-term-class (:background ,bg2))))

   `(helm-separator
     ((,madhat2r-class (:foreground ,comp :background ,bg1))
      (,madhat2r-term-class (:foreground ,comp-term :background ,bg1))))

   `(helm-source-header
     ((,madhat2r-class (:background ,comp :foreground ,bg1 :inherit bold))
      (,madhat2r-term-class (:background ,comp-term :foreground ,bg1 :inherit bold))))

   `(helm-time-zone-current
     ((,madhat2r-class (:foreground ,keyword :background ,bg1))
      (,madhat2r-term-class (:foreground ,keyword-term :background ,bg1))))

   `(helm-time-zone-home
     ((,madhat2r-class (:foreground ,comp :background ,bg1))
      (,madhat2r-term-class (:foreground ,comp-term :background ,bg1))))

   `(helm-visible-mark
     ((,madhat2r-class (:foreground ,keyword :background ,bg3))
      (,madhat2r-term-class (:foreground ,keyword-term :background ,bg3))))

   ;;;;; helm-swoop

   `(helm-swoop-target-line-block-face
     ((,madhat2r-class (:foreground ,base :background ,highlight))
      (,madhat2r-term-class (:foreground ,base-term :background ,highlight))))

   `(helm-swoop-target-line-face
     ((,madhat2r-class (:background ,highlight))
      (,madhat2r-term-class (:background ,highlight-term))))

   `(helm-swoop-target-word-face
     ((,madhat2r-class (:background ,highlight :foreground ,mat))
      (,madhat2r-term-class (:background ,highlight-term :foreground ,mat))))

   ;;;;; highlights

   `(hi-yellow
     ((,madhat2r-class (:foreground ,yellow :background ,yellow-bg))
      (,madhat2r-term-class (:foreground ,yellow-term :background ,yellow-bg))))

   `(hi-green
     ((,madhat2r-class (:foreground ,green :background ,green-bg))
      (,madhat2r-term-class (:foreground ,green-term :background ,green-bg))))

   ;;;;; highlight-indentation

   `(highlight-indentation-face
     ((,madhat2r-class (:background ,comment-bg))
      (,madhat2r-term-class (:background ,comment-bg))))

   ;;;;; highlight-symbol

   `(highlight-symbol-face
     ((,madhat2r-class (:background ,bg2))
      (,madhat2r-term-class (:background ,bg2))))

   ;;;;; hydra

   `(hydra-face-blue
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(hydra-face-red
     ((,madhat2r-class (:foreground ,func))
      (,madhat2r-term-class (:foreground ,func-term))))

   ;;;;; ido

   `(ido-first-match
     ((,madhat2r-class (:foreground ,comp :inherit bold))
      (,madhat2r-term-class (:foreground ,comp-term :inherit bold))))

   `(ido-only-match
     ((,madhat2r-class (:foreground ,mat :inherit bold))
      (,madhat2r-term-class (:foreground ,mat-term :inherit bold))))

   `(ido-subdir
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(ido-vertical-match-face
     ((,madhat2r-class (:foreground ,comp :underline nil))
      (,madhat2r-term-class (:foreground ,comp-term :underline nil))))

   ;;;;; info

   `(info-header-xref
     ((,madhat2r-class (:foreground ,func :underline t))
      (,madhat2r-term-class (:foreground ,func-term :underline t))))

   `(info-xref
     ((,madhat2r-class (:foreground ,comp :underline t))
      (,madhat2r-term-class (:foreground ,comp-term :underline t))))

   `(info-menu
     ((,madhat2r-class (:foreground ,suc))
      (,madhat2r-term-class (:foreground ,suc-term))))

   `(info-node
     ((,madhat2r-class (:foreground ,func :inherit bold))
      (,madhat2r-term-class (:foreground ,func-term :inherit bold))))

   `(info-quoted-name
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(info-reference-item
     ((,madhat2r-class (:background nil :underline t :inherit bold))
      (,madhat2r-term-class (:background nil :underline t :inherit bold))))

   `(info-string
     ((,madhat2r-class (:foreground ,str))
      (,madhat2r-term-class (:foreground ,str-term))))

   `(info-title-1
     ((,madhat2r-class (:height 1.4 :inherit bold))
      (,madhat2r-term-class (:height 1.4 :inherit bold))))

   `(info-title-2
     ((,madhat2r-class (:height 1.3 :inherit bold))
      (,madhat2r-term-class (:height 1.3 :inherit bold))))

   `(info-title-3
     ((,madhat2r-class (:height 1.3))
      (,madhat2r-term-class (:height 1.3))))

   `(info-title-4
     ((,madhat2r-class (:height 1.2))
      (,madhat2r-term-class (:height 1.2))))

   ;;;;; ivy

   `(ivy-current-match
     ((,madhat2r-class (:background ,highlight :inherit bold))
      (,madhat2r-term-class (:background ,highlight-term :inherit bold))))

   `(ivy-minibuffer-match-face-1
     ((,madhat2r-class (:inherit bold))
      (,madhat2r-term-class (:inherit bold))))

   `(ivy-minibuffer-match-face-2
     ((,madhat2r-class (:foreground ,head1 :underline t))
      (,madhat2r-term-class (:foreground ,head1 :underline t))))

   `(ivy-minibuffer-match-face-3
     ((,madhat2r-class (:foreground ,head4 :underline t))
      (,madhat2r-term-class (:foreground ,head4 :underline t))))

   `(ivy-minibuffer-match-face-4
     ((,madhat2r-class (:foreground ,head3 :underline t))
      (,madhat2r-term-class (:foreground ,head3 :underline t))))

   `(ivy-remote
     ((,madhat2r-class (:foreground ,cyan))
      (,madhat2r-term-class (:foreground ,cyan-term))))

   ;;;;; latex

   `(font-latex-bold-face
     ((,madhat2r-class (:foreground ,comp))
      (,madhat2r-term-class (:foreground ,comp-term))))

   `(font-latex-italic-face
     ((,madhat2r-class (:foreground ,keyword :italic t))
      (,madhat2r-term-class (:foreground ,keyword-term :italic t))))

   `(font-latex-match-reference-keywords
     ((,madhat2r-class (:foreground ,const))
      (,madhat2r-term-class (:foreground ,const-term))))

   `(font-latex-match-variable-keywords
     ((,madhat2r-class (:foreground ,var))
      (,madhat2r-term-class (:foreground ,var-term))))

   `(font-latex-sectioning-0-face
     ((,madhat2r-class (:inherit bold :foreground ,head3 :height ,(if madhat2r-theme-org-height 1.3 1.0)))
      (,madhat2r-term-class (:inherit bold :foreground ,head3 :height ,(if madhat2r-theme-org-height 1.3 1.0)))))

   `(font-latex-sectioning-1-face
     ((,madhat2r-class (:inherit bold :foreground ,head4 :height ,(if madhat2r-theme-org-height 1.3 1.0)))
      (,madhat2r-term-class (:inherit bold :foreground ,head4 :height ,(if madhat2r-theme-org-height 1.3 1.0)))))

   `(font-latex-sectioning-2-face
     ((,madhat2r-class (:inherit bold :foreground ,head1 :height ,(if madhat2r-theme-org-height 1.3 1.0)))
      (,madhat2r-term-class (:inherit bold :foreground ,head1 :height ,(if madhat2r-theme-org-height 1.3 1.0)))))

   `(font-latex-sectioning-3-face
     ((,madhat2r-class (:inherit bold :foreground ,head2 :height ,(if madhat2r-theme-org-height 1.2 1.0)))
      (,madhat2r-term-class (:inherit bold :foreground ,head2 :height ,(if madhat2r-theme-org-height 1.2 1.0)))))

   `(font-latex-sectioning-4-face
     ((,madhat2r-class (:bold nil :foreground ,head3 :height ,(if madhat2r-theme-org-height 1.1 1.0)))
      (,madhat2r-term-class (:bold nil :foreground ,head3 :height ,(if madhat2r-theme-org-height 1.1 1.0)))))

   `(font-latex-sectioning-5-face
     ((,madhat2r-class (:bold nil :foreground ,head4))
      (,madhat2r-term-class (:bold nil :foreground ,head4))))

   `(font-latex-string-face
     ((,madhat2r-class (:foreground ,str))
      (,madhat2r-term-class (:foreground ,str-term))))

   ;;;;; linum-mode

   `(linum
     ((,madhat2r-class (:foreground ,lnum :background ,bg3))
      (,madhat2r-term-class (:foreground ,lnum-term :background ,bg3))))

   ;;;;; linum-relative

   `(linum-relative-current-face
     ((,madhat2r-class (:foreground ,comp))
      (,madhat2r-term-class (:foreground ,comp-term))))

   ;;;;; magit

   `(magit-blame-culprit
     ((,madhat2r-class :background ,yellow-bg :foreground ,yellow)
      (,madhat2r-term-class :background ,yellow-bg :foreground ,yellow-term)))

   `(magit-blame-header
     ((,madhat2r-class :background ,yellow-bg :foreground ,green)
      (,madhat2r-term-class :background ,yellow-bg :foreground ,green-term)))

   `(magit-blame-sha1
     ((,madhat2r-class :background ,yellow-bg :foreground ,func)
      (,madhat2r-term-class :background ,yellow-bg :foreground ,func-term)))

   `(magit-blame-subject
     ((,madhat2r-class :background ,yellow-bg :foreground ,yellow)
      (,madhat2r-term-class :background ,yellow-bg :foreground ,yellow-term)))

   `(magit-blame-time
     ((,madhat2r-class :background ,yellow-bg :foreground ,green)
      (,madhat2r-term-class :background ,yellow-bg :foreground ,green-term)))

   `(magit-blame-name
     ((,madhat2r-class :background ,yellow-bg :foreground ,yellow)
      (,madhat2r-term-class :background ,yellow-bg :foreground ,yellow-term)))

   `(magit-blame-heading
     ((,madhat2r-class :background ,yellow-bg :foreground ,green)
      (,madhat2r-term-class :background ,yellow-bg :foreground ,green-term)))

   `(magit-blame-hash
     ((,madhat2r-class :background ,yellow-bg :foreground ,func)
      (,madhat2r-term-class :background ,yellow-bg :foreground ,func-term)))

   `(magit-blame-summary
     ((,madhat2r-class :background ,yellow-bg :foreground ,yellow)
      (,madhat2r-term-class :background ,yellow-bg :foreground ,yellow-term)))

   `(magit-blame-date
     ((,madhat2r-class :background ,yellow-bg :foreground ,green)
      (,madhat2r-term-class :background ,yellow-bg :foreground ,green-term)))

   `(magit-branch
     ((,madhat2r-class (:foreground ,const :inherit bold))
      (,madhat2r-term-class (:foreground ,const-term :inherit bold))))

   `(magit-branch-current
     ((,madhat2r-class (:background ,blue-bg :foreground ,blue :inherit bold :box t))
      (,madhat2r-term-class (:background ,blue-bg :foreground ,blue-term :inherit bold :box t))))

   `(magit-branch-local
     ((,madhat2r-class (:background ,blue-bg :foreground ,blue :inherit bold))
      (,madhat2r-term-class (:background ,blue-bg :foreground ,blue-term :inherit bold))))

   `(magit-branch-remote
     ((,madhat2r-class (:background ,aqua-bg :foreground ,aqua :inherit bold))
      (,madhat2r-term-class (:background ,aqua-bg :foreground ,aqua-term :inherit bold))))

   `(magit-diff-context-highlight
     ((,madhat2r-class (:background ,bg2 :foreground ,base))
      (,madhat2r-term-class (:background ,bg2 :foreground ,base-term))))

   `(magit-diff-file-header
     ((,madhat2r-class (:background ,comment-bg :foreground ,comment))
      (,madhat2r-term-class (:background ,comment-bg :foreground ,comment-term))))

   `(magit-diff-file-heading
     ((,madhat2r-class (:background ,comment-bg :foreground ,comment))
      (,madhat2r-term-class (:background ,comment-bg :foreground ,comment-term))))

   `(magit-diff-file-heading-highlight
     ((,madhat2r-class (:background ,comment-bg :foreground ,comment))
      (,madhat2r-term-class (:background ,comment-bg :foreground ,comment-term))))

   `(magit-diff-hunk-header
     ((,madhat2r-class (:background ,ttip-bg :foreground ,ttip))
      (,madhat2r-term-class (:background ,ttip-bg :foreground ,ttip-term))))

   `(magit-diff-hunk-heading
     ((,madhat2r-class (:background ,ttip-bg :foreground ,ttip))
      (,madhat2r-term-class (:background ,ttip-bg :foreground ,ttip-term))))

   `(magit-diff-hunk-heading-highlight
     ((,madhat2r-class (:background ,ttip-bg :foreground ,ttip))
      (,madhat2r-term-class (:background ,ttip-bg :foreground ,ttip-term))))

   `(magit-hash
     ((,madhat2r-class (:foreground ,var))
      (,madhat2r-term-class (:foreground ,var-term))))

   `(magit-hunk-heading
     ((,madhat2r-class (:background ,bg3))
      (,madhat2r-term-class (:background ,bg3))))

   `(magit-hunk-heading-highlight
     ((,madhat2r-class (:background ,bg3))
      (,madhat2r-term-class (:background ,bg3))))

   `(magit-item-highlight
     ((,madhat2r-class :background ,bg2)
      (,madhat2r-term-class :background ,bg2)))

   `(magit-log-author
     ((,madhat2r-class (:foreground ,func))
      (,madhat2r-term-class (:foreground ,func-term))))

   `(magit-log-head-label-head
     ((,madhat2r-class (:background ,yellow :foreground ,bg1 :inherit bold))
      (,madhat2r-term-class (:background ,yellow-term :foreground ,bg1 :inherit bold))))

   `(magit-log-head-label-local
     ((,madhat2r-class (:background ,keyword :foreground ,bg1 :inherit bold))
      (,madhat2r-term-class (:background ,keyword-term :foreground ,bg1 :inherit bold))))

   `(magit-log-head-label-remote
     ((,madhat2r-class (:background ,suc :foreground ,bg1 :inherit bold))
      (,madhat2r-term-class (:background ,suc-term :foreground ,bg1 :inherit bold))))

   `(magit-log-head-label-tags
     ((,madhat2r-class (:background ,magenta :foreground ,bg1 :inherit bold))
      (,madhat2r-term-class (:background ,magenta-term :foreground ,bg1 :inherit bold))))

   `(magit-log-head-label-wip
     ((,madhat2r-class (:background ,cyan :foreground ,bg1 :inherit bold))
      (,madhat2r-term-class (:background ,cyan-term :foreground ,bg1 :inherit bold))))

   `(magit-log-sha1
     ((,madhat2r-class (:foreground ,str))
      (,madhat2r-term-class (:foreground ,str-term))))

   `(magit-process-ng
     ((,madhat2r-class (:foreground ,war :inherit bold))
      (,madhat2r-term-class (:foreground ,war-term :inherit bold))))

   `(magit-process-ok
     ((,madhat2r-class (:foreground ,func :inherit bold))
      (,madhat2r-term-class (:foreground ,func-term :inherit bold))))

   `(magit-section-heading
     ((,madhat2r-class (:foreground ,keyword :inherit bold))
      (,madhat2r-term-class (:foreground ,keyword-term :inherit bold))))

   `(magit-section-highlight
     ((,madhat2r-class (:background ,bg2))
      (,madhat2r-term-class (:background ,bg2))))

   `(magit-section-title
     ((,madhat2r-class (:background ,bg1 :foreground ,keyword :inherit bold))
      (,madhat2r-term-class (:background ,bg1 :foreground ,keyword-term :inherit bold))))

   ;;;;; man

   `(Man-overstrike
     ((,madhat2r-class (:foreground ,head1 :inherit bold))
      (,madhat2r-term-class (:foreground ,head1 :inherit bold))))

   `(Man-reverse
     ((,madhat2r-class (:foreground ,highlight))
      (,madhat2r-term-class (:foreground ,highlight-term))))

   `(Man-underline
     ((,madhat2r-class (:foreground ,comp :underline t))
      (,madhat2r-term-class (:foreground ,comp-term :underline t))))

   ;;;;; markdown

   `(markdown-header-face-1
     ((,madhat2r-class (:inherit bold :foreground ,head1 :height ,(if madhat2r-theme-org-height 1.3 1.0) ))
      (,madhat2r-term-class (:inherit bold :foreground ,head1 :height ,(if madhat2r-theme-org-height 1.3 1.0) ))))

   `(markdown-header-face-2
     ((,madhat2r-class (:inherit bold :foreground ,head2 :height ,(if madhat2r-theme-org-height 1.2 1.0) ))
      (,madhat2r-term-class (:inherit bold :foreground ,head2 :height ,(if madhat2r-theme-org-height 1.2 1.0) ))))

   `(markdown-header-face-3
     ((,madhat2r-class (:bold nil :foreground ,head3 :height ,(if madhat2r-theme-org-height 1.1 1.0)))
      (,madhat2r-term-class (:bold nil :foreground ,head3 :height ,(if madhat2r-theme-org-height 1.1 1.0)))))

   `(markdown-header-face-4
     ((,madhat2r-class (:bold nil :foreground ,head4))
      (,madhat2r-term-class (:bold nil :foreground ,head4))))

   `(markdown-header-face-5
     ((,madhat2r-class (:bold nil :foreground ,head1))
      (,madhat2r-term-class (:bold nil :foreground ,head1))))

   `(markdown-header-face-6
     ((,madhat2r-class (:bold nil :foreground ,head2))
      (,madhat2r-term-class (:bold nil :foreground ,head2))))

   ;;;;; mode-line

   `(mode-line
     ((,madhat2r-class (:foreground ,base :background ,act1 :box (:color ,border :line-width 1)))
      (,madhat2r-term-class (:foreground ,base-term :background ,act1 :box (:color ,border :line-width 1)))))

   `(mode-line-inactive
     ((,madhat2r-class (:foreground ,base :background ,bg1  :box (:color ,border :line-width 1)))
      (,madhat2r-term-class (:foreground ,base-term :background ,bg1  :box (:color ,border :line-width 1)))))

   `(mode-line-buffer-id
     ((,madhat2r-class (:inherit bold :foreground ,func))
      (,madhat2r-term-class (:inherit bold :foreground ,func-term))))

   ;;;;; mu4e

   `(mu4e-cited-1-face
     ((,madhat2r-class (:foreground ,base))
      (,madhat2r-term-class (:foreground ,base-term))))

   `(mu4e-cited-7-face
     ((,madhat2r-class (:foreground ,base))
      (,madhat2r-term-class (:foreground ,base-term))))

   `(mu4e-header-marks-face
     ((,madhat2r-class (:foreground ,comp))
      (,madhat2r-term-class (:foreground ,comp-term))))

   `(mu4e-header-key-face
     ((,madhat2r-class (:foreground ,head2 :inherit bold))
      (,madhat2r-term-class (:foreground ,head2 :inherit bold))))

   `(mu4e-view-url-number-face
     ((,madhat2r-class (:foreground ,comp))
      (,madhat2r-term-class (:foreground ,comp-term))))

   `(mu4e-unread-face
     ((,madhat2r-class (:foreground ,yellow :inherit bold))
      (,madhat2r-term-class (:foreground ,yellow-term :inherit bold))))

   ;;;;; neotree

   `(neo-dir-link-face
     ((,madhat2r-class (:foreground ,keyword :inherit bold))
      (,madhat2r-term-class (:foreground ,keyword-term :inherit bold))))

   `(neo-expand-btn-face
     ((,madhat2r-class (:foreground ,base))
      (,madhat2r-term-class (:foreground ,base-term))))

   `(neo-file-link-face
     ((,madhat2r-class (:foreground ,base))
      (,madhat2r-term-class (:foreground ,base-term))))

   `(neo-root-dir-face
     ((,madhat2r-class (:foreground ,func :inherit bold))
      (,madhat2r-term-class (:foreground ,func-term :inherit bold))))

   ;;;;; org

   `(org-agenda-clocking
     ((,madhat2r-class (:background ,highlight :foreground ,comp))
      (,madhat2r-term-class (:background ,highlight-term :foreground ,comp))))

   `(org-agenda-date
     ((,madhat2r-class (:foreground ,var :height ,(if madhat2r-theme-org-agenda-height 1.1 1.0)))
      (,madhat2r-term-class (:foreground ,var-term :height ,(if madhat2r-theme-org-agenda-height 1.1 1.0)))))

   `(org-agenda-date-today
     ((,madhat2r-class (:foreground ,keyword :inherit bold :height ,(if madhat2r-theme-org-agenda-height 1.3 1.0)))
      (,madhat2r-term-class (:foreground ,keyword-term :inherit bold :height ,(if madhat2r-theme-org-agenda-height 1.3 1.0)))))

   `(org-agenda-date-weekend
     ((,madhat2r-class (:inherit bold :foreground ,var))
      (,madhat2r-term-class (:inherit bold :foreground ,var-term))))

   `(org-agenda-done
     ((,madhat2r-class (:foreground ,suc :height ,(if madhat2r-theme-org-agenda-height 1.2 1.0)))
      (,madhat2r-term-class (:foreground ,suc-term :height ,(if madhat2r-theme-org-agenda-height 1.2 1.0)))))

   `(org-agenda-structure
     ((,madhat2r-class (:inherit bold :foreground ,comp))
      (,madhat2r-term-class (:inherit bold :foreground ,comp-term))))

   `(org-block
     ((,madhat2r-class (:background ,cblk-bg :foreground ,cblk))
      (,madhat2r-term-class (:background ,cblk-bg :foreground ,cblk-term))))

   `(org-block-begin-line
     ((,madhat2r-class (:background ,cblk-ln-bg :foreground ,cblk-ln))
      (,madhat2r-term-class (:background ,cblk-ln-bg :foreground ,cblk-ln))))

   `(org-block-end-line
     ((,madhat2r-class (:background ,cblk-ln-bg :foreground ,cblk-ln))
      (,madhat2r-term-class (:background ,cblk-ln-bg :foreground ,cblk-ln))))

   `(org-clock-overlay
     ((,madhat2r-class (:foreground ,comp))
      (,madhat2r-term-class (:foreground ,comp-term))))

   `(org-code
     ((,madhat2r-class (:foreground ,cyan))
      (,madhat2r-term-class (:foreground ,cyan-term))))

   `(org-column
     ((,madhat2r-class (:background ,highlight))
      (,madhat2r-term-class (:background ,highlight-term))))

   `(org-column-title
     ((,madhat2r-class (:background ,highlight))
      (,madhat2r-term-class (:background ,highlight-term))))

   `(org-date
     ((,madhat2r-class (:underline t :foreground ,var))
      (,madhat2r-term-class (:underline t :foreground ,var-term))))

   `(org-date-selected
     ((,madhat2r-class (:background ,func :foreground ,bg1))
      (,madhat2r-term-class (:background ,func-term :foreground ,bg1))))

   `(org-document-info-keyword
     ((,madhat2r-class (:foreground ,meta))
      (,madhat2r-term-class (:foreground ,meta-term))))

   `(org-document-title
     ((,madhat2r-class (:foreground ,func :inherit bold :height ,(if madhat2r-theme-org-height 1.4 1.0) :underline t))
      (,madhat2r-term-class (:foreground ,func-term :inherit bold :height ,(if madhat2r-theme-org-height 1.4 1.0) :underline t))))

   `(org-done
     ((,madhat2r-class (:foreground ,suc :inherit bold :background ,green-bg))
      (,madhat2r-term-class (:foreground ,suc-term :inherit bold :background ,green-bg))))

   `(org-ellipsis
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(org-footnote
     ((,madhat2r-class (:underline t :foreground ,base))
      (,madhat2r-term-class (:underline t :foreground ,base-term))))

   `(org-hide
     ((,madhat2r-class (:foreground ,base))
      (,madhat2r-term-class (:foreground ,base-term))))

   `(org-kbd
     ((,madhat2r-class (:inherit region :foreground ,base :box (:line-width 1 :style released-button)))
      (,madhat2r-term-class (:inherit region :foreground ,base-term :box (:line-width 1 :style released-button)))))

   `(org-level-1
     ((,madhat2r-class (:inherit bold :foreground ,head1 :height ,(if madhat2r-theme-org-height 1.3 1.0)))
      (,madhat2r-term-class (:inherit bold :foreground ,head1 :height ,(if madhat2r-theme-org-height 1.3 1.0)))))

   `(org-level-2
     ((,madhat2r-class (:inherit bold :foreground ,head2 :height ,(if madhat2r-theme-org-height 1.2 1.0)))
      (,madhat2r-term-class (:inherit bold :foreground ,head2 :height ,(if madhat2r-theme-org-height 1.2 1.0)))))

   `(org-level-3
     ((,madhat2r-class (:bold nil :foreground ,head3 :height ,(if madhat2r-theme-org-height 1.1 1.0)))
      (,madhat2r-term-class (:bold nil :foreground ,head3 :height ,(if madhat2r-theme-org-height 1.1 1.0)))))

   `(org-level-4
     ((,madhat2r-class (:bold nil :foreground ,head4))
      (,madhat2r-term-class (:bold nil :foreground ,head4))))

   `(org-level-5
     ((,madhat2r-class (:bold nil :foreground ,head1))
      (,madhat2r-term-class (:bold nil :foreground ,head1))))

   `(org-level-6
     ((,madhat2r-class (:bold nil :foreground ,head2))
      (,madhat2r-term-class (:bold nil :foreground ,head2))))

   `(org-level-7
     ((,madhat2r-class (:bold nil :foreground ,head3))
      (,madhat2r-term-class (:bold nil :foreground ,head3))))

   `(org-level-8
     ((,madhat2r-class (:bold nil :foreground ,head4))
      (,madhat2r-term-class (:bold nil :foreground ,head4))))

   `(org-link
     ((,madhat2r-class (:underline t :foreground ,link))
      (,madhat2r-term-class (:underline t :foreground ,link-term))))

   `(org-meta-line
     ((,madhat2r-class (:foreground ,meta))
      (,madhat2r-term-class (:foreground ,meta-term))))

   `(org-mode-line-clock-overrun
     ((,madhat2r-class (:foreground ,err))
      (,madhat2r-term-class (:foreground ,err-term))))

   `(org-priority
     ((,madhat2r-class (:foreground ,war :inherit bold))
      (,madhat2r-term-class (:foreground ,war-term :inherit bold))))

   `(org-quote
     ((,madhat2r-class (:inherit org-block :slant italic))
      (,madhat2r-term-class (:inherit org-block :slant italic))))

   `(org-scheduled
     ((,madhat2r-class (:foreground ,comp))
      (,madhat2r-term-class (:foreground ,comp-term))))

   `(org-scheduled-today
     ((,madhat2r-class (:foreground ,func :height ,(if madhat2r-theme-org-agenda-height 1.2 1.0)))
      (,madhat2r-term-class (:foreground ,func-term :height ,(if madhat2r-theme-org-agenda-height 1.2 1.0)))))

   `(org-sexp-date
     ((,madhat2r-class (:foreground ,base))
      (,madhat2r-term-class (:foreground ,base-term))))

   `(org-special-keyword
     ((,madhat2r-class (:foreground ,func))
      (,madhat2r-term-class (:foreground ,func-term))))

   `(org-table
     ((,madhat2r-class (:foreground ,base :background ,complement-bg))
      (,madhat2r-term-class (:foreground ,base-term :background ,complement-bg))))

   `(org-time-grid
     ((,madhat2r-class (:foreground ,str))
      (,madhat2r-term-class (:foreground ,str-term))))

   `(org-todo
     ((,madhat2r-class (:foreground ,war :inherit bold :background ,yellow-bg))
      (,madhat2r-term-class (:foreground ,war-term :inherit bold :background ,yellow-bg))))

   `(org-verbatim
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(org-verse
     ((,madhat2r-class (:inherit org-block :slant italic))
      (,madhat2r-term-class (:inherit org-block :slant italic))))

   `(org-warning
     ((,madhat2r-class (:foreground ,err))
      (,madhat2r-term-class (:foreground ,err-term))))

   ;;;;; perspective

   `(persp-selected-face
     ((,madhat2r-class (:inherit bold :foreground ,func))
      (,madhat2r-term-class (:inherit bold :foreground ,func-term))))

   ;;;;; popup

   `(popup-face
     ((,madhat2r-class (:background ,ttip-bg :foreground ,ttip))
      (,madhat2r-term-class (:background ,ttip-bg :foreground ,ttip-term))))

   `(popup-tip-face
     ((,madhat2r-class (:background ,ttip-sl :foreground ,base :bold nil :italic nil :underline nil))
      (,madhat2r-term-class (:background ,ttip-sl :foreground ,base-term :bold nil :italic nil :underline nil))))

   `(popup-menu-face
     ((,madhat2r-class (:background ,ttip-bg :foreground ,base))
      (,madhat2r-term-class (:background ,ttip-bg :foreground ,base-term))))

   `(popup-enu-selection-face
     ((,madhat2r-class (:background ,ttip-sl :foreground ,base))
      (,madhat2r-term-class (:background ,ttip-sl :foreground ,base-term))))

   `(popup-menu-mouse-face
     ((,madhat2r-class (:inherit highlight))
      (,madhat2r-term-class (:inherit highlight))))

   `(popup-isearch-match
     ((,madhat2r-class (:inherit match))
      (,madhat2r-term-class (:inherit match))))

   `(popup-scroll-bar-foreground-face
     ((,madhat2r-class (:background ,act2))
      (,madhat2r-term-class (:background ,act2))))

   `(popup-scroll-bar-background-face
     ((,madhat2r-class (:background ,bg2))
      (,madhat2r-term-class (:background ,bg2))))

   ;;;;; powerline

   `(powerline-active1
     ((,madhat2r-class (:background ,act2 :foreground ,base))
      (,madhat2r-term-class (:background ,act2 :foreground ,base-term))))

   `(powerline-active2
     ((,madhat2r-class (:background ,act2 :foreground ,base))
      (,madhat2r-term-class (:background ,act2 :foreground ,base-term))))

   `(powerline-inactive1
     ((,madhat2r-class (:background ,bg2 :foreground ,base))
      (,madhat2r-term-class (:background ,bg2 :foreground ,base-term))))

   `(powerline-inactive2
     ((,madhat2r-class (:background ,bg2 :foreground ,base))
      (,madhat2r-term-class (:background ,bg2 :foreground ,base-term))))

   ;;;;; rainbow-delimiters

   `(rainbow-delimiters-depth-1-face
     ((,madhat2r-class :foreground ,keyword)
      (,madhat2r-term-class :foreground ,keyword-term)))

   `(rainbow-delimiters-depth-2-face
     ((,madhat2r-class :foreground ,func)
      (,madhat2r-term-class :foreground ,func-term)))

   `(rainbow-delimiters-depth-3-face
     ((,madhat2r-class :foreground ,str)
      (,madhat2r-term-class :foreground ,str-term)))

   `(rainbow-delimiters-depth-4-face
     ((,madhat2r-class :foreground ,green)
      (,madhat2r-term-class :foreground ,green-term)))

   `(rainbow-delimiters-depth-5-face
     ((,madhat2r-class :foreground ,yellow)
      (,madhat2r-term-class :foreground ,yellow-term)))

   `(rainbow-delimiters-depth-6-face
     ((,madhat2r-class :foreground ,keyword)
      (,madhat2r-term-class :foreground ,keyword-term)))

   `(rainbow-delimiters-depth-7-face
     ((,madhat2r-class :foreground ,func)
      (,madhat2r-term-class :foreground ,func-term)))

   `(rainbow-delimiters-depth-8-face
     ((,madhat2r-class :foreground ,str)
      (,madhat2r-term-class :foreground ,str-term)))

   `(rainbow-delimiters-unmatched-face
     ((,madhat2r-class :foreground ,err :overline t)
      (,madhat2r-term-class :foreground ,err-term :overline t)))

   `(rainbow-delimiters-mismatched-face
     ((,madhat2r-class :foreground ,err :overline t)
      (,madhat2r-term-class :foreground ,err-term :overline t)))

   ;;;;; shm

   `(shm-current-face
     ((,madhat2r-class (:background ,green-bg-s))
      (,madhat2r-term-class (:background ,green-bg-s))))

   `(shm-quarantine-face
     ((,madhat2r-class (:background ,red-bg-s))
      (,madhat2r-term-class (:background ,red-bg-s))))

   ;;;;; show-paren

   `(show-paren-match
     ((,madhat2r-class (:background ,green-bg-s))
      (,madhat2r-term-class (:background ,green-bg-s))))

   `(show-paren-mismatch
     ((,madhat2r-class (:background ,red-bg-s))
      (,madhat2r-term-class (:background ,red-bg-s))))

   ;;;;; smartparens

   `(sp-pair-overlay-face
     ((,madhat2r-class (:background ,highlight :foreground nil))
      (,madhat2r-term-class (:background ,highlight-term :foreground nil))))

   `(sp-show-pair-match-face
     ((,madhat2r-class (:background ,mat :foreground ,bg1 :inherit bold))
      (,madhat2r-term-class (:background ,mat-term :foreground ,bg1 :inherit bold))))

   ;;;;; spaceline

   `(spaceline-python-venv
     ((,madhat2r-class (:foreground ,comp))
      (,madhat2r-term-class (:foreground ,comp-term))))

   `(spaceline-flycheck-error
     ((,madhat2r-class (:foreground ,err))
      (,madhat2r-term-class (:foreground ,err-term))))

   `(spaceline-flycheck-info
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(spaceline-flycheck-warning
     ((,madhat2r-class (:foreground ,war))
      (,madhat2r-term-class (:foreground ,war-term))))

   ;;;;; spacemacs-specific

   `(spacemacs-transient-state-title-face
     ((,madhat2r-class (:background nil :foreground ,keyword :box nil :inherit bold))
      (,madhat2r-term-class (:background nil :foreground ,keyword-term :box nil :inherit bold))))

   ;;;;; swiper

   `(swiper-line-face
     ((,madhat2r-class (:background ,highlight :inherit bold))
      (,madhat2r-term-class (:background ,highlight-term :inherit bold))))

   `(swiper-match-face-1
     ((,madhat2r-class (:inherit bold))
      (,madhat2r-term-class (:inherit bold))))

   `(swiper-match-face-2
     ((,madhat2r-class (:foreground ,head1 :underline t))
      (,madhat2r-term-class (:foreground ,head1 :underline t))))

   `(swiper-match-face-3
     ((,madhat2r-class (:foreground ,head4 :underline t))
      (,madhat2r-term-class (:foreground ,head4 :underline t))))

   `(swiper-match-face-4
     ((,madhat2r-class (:foreground ,head3 :underline t))
      (,madhat2r-term-class (:foreground ,head3 :underline t))))

   ;;;;; term

   `(term
     ((,madhat2r-class (:foreground ,base :background ,bg1))
      (,madhat2r-term-class (:foreground ,base-term :background ,bg1))))

   `(term-color-black
     ((,madhat2r-class (:foreground ,bg4))
      (,madhat2r-term-class (:foreground ,bg4))))

   `(term-color-blue
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(term-color-cyan
     ((,madhat2r-class (:foreground ,cyan))
      (,madhat2r-term-class (:foreground ,cyan-term))))

   `(term-color-green
     ((,madhat2r-class (:foreground ,green))
      (,madhat2r-term-class (:foreground ,green-term))))

   `(term-color-magenta
     ((,madhat2r-class (:foreground ,magenta))
      (,madhat2r-term-class (:foreground ,magenta-term))))

   `(term-color-red
     ((,madhat2r-class (:foreground ,red))
      (,madhat2r-term-class (:foreground ,red-term))))

   `(term-color-white
     ((,madhat2r-class (:foreground ,base))
      (,madhat2r-term-class (:foreground ,base-term))))

   `(term-color-yellow
     ((,madhat2r-class (:foreground ,yellow))
      (,madhat2r-term-class (:foreground ,yellow-term))))

   ;;;;; web-mode

   `(web-mode-builtin-face
     ((,madhat2r-class (:inherit ,font-lock-builtin-face))
      (,madhat2r-term-class (:inherit ,font-lock-builtin-face))))

   `(web-mode-comment-face
     ((,madhat2r-class (:inherit ,font-lock-comment-face))
      (,madhat2r-term-class (:inherit ,font-lock-comment-face))))

   `(web-mode-constant-face
     ((,madhat2r-class (:inherit ,font-lock-constant-face))
      (,madhat2r-term-class (:inherit ,font-lock-constant-face))))

   `(web-mode-doctype-face
     ((,madhat2r-class (:inherit ,font-lock-comment-face))
      (,madhat2r-term-class (:inherit ,font-lock-comment-face))))

   `(web-mode-function-name-face
     ((,madhat2r-class (:inherit ,font-lock-function-name-face))
      (,madhat2r-term-class (:inherit ,font-lock-function-name-face))))

   `(web-mode-html-attr-name-face
     ((,madhat2r-class (:foreground ,func))
      (,madhat2r-term-class (:foreground ,func-term))))

   `(web-mode-html-attr-value-face
     ((,madhat2r-class (:foreground ,var))
      (,madhat2r-term-class (:foreground ,var-term))))

   `(web-mode-html-tag-face
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(web-mode-keyword-face
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(web-mode-string-face
     ((,madhat2r-class (:foreground ,str))
      (,madhat2r-term-class (:foreground ,str-term))))

   `(web-mode-symbol-face
     ((,madhat2r-class (:foreground ,type))
      (,madhat2r-term-class (:foreground ,type-term))))

   `(web-mode-type-face
     ((,madhat2r-class (:inherit ,font-lock-type-face))
      (,madhat2r-term-class (:inherit ,font-lock-type-face))))

   `(web-mode-warning-face
     ((,madhat2r-class (:inherit ,font-lock-warning-face))
      (,madhat2r-term-class (:inherit ,font-lock-warning-face))))

   `(web-mode-current-element-highlight-face
     ((,madhat2r-class (:background ,mat))
      (,madhat2r-term-class (:background ,mat-term))))

   ;;;;; which-key

   `(which-key-command-description-face
     ((,madhat2r-class (:foreground ,base))
      (,madhat2r-term-class (:foreground ,base-term))))

   `(which-key-group-description-face
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(which-key-key-face
     ((,madhat2r-class (:foreground ,func :inherit bold))
      (,madhat2r-term-class (:foreground ,func-term :inherit bold))))

   `(which-key-separator-face
     ((,madhat2r-class (:background nil :foreground ,str))
      (,madhat2r-term-class (:background nil :foreground ,str-term))))

   `(which-key-special-key-face
     ((,madhat2r-class (:background ,func :foreground ,bg1))
      (,madhat2r-term-class (:background ,func-term :foreground ,bg1))))

   ;;;;; which-function-mode

   `(which-func
     ((,madhat2r-class (:foreground ,func))
      (,madhat2r-term-class (:foreground ,func-term))))

   ;;;;; whitespace-mode

   `(whitespace-empty
     ((,madhat2r-class (:background nil :foreground ,yellow))
      (,madhat2r-term-class (:background nil :foreground ,yellow-term))))

   `(whitespace-indentation
     ((,madhat2r-class (:background nil :foreground ,war))
      (,madhat2r-term-class (:background nil :foreground ,war-term))))

   `(whitespace-line
     ((,madhat2r-class (:background nil :foreground ,comp))
      (,madhat2r-term-class (:background nil :foreground ,comp-term))))

   `(whitespace-newline
     ((,madhat2r-class (:background nil :foreground ,comp))
      (,madhat2r-term-class (:background nil :foreground ,comp-term))))

   `(whitespace-space
     ((,madhat2r-class (:background nil :foreground ,act2))
      (,madhat2r-term-class (:background nil :foreground ,act2))))

   `(whitespace-space-after-tab
     ((,madhat2r-class (:background nil :foreground ,yellow))
      (,madhat2r-term-class (:background nil :foreground ,yellow-term))))

   `(whitespace-space-before-tab
     ((,madhat2r-class (:background nil :foreground ,yellow))
      (,madhat2r-term-class (:background nil :foreground ,yellow-term))))

   `(whitespace-tab
     ((,madhat2r-class (:background nil))
      (,madhat2r-term-class (:background nil))))

   `(whitespace-trailing
     ((,madhat2r-class (:background ,err :foreground ,war))
      (,madhat2r-term-class (:background ,err-term :foreground ,war))))

   ;;;;; other, need more work

   `(ac-completion-face
     ((,madhat2r-class (:underline t :foreground ,keyword))
      (,madhat2r-term-class (:underline t :foreground ,keyword-term))))

   `(ffap
     ((,madhat2r-class (:foreground ,base))
      (,madhat2r-term-class (:foreground ,base-term))))

   `(flx-highlight-face
     ((,madhat2r-class (:foreground ,comp :underline nil))
      (,madhat2r-term-class (:foreground ,comp-term :underline nil))))

   `(icompletep-determined
     ((,madhat2r-class :foreground ,keyword)
      (,madhat2r-term-class :foreground ,keyword-term)))

   `(js2-external-variable
     ((,madhat2r-class (:foreground ,comp))
      (,madhat2r-term-class (:foreground ,comp-term))))

   `(js2-function-param
     ((,madhat2r-class (:foreground ,const))
      (,madhat2r-term-class (:foreground ,const-term))))

   `(js2-jsdoc-html-tag-delimiter
     ((,madhat2r-class (:foreground ,str))
      (,madhat2r-term-class (:foreground ,str-term))))

   `(js2-jsdoc-html-tag-name
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(js2-jsdoc-value
     ((,madhat2r-class (:foreground ,str))
      (,madhat2r-term-class (:foreground ,str-term))))

   `(js2-private-function-call
     ((,madhat2r-class (:foreground ,const))
      (,madhat2r-term-class (:foreground ,const-term))))

   `(js2-private-member
     ((,madhat2r-class (:foreground ,base))
      (,madhat2r-term-class (:foreground ,base-term))))

   `(js3-error-face
     ((,madhat2r-class (:underline ,war))
      (,madhat2r-term-class (:underline ,war-term))))

   `(js3-external-variable-face
     ((,madhat2r-class (:foreground ,var))
      (,madhat2r-term-class (:foreground ,var-term))))

   `(js3-function-param-face
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(js3-instance-member-face
     ((,madhat2r-class (:foreground ,const))
      (,madhat2r-term-class (:foreground ,const-term))))

   `(js3-jsdoc-tag-face
     ((,madhat2r-class (:foreground ,keyword))
      (,madhat2r-term-class (:foreground ,keyword-term))))

   `(js3-warning-face
     ((,madhat2r-class (:underline ,keyword))
      (,madhat2r-term-class (:underline ,keyword-term))))

   `(slime-repl-inputed-output-face
     ((,madhat2r-class (:foreground ,comp))
      (,madhat2r-term-class (:foreground ,comp-term))))

   `(trailing-whitespace
     ((,madhat2r-class :foreground nil :background ,err)
      (,madhat2r-term-class :foreground nil :background ,err-term)))

   `(undo-tree-visualizer-current-face
     ((,madhat2r-class :foreground ,keyword)
      (,madhat2r-term-class :foreground ,keyword-term)))

   `(undo-tree-visualizer-default-face
     ((,madhat2r-class :foreground ,base)
      (,madhat2r-term-class :foreground ,base-term)))

   `(undo-tree-visualizer-register-face
     ((,madhat2r-class :foreground ,comp)
      (,madhat2r-term-class :foreground ,comp-term)))

   `(undo-tree-visualizer-unmodified-face
     ((,madhat2r-class :foreground ,var)
      (,madhat2r-term-class :foreground ,var-term)))

   `(custom-link
     ((,madhat2r-class :foreground ,comp :underline t)
      (,madhat2r-term-class :foreground ,comp-term :underline t))))

  (custom-theme-set-variables
   'madhat2r
   `(ansi-color-names-vector [,bg4 ,red ,green ,yellow ,blue ,magenta ,cyan ,base])))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'madhat2r)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; madhat2r-theme.el ends here
