;;; lawrence-theme.el --- lawrence theme

;; Copyright (C) 2003 by lawrence mitchell
;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/emacs-jp/replace-colorthemes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Port of lawrence theme from `color-themes'

;;; Code:

(deftheme lawrence
  "lawrence theme")

(custom-theme-set-faces
 'lawrence

 '(default ((t (:background "black" :foreground "#00CC00"))))
 '(mouse ((t (:background "black"))))
 '(cursor ((t (:background "green"))))
 '(border ((t (:background "black"))))

 '(erc-button-face ((t (:bold t :foreground "#00CC00" :background "black"))))
 '(erc-button-mouse-face ((t (:foreground "#00CC00" :background "darkgreen"))))
 '(gnus-article-button-face ((t (:bold t :foreground "#00CC00" :background "black"))))
 '(gnus-article-mouse-face ((t (:foreground "#00CC00" :background "darkgreen"))))
 '(gnus-mouse-face ((t (:foreground "#00CC00" :background "darkgreen"))))
 '(list-matching-lines-buffer-name-face ((t (:foreground "#00CC00" :background "black" :underline t))))
 '(list-matching-lines-face ((t (:bold t :foreground "#00CC00" :background "black"))))
 '(paren-match-face ((t (:background "black" :foreground "darkgreen"))))
 '(paren-mismatch-face ((t (:foreground "#00CC00" :background "black" :strike-through t))))
 '(paren-no-match-face ((t (:background "black" :foreground "red"))))
 '(view-highlight-face ((t (:bold t :foreground "#00CC00" :background "black"))))
 '(widget-mouse-face ((t (:bold t :foreground "#00CC00" :background "black"))))

 '(Buffer-menu-buffer-face ((t (:bold t :weight bold))))
 '(bg:erc-color-face0 ((t (:background "White"))))
 '(bg:erc-color-face1 ((t (:background "black"))))
 '(bg:erc-color-face10 ((t (:background "lightblue1"))))
 '(bg:erc-color-face11 ((t (:background "cyan"))))
 '(bg:erc-color-face12 ((t (:background "blue"))))
 '(bg:erc-color-face13 ((t (:background "deeppink"))))
 '(bg:erc-color-face14 ((t (:background "gray50"))))
 '(bg:erc-color-face15 ((t (:background "gray90"))))
 '(bg:erc-color-face2 ((t (:background "blue4"))))
 '(bg:erc-color-face3 ((t (:background "green4"))))
 '(bg:erc-color-face4 ((t (:background "red"))))
 '(bg:erc-color-face5 ((t (:background "brown"))))
 '(bg:erc-color-face6 ((t (:background "purple"))))
 '(bg:erc-color-face7 ((t (:background "orange"))))
 '(bg:erc-color-face8 ((t (:background "yellow"))))
 '(bg:erc-color-face9 ((t (:background "green"))))
 '(bold ((t (:bold t :foreground "#00CC00" :background "black"))))
 '(bold-italic ((t (:italic t :bold t :slant oblique :weight semi-bold))))
 '(button ((t (:underline t))))
 '(comint-highlight-input ((t (nil))))
 '(comint-highlight-prompt ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))
 '(custom-button-face ((t (:bold t :foreground "#00CC00" :background "black"))))
 '(custom-button-pressed-face ((t (nil))))
 '(custom-changed-face ((t (:italic t :foreground "#00CC00" :background "black" :slant oblique))))
 '(custom-comment-face ((t (nil))))
 '(custom-comment-tag-face ((t (nil))))
 '(custom-documentation-face ((t (nil))))
 '(custom-face-tag-face ((t (nil))))
 '(custom-group-tag-face ((t (nil))))
 '(custom-group-tag-face-1 ((t (nil))))
 '(custom-invalid-face ((t (:foreground "#00CC00" :background "black" :strike-through t))))
 '(custom-modified-face ((t (nil))))
 '(custom-rogue-face ((t (nil))))
 '(custom-saved-face ((t (nil))))
 '(custom-set-face ((t (nil))))
 '(custom-state-face ((t (nil))))
 '(custom-variable-button-face ((t (nil))))
 '(custom-variable-tag-face ((t (nil))))
 '(erc-action-face ((t (:bold t :weight semi-bold))))
 '(erc-bold-face ((t (:bold t :weight bold))))
 '(erc-current-nick-face ((t (:bold t :foreground "LightSeaGreen" :weight semi-bold))))
 '(erc-dangerous-host-face ((t (:foreground "red"))))
 '(erc-default-face ((t (nil))))
 '(erc-direct-msg-face ((t (:foreground "IndianRed"))))
 '(erc-error-face ((t (:bold t :weight semi-bold :background "darkblue" :foreground "#00CC00"))))
 '(erc-fool-face ((t (:foreground "dim gray"))))
 '(erc-input-face ((t (:foreground "springgreen"))))
 '(erc-inverse-face ((t (:bold t :background "Darkgreen" :foreground "Black" :weight semi-bold))))
 '(erc-keyword-face ((t (:bold t :foreground "pale green" :weight bold))))
 '(erc-nick-default-face ((t (:bold t :weight semi-bold))))
 '(erc-nick-msg-face ((t (:bold t :foreground "springgreen" :weight semi-bold))))
 '(erc-notice-face ((t (:foreground "seagreen" :weight normal))))
 '(erc-pal-face ((t (:bold t :foreground "Magenta" :weight bold))))
 '(erc-prompt-face ((t (:bold t :background "lightBlue2" :foreground "Black" :weight semi-bold))))
 '(erc-timestamp-face ((t (:foreground "seagreen" :weight normal))))
 '(erc-underline-face ((t (:underline t))))
 '(fg:erc-color-face0 ((t (:foreground "White"))))
 '(fg:erc-color-face1 ((t (:foreground "black"))))
 '(fg:erc-color-face10 ((t (:foreground "lightblue1"))))
 '(fg:erc-color-face11 ((t (:foreground "cyan"))))
 '(fg:erc-color-face12 ((t (:foreground "blue"))))
 '(fg:erc-color-face13 ((t (:foreground "deeppink"))))
 '(fg:erc-color-face14 ((t (:foreground "gray50"))))
 '(fg:erc-color-face15 ((t (:foreground "gray90"))))
 '(fg:erc-color-face2 ((t (:foreground "blue4"))))
 '(fg:erc-color-face3 ((t (:foreground "green4"))))
 '(fg:erc-color-face4 ((t (:foreground "red"))))
 '(fg:erc-color-face5 ((t (:foreground "brown"))))
 '(fg:erc-color-face6 ((t (:foreground "purple"))))
 '(fg:erc-color-face7 ((t (:foreground "orange"))))
 '(fg:erc-color-face8 ((t (:foreground "yellow"))))
 '(fg:erc-color-face9 ((t (:foreground "green"))))
 '(fixed-pitch ((t (nil))))
 '(font-latex-string-face ((t (:bold t :weight semi-bold :foreground "seagreen" :background "black"))))
 '(font-latex-warning-face ((t (:bold t :weight semi-bold :background "darkblue" :foreground "#00CC00"))))
 '(font-lock-builtin-face ((t (:foreground "seagreen1"))))
 '(font-lock-comment-face ((t (:background "black" :foreground "medium spring green"))))
 '(font-lock-constant-face ((t (nil))))
 '(font-lock-doc-face ((t (:bold t :background "black" :foreground "seagreen" :weight semi-bold))))
 '(font-lock-function-name-face ((t (:bold t :foreground "#00CC00" :background "black"))))
 '(font-lock-keyword-face ((t (:bold t :background "black" :foreground "green" :underline t :weight semi-bold))))
 '(font-lock-preprocessor-face ((t (:foreground "#00ccdd"))))
 '(font-lock-string-face ((t (:bold t :background "black" :foreground "seagreen" :weight semi-bold))))
 '(font-lock-type-face ((t (nil))))
 '(font-lock-variable-name-face ((t (nil))))
 '(font-lock-warning-face ((t (:bold t :foreground "#00CC00" :background "darkblue" :weight semi-bold))))
 '(fringe ((t (:foreground "#00CC00" :background "#151515"))))
 '(gnus-cite-attribution-face ((t (:italic t :foreground "#00CC00" :background "black" :slant italic))))
 '(gnus-cite-face-1 ((t (:background "black" :foreground "springgreen"))))
 '(gnus-cite-face-10 ((t (nil))))
 '(gnus-cite-face-11 ((t (nil))))
 '(gnus-cite-face-2 ((t (:background "black" :foreground "lightseagreen"))))
 '(gnus-cite-face-3 ((t (:background "black" :foreground "darkseagreen"))))
 '(gnus-cite-face-4 ((t (:background "black" :foreground "forestgreen"))))
 '(gnus-cite-face-5 ((t (:background "black" :foreground "springgreen"))))
 '(gnus-cite-face-6 ((t (:background "black" :foreground "springgreen"))))
 '(gnus-cite-face-7 ((t (:background "black" :foreground "springgreen"))))
 '(gnus-cite-face-8 ((t (:background "black" :foreground "springgreen"))))
 '(gnus-cite-face-9 ((t (:background "black" :foreground "springgreen"))))
 '(gnus-emphasis-bold ((t (:bold t :weight semi-bold))))
 '(gnus-emphasis-bold-italic ((t (:italic t :bold t :slant italic :weight semi-bold))))
 '(gnus-emphasis-highlight-words ((t (:bold t :foreground "#00CC00" :background "black" :underline t :weight bold))))
 '(gnus-emphasis-italic ((t (:italic t :slant italic))))
 '(gnus-emphasis-strikethru ((t (nil))))
 '(gnus-emphasis-underline ((t (:underline t))))
 '(gnus-emphasis-underline-bold ((t (:bold t :underline t :weight semi-bold))))
 '(gnus-emphasis-underline-bold-italic ((t (:italic t :bold t :underline t :slant italic :weight semi-bold))))
 '(gnus-emphasis-underline-italic ((t (:italic t :underline t :slant italic))))
 '(gnus-group-mail-1-empty-face ((t (nil))))
 '(gnus-group-mail-1-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))
 '(gnus-group-mail-2-empty-face ((t (nil))))
 '(gnus-group-mail-2-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))
 '(gnus-group-mail-3-empty-face ((t (nil))))
 '(gnus-group-mail-3-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))
 '(gnus-group-mail-low-empty-face ((t (nil))))
 '(gnus-group-mail-low-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))
 '(gnus-group-news-1-empty-face ((t (nil))))
 '(gnus-group-news-1-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))
 '(gnus-group-news-2-empty-face ((t (nil))))
 '(gnus-group-news-2-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))
 '(gnus-group-news-3-empty-face ((t (nil))))
 '(gnus-group-news-3-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))
 '(gnus-group-news-4-empty-face ((t (nil))))
 '(gnus-group-news-4-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))
 '(gnus-group-news-5-empty-face ((t (nil))))
 '(gnus-group-news-5-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))
 '(gnus-group-news-6-empty-face ((t (nil))))
 '(gnus-group-news-6-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))
 '(gnus-group-news-low-empty-face ((t (nil))))
 '(gnus-group-news-low-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))
 '(gnus-header-content-face ((t (:background "black" :foreground "springgreen"))))
 '(gnus-header-from-face ((t (nil))))
 '(gnus-header-name-face ((t (nil))))
 '(gnus-header-newsgroups-face ((t (nil))))
 '(gnus-header-subject-face ((t (nil))))
 '(gnus-server-agent-face ((t (:bold t :foreground "PaleTurquoise" :weight bold))))
 '(gnus-server-closed-face ((t (:italic t :foreground "Light Steel Blue" :slant italic))))
 '(gnus-server-denied-face ((t (:bold t :foreground "Pink" :weight semi-bold))))
 '(gnus-server-offline-face ((t (:bold t :foreground "Yellow" :weight bold))))
 '(gnus-server-opened-face ((t (:bold t :foreground "Green1" :weight semi-bold))))
 '(gnus-signature-face ((t (:background "black" :foreground "springgreen" :slant normal))))
 '(gnus-splash-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))
 '(gnus-summary-cancelled-face ((t (:foreground "#00CC00" :background "black" :strike-through t))))
 '(gnus-summary-high-ancient-face ((t (nil))))
 '(gnus-summary-high-read-face ((t (nil))))
 '(gnus-summary-high-ticked-face ((t (:background "black" :foreground "seagreen"))))
 '(gnus-summary-high-undownloaded-face ((t (:bold t :foreground "LightGray" :weight bold))))
 '(gnus-summary-high-unread-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))
 '(gnus-summary-low-ancient-face ((t (nil))))
 '(gnus-summary-low-read-face ((t (nil))))
 '(gnus-summary-low-ticked-face ((t (nil))))
 '(gnus-summary-low-undownloaded-face ((t (:italic t :foreground "LightGray" :slant italic :weight normal))))
 '(gnus-summary-low-unread-face ((t (:bold t :foreground "#00CC00" :background "black" :weight bold))))
 '(gnus-summary-normal-ancient-face ((t (nil))))
 '(gnus-summary-normal-read-face ((t (nil))))
 '(gnus-summary-normal-ticked-face ((t (:bold t :foreground "#00CC00" :background "black"))))
 '(gnus-summary-normal-undownloaded-face ((t (:foreground "LightGray" :weight normal))))
 '(gnus-summary-normal-unread-face ((t (nil))))
 '(gnus-summary-selected-face ((t (:background "#101010"))))
 '(gnus-x-face ((t (:background "white" :foreground "black"))))
 '(header-line ((t (nil))))
 '(highlight ((t (:foreground "#00CC00" :background "darkgreen"))))
 '(ido-first-match-face ((t (:bold t :weight bold))))
 '(ido-indicator-face ((t (:background "red" :foreground "yellow" :width condensed))))
 '(ido-only-match-face ((t (:foreground "ForestGreen"))))
 '(ido-subdir-face ((t (:foreground "red"))))
 '(isearch ((t (:background "seagreen" :foreground "black"))))
 '(isearch-lazy-highlight-face ((t (:background "darkseagreen" :foreground "black"))))
 '(italic ((t (:italic t :foreground "#00CC00" :background "black" :slant oblique))))
 '(menu ((t (:bold t :background "black" :foreground "green" :weight semi-bold
             :box (:line-width -1 :color "#606060")))))
 '(message-cited-text-face ((t (:italic t :foreground "#00CC00" :background "black" :slant oblique))))
 '(message-header-cc-face ((t (nil))))
 '(message-header-name-face ((t (nil))))
 '(message-header-newsgroups-face ((t (:bold t :foreground "#00CC00" :background "black"))))
 '(message-header-other-face ((t (:bold t :foreground "#00CC00" :background "black"))))
 '(message-header-subject-face ((t (:bold t :foreground "#00CC00" :background "black"))))
 '(message-header-to-face ((t (:bold t :foreground "#00CC00" :background "black"))))
 '(message-header-xheader-face ((t (nil))))
 '(message-mml-face ((t (:italic t :foreground "#00CC00" :background "black" :slant oblique))))
 '(message-separator-face ((t (nil))))
 '(minibuffer-prompt ((t (:background "black" :foreground "seagreen"))))
 '(mode-line ((t (:bold t :background "#404040" :foreground "green" :weight semi-bold
                  :box (:line-width -1 :color "#606060")))))
 '(mode-line-inactive ((t (:bold t :weight semi-bold  :foreground "green"
                           :box (:line-width -1 :color "#606060") :background "#101010"))))
 '(paren-face ((t (:background "black" :foreground "darkgreen"))))
 '(paren-face-match ((t (:background "black" :foreground "springgreen"))))
 '(paren-face-mismatch ((t (:foreground "#00CC00" :background "black" :strike-through t))))
 '(paren-face-no-match ((t (:background "black" :foreground "red"))))
 '(region ((t (:background "seagreen" :foreground "black"))))
 '(scroll-bar ((t (nil))))
 '(secondary-selection ((t (:background "darkseagreen" :foreground "black"))))
 '(semantic-dirty-token-face ((t (:background "gray10"))))
 '(semantic-unmatched-syntax-face ((t (:underline "red"))))
 '(sgml-end-tag-face ((t (:foreground "seagreen"))))
 '(sgml-start-tag-face ((t (:foreground "seagreen"))))
 '(tabbar-button-face ((t (:background "black" :foreground "#00cc00"
                           :box (:line-width 2 :color "black" :style released-button)))))
 '(tabbar-default-face ((t (:background "black" :foreground "#00cc00"))))
 '(tabbar-selected-face ((t (:background "black" :foreground "springgreen"
                             :box (:line-width 2 :color "black" :style released-button)))))
 '(tabbar-separator-face ((t (:foreground "#00cc00" :background "black"
                              :box (:line-width 2 :color "black" :style pressed-button)))))
 '(tabbar-unselected-face ((t (:background "black" :foreground "seagreen"))))
 '(tool-bar ((t (:box (:line-width 1 :style released-button)))))
 '(tooltip ((t (nil))))
 '(trailing-whitespace ((t (:background "lightseagreen" :foreground "black"))))
 '(underline ((t (:foreground "#00CC00" :background "black" :underline t))))
 '(variable-pitch ((t (:underline nil :foreground "#00CC00" :background "black"))))
 '(widget-button-face ((t (:bold t :foreground "#00CC00" :background "black"))))
 '(widget-button-pressed-face ((t (nil))))
 '(widget-documentation-face ((t (nil))))
 '(widget-field-face ((t (:italic t :foreground "#00CC00" :background "black" :slant oblique))))
 '(widget-inactive-face ((t (nil))))
 '(widget-single-line-field-face ((t (nil)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'lawrence)

;;; lawrence-theme.el ends here
