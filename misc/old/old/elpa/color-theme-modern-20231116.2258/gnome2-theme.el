;;; gnome2-theme.el --- gnome2 theme

;; Copyright (C) 2005, 2006  Xavier Maillard <zedek@gnu.org>
;; Copyright (C) 2005, 2006  Brian Palmer <bpalmer@gmail.com>
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
;; Port of gnome2 theme from `color-themes'

;;; Code:

(deftheme gnome2
  "gnome2 theme")

(custom-theme-set-faces
 'gnome2

 '(default ((t (:foreground "wheat" :background "darkslategrey"))))

 '(mouse ((t (:foreground "Grey"))))
 '(cursor ((t (:background "LightGray"))))
 '(border ((t (:foreground "black"))))

 '(bbdb-company ((t (:foreground "pale green"))))
 '(bbdb-name ((t (:bold t :foreground "pale green"))))
 '(bbdb-field-name ((t (:foreground "medium sea green"))))
 '(bbdb-field-value ((t (:foreground "dark sea green"))))
 '(bold ((t (:bold t))))
 '(bold-italic ((t (:italic t :bold t :foreground "beige"))))
 '(calendar-today-face ((t (:underline t))))
 '(comint-highlight-prompt ((t (:foreground "medium aquamarine"))))
 '(cperl-array-face ((t (:foreground "Yellow"))))
 '(cperl-hash-face ((t (:foreground "White"))))
 '(cperl-nonoverridable-face ((t (:foreground "SkyBlue"))))
 '(custom-button-face ((t (:underline t :foreground "MediumSlateBlue"))))
 '(custom-documentation-face ((t (:foreground "Grey"))))
 '(custom-group-tag-face ((t (:foreground "MediumAquamarine"))))
 '(custom-state-face ((t (:foreground "LightSalmon"))))
 '(custom-variable-tag-face ((t (:foreground "Aquamarine"))))
 '(diary-face ((t (:foreground "IndianRed"))))
 '(dired-face-directory ((t (:bold t :foreground "sky blue"))))
 '(dired-face-permissions ((t (:foreground "aquamarine"))))
 '(dired-face-flagged ((t (:foreground "tomato"))))
 '(dired-face-marked ((t (:foreground "light salmon"))))
 '(dired-face-executable ((t (:foreground "green yellow"))))
 '(fringe ((t (:background "darkslategrey"))))
 '(highlight ((t (:background "PaleGreen" :foreground "DarkGreen"))))
 '(highline-face ((t (:background "SeaGreen"))))
 '(holiday-face ((t (:background "DimGray"))))
 '(hyper-apropos-hyperlink ((t (:bold t :foreground "DodgerBlue1"))))
 '(hyper-apropos-documentation ((t (:foreground "LightSalmon"))))
 '(info-header-xref ((t (:foreground "DodgerBlue1" :bold t))))
 '(info-menu-5 ((t (:underline t))))
 '(info-node ((t (:underline t :bold t :foreground "DodgerBlue1"))))
 '(info-xref ((t (:bold t :foreground "DodgerBlue1"))))
 '(isearch ((t (:background "sea green"))))
 '(italic ((t (:italic t))))
 '(menu ((t (:foreground "wheat" :background "darkslategrey"))))
 '(mode-line ((t (:background "dark olive green" :foreground "wheat"))))
 '(mode-line-buffer-id ((t (:background "dark olive green" :foreground "beige"))))
 '(mode-line-mousable ((t (:background "dark olive green" :foreground "yellow green"))))
 '(mode-line-mousable-minor-mode ((t (:background "dark olive green" :foreground "wheat"))))
 '(region ((t (:background "dark cyan" :foreground "cyan"))))
 '(secondary-selection ((t (:background "Aquamarine" :foreground "SlateBlue"))))
 '(show-paren-match-face ((t (:bold t :background "Aquamarine" :foreground "steel blue"))))
 '(show-paren-mismatch-face ((t (:background "Red" :foreground "White"))))
 '(underline ((t (:underline t))))
 '(widget-field-face ((t (:foreground "LightBlue"))))
 '(widget-inactive-face ((t (:foreground "DimGray"))))
 '(widget-single-line-field-face ((t (:foreground "LightBlue"))))
 '(w3m-anchor-face ((t (:bold t :foreground "DodgerBlue1"))))
 '(w3m-arrived-anchor-face ((t (:bold t :foreground "DodgerBlue3"))))
 '(w3m-header-line-location-title-face ((t (:foreground "beige" :background "dark olive green"))))
 '(w3m-header-line-location-content-face ((t (:foreground "wheat" :background "dark olive green"))))
 '(woman-bold-face ((t (:bold t))))
 '(woman-italic-face ((t (:foreground "beige"))))
 '(woman-unknown-face ((t (:foreground "LightSalmon"))))
 '(zmacs-region ((t (:background "dark cyan" :foreground "cyan")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'gnome2)

;;; gnome2-theme.el ends here
