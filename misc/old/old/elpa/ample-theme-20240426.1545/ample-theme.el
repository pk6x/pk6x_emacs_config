;;; ample-theme.el --- Calm Dark Theme for Emacs
;;
;; Filename: ample-theme.el
;; Description: Dark version of the ample themes.
;; Author: Jordon Biondo <jordonbiondo@gmail.com>
;; Created: Wed Jul 24 01:04:58 2013 (-0400)
;; Version: 0.3.0
;; Last-Updated: Mon Nov  3 11:16:26 2014 (-0500)
;;           By: Jordon Biondo
;;     Update #: 29
;; URL: https://github.com/jordonbiondo/ample-theme
;; Keywords: theme, dark
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  ample-theme is part of the ample themes, a collection of 3 themes
;;  sharing a similar pallet with a light, dark, and flat version.
;;
;;  Ample-theme is a calm dark theme with a focus on being easy on the eyes
;;  during night and day.
;;
;;  Terminals with 256 colors and up will display this theme correctly.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(deftheme ample "A smooth dark theme.")

;; Not a bad idea to define a palette...
(let ((ample/green "#6aaf50")
      (ample/dark-green "#057f40")
      (ample/blue "#00bfff")
      (ample/blue-bg "#102843")
      (ample/light-blue "#528fd1")
      (ample/lighter-blue "#68a5e9")
      (ample/orange "#dF9522")
      (ample/tan "#bdbc61")
      (ample/dark-tan "#7d7c61")
      (ample/yellow "#baba36")
      (ample/bright-yellow "#fffe0a")
      (ample/purple "#ab75c3")
      (ample/light-gray "#858585")
      (ample/gray "#757575")
      (ample/dark-gray "#656565")
      (ample/darker-gray "#454545")
      (ample/darkest-gray "#252525")
      (ample/brown "#987654")
      (ample/red "#cd5542")
      (ample/dark-red "#9d2512")

      (ample/cursor "#f57e00")
      (ample/fringe "#1f1f1f")
      (ample/region "#636363")

      (ample/rb0 "#81b0e3")
      (ample/rb1 "#a5a5a5")
      (ample/rb2 "#6190c3")
      (ample/rb3 "#959595")
      (ample/rb4 "#4170a3")
      (ample/rb5 "#757575")

      (ample/bg "#3b3b3b")
      (ample/fg "#bdbdb3"))




  ;; Set faces
  (custom-theme-set-faces
   `ample ;; you must use the same theme name here...
   `(default ((t (:foreground ,ample/fg :background ,ample/bg))))
   `(cursor  ((t (:foreground ,ample/bg :background ,ample/cursor))))
   `(fringe  ((t (:background ,ample/fringe))))
   `(link    ((t (:foreground ,ample/lighter-blue :underline t))))
   `(region  ((t (:background ,ample/region))))
   `(shadow  ((t (:foreground ,ample/light-gray))))

   ;; standard font lock
   `(font-lock-builtin-face		((t (:foreground ,ample/light-blue))))
   `(font-lock-comment-face		((t (:foreground ,ample/gray))))
   `(font-lock-comment-delimiter-face	((t (:foreground ,ample/dark-gray))))
   `(font-lock-function-name-face	((t (:foreground ,ample/green))))
   `(font-lock-keyword-face		((t (:foreground ,ample/blue))))
   `(font-lock-string-face		((t (:foreground ,ample/tan))))
   `(font-lock-preprocessor-face	((t (:foreground ,ample/purple))))
   `(font-lock-type-face		((t (:foreground ,ample/red))))
   `(font-lock-constant-face		((t (:foreground ,ample/purple))))
   `(font-lock-warning-face		((t (:foreground "red" :bold t))))
   `(font-lock-variable-name-face	((t (:foreground ,ample/yellow))))
   `(font-lock-doc-face			((t (:foreground ,ample/dark-tan))))

   ;; mode line & powerline
   `(powerline-active1   ((t (:background ,ample/darkest-gray :foreground "cornsilk4"))))
   `(powerline-active2   ((t (:background "cornsilk4" :foreground ,ample/darkest-gray ))))
   `(powerline-inactive1 ((t (:background ,ample/bg :foreground "cornsilk4"))))
   `(powerline-inactive2 ((t (:background ,ample/darker-gray :foreground "cornsilk4" ))))
   `(mode-line-inactive  ((t (:background ,ample/darker-gray :foreground "cornsilk4"))))
   `(mode-line           ((t (:background "cornsilk4" :foreground ,ample/darkest-gray))))

   `(linum ((t (:background nil :foreground ,ample/gray))))

   `(popup-tip-face ((t (:background ,ample/fg :foreground ,ample/bg))))

   `(header-line ((t (:background ,ample/fg :foreground ,ample/bg))))

   `(button  ((t (:foreground ,ample/lighter-blue :background nil :underline t))))

   ;; search
   `(isearch		((t (:background ,ample/blue :foreground ,ample/bg))))
   `(lazy-highlight	((t (:background ,ample/bg :foreground ,ample/purple :underline t))))

   ;; evil-search-highlight-persist
   `(evil-search-highlight-persist-highlight-face ((t (:background ,ample/blue :foreground ,ample/bg))))

   ;; ace-jump
   `(ace-jump-face-background ((t (:inherit font-lock-comment-face))))
   `(ace-jump-face-foreground ((t (:foreground ,ample/orange))))

   `(avy-background-face  ((t (:foreground ,ample/dark-gray :background nil))))
   `(avy-goto-char-timer-face ((t (:foreground ,ample/fg :background ,ample/dark-green))))
   `(avy-lead-face  ((t (:foreground ,ample/fg :background ,ample/dark-red))))
   `(avy-lead-face-0  ((t (:foreground ,ample/fg :background ,ample/blue))))
   `(avy-lead-face-1  ((t (:foreground ,ample/bg :background ,ample/fg))))
   `(avy-lead-face-2  ((t (:foreground ,ample/fg :background ,ample/purple))))

   `(vertical-border ((t (:background ,ample/darker-gray :foreground ,ample/darkest-gray))))

   `(hl-line ((t (:background "#1f1f1f"))))

   `(highlight-indentation-face ((t (:background ,ample/darkest-gray))))

   ;; mini buff
   `(minibuffer-prompt ((t (:foreground ,ample/bright-yellow :bold t :background nil))))


   `(compilation-error		((t (:foreground ,ample/red :bold t))))
   `(compilation-warning	((t (:foreground ,ample/orange :bold t))))
   `(compilation-info		((t (:foreground ,ample/green :bold t))))

   ;; eshell
   `(eshell-prompt ((t (:foreground ,ample/purple))))
   `(eshell-ls-directory ((t (:foreground ,ample/blue))))
   `(eshell-ls-product ((t (:foreground ,ample/orange))))
   `(eshell-ls-backup ((t (:foreground ,ample/darker-gray))))
   `(eshell-ls-executable ((t (:foreground ,ample/green))))

   ;; shell
   `(comint-highlight-prompt ((t (:foreground ,ample/green))))

   ;; term
   `(term-color-black ((t (:foreground ,ample/darkest-gray :background ,ample/darkest-gray))))
   `(term-color-red ((t (:foreground ,ample/red :background ,ample/red))))
   `(term-color-green ((t (:foreground ,ample/green :background ,ample/green))))
   `(term-color-yellow ((t (:foreground ,ample/yellow :background ,ample/yellow))))
   `(term-color-blue ((t (:foreground ,ample/blue :background ,ample/blue))))
   `(term-color-magenta ((t (:foreground ,ample/purple :background ,ample/purple))))
   `(term-color-cyan ((t (:foreground ,ample/lighter-blue :background ,ample/lighter-blue))))
   `(term-color-white ((t (:foreground ,ample/fg :background ,ample/fg))))
   `(term-default-fg-color ((t (:inherit ample/fg))))
   `(term-default-bg-color ((t (:inherit ample/bg))))

   ;; erc
   `(erc-nick-default-face ((t (:foreground ,ample/blue))))
   `(erc-my-nick-face ((t (:foreground ,ample/yellow))))
   `(erc-current-nick-face ((t (:foreground ,ample/light-blue))))
   `(erc-notice-face ((t (:foreground ,ample/green))))
   `(erc-input-face ((t (:foreground "white"))))
   `(erc-timestamp-face ((t (:foreground ,ample/darker-gray))))
   `(erc-prompt-face ((t (:foreground "#191919" :background ,ample/purple))))

   ;;undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:inherit default))))
   `(undo-tree-visualizer-default-face ((t (:inherit font-lock-comment-face))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,ample/yellow :background nil))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,ample/red :background nil))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,ample/purple :background nil))))

   ;;show paren
   `(show-paren-match ((t (:foreground nil :background ,ample/dark-gray))))
   `(show-paren-mismatch ((t (:inherit error))))

   ;; error
   `(error ((t (:foreground "red"))))

   ;; ido
   `(ido-only-match		((t (:foreground ,ample/green))))
   `(ido-first-match		((t (:foreground ,ample/blue))))
   `(ido-incomplete-regexp	((t (:foreground ,ample/red))))
   `(ido-subdir			((t (:foreground ,ample/yellow))))
   ;; flx-ido
   `(flx-highlight-face         ((t (:foreground ,ample/lighter-blue :background nil :underline nil :bold t))))

   ;;js2
   `(js2-external-variable		((t (:foreground ,ample/orange :background nil))))
   `(js2-function-param			((t (:foreground ,ample/dark-green :background nil))))
   `(js2-instance-member		((t (:foreground ,ample/purple :background nil))))
   `(js2-jsdoc-html-tag-delimiter	((t (:foreground ,ample/dark-gray :background nil))))
   `(js2-jsdoc-html-tag-name		((t (:foreground ,ample/darkest-gray :background nil))))
   `(js2-jsdoc-tag			((t (:foreground ,ample/dark-red :background nil))))
   `(js2-jsdoc-type			((t (:foreground ,ample/red :background nil))))
   `(js2-jsdoc-value			((t (:foreground ,ample/tan :background nil))))
   `(js2-private-function-call		((t (:foreground ,ample/dark-green :background nil))))
   `(js2-private-member			((t (:foreground ,ample/dark-tan :background nil))))
   `(js2-warning			((t (:foreground nil :background nil :underline ,ample/orange))))

   ;;web-mode
   `(web-mode-block-attr-name-face		((t (:foreground "#8fbc8f" :background nil))))
   `(web-mode-block-attr-value-face		((t (:inherit font-lock-string-face))))
   `(web-mode-block-comment-face		((t (:inherit font-lock-comment-face))))
   `(web-mode-block-control-face		((t (:inherit font-lock-preprocessor-face))))
   `(web-mode-block-delimiter-face		((t (:inherit font-lock-preprocessor-face))))
   `(web-mode-block-face			((t (:foreground nil :background "LightYellow1"))))
   `(web-mode-block-string-face			((t (:inherit font-lock-string-face))))
   `(web-mode-builtin-face			((t (:inherit font-lock-builtin-face))))
   `(web-mode-comment-face			((t (:inherit font-lock-comment-face))))
   `(web-mode-comment-keyword-face		((t (:foreground nil :background nil :bold t))))
   `(web-mode-constant-face			((t (:foreground ,ample/purple :background nil))))
   `(web-mode-css-at-rule-face			((t (:foreground ,ample/purple :background nil))))
   `(web-mode-css-color-face			((t (:foreground ,ample/light-blue :background nil))))
   `(web-mode-css-comment-face			((t (:inherit font-lock-comment-face))))
   `(web-mode-css-function-face			((t (:foreground ,ample/light-blue :background nil))))
   `(web-mode-css-priority-face			((t (:foreground ,ample/light-blue :background nil))))
   `(web-mode-css-property-name-face		((t (:inherit font-lock-variable-name-face))))
   `(web-mode-css-pseudo-class-face		((t (:foreground ,ample/light-blue :background nil))))
   `(web-mode-css-selector-face			((t (:foreground ,ample/blue :background nil))))
   `(web-mode-css-string-face			((t (:foreground ,ample/tan :background nil))))
   `(web-mode-current-element-highlight-face	((t (:foreground nil :background "#000000"))))
   `(web-mode-doctype-face			((t (:inherit font-lock-doc-face))))
   `(web-mode-error-face			((t (:inherit error))))
   `(web-mode-folded-face			((t (:foreground nil :background nil :underline t))))
   `(web-mode-function-call-face		((t (:inherit font-lock-function-name-face))))
   `(web-mode-function-name-face		((t (:inherit font-lock-function-name-face))))
   `(web-mode-html-attr-custom-face		((t (:inherit font-lock-comment-face))))
   `(web-mode-html-attr-equal-face		((t (:inherit font-lock-comment-face))))
   `(web-mode-html-attr-name-face		((t (:inherit font-lock-comment-face))))
   `(web-mode-html-attr-value-face		((t (:inherit font-lock-string-face))))
   `(web-mode-html-tag-bracket-face		((t (:inherit font-lock-comment-face))))
   `(web-mode-html-tag-custom-face		((t (:inherit font-lock-comment-face))))
   `(web-mode-html-tag-face			((t (:inherit font-lock-comment-face))))
   `(web-mode-javascript-comment-face		((t (:inherit font-lock-comment-face))))
   `(web-mode-javascript-string-face		((t (:inherit font-lock-string-face))))
   `(web-mode-json-comment-face			((t (:inherit font-lock-comment-face))))
   `(web-mode-json-context-face			((t (:foreground "orchid3" :background nil))))
   `(web-mode-json-key-face			((t (:foreground "plum" :background nil))))
   `(web-mode-json-string-face			((t (:inherit font-lock-string-face))))
   `(web-mode-keyword-face			((t (:inherit font-lock-keyword-face))))
   `(web-mode-param-name-face			((t (:foreground "Snow3" :background nil))))
   `(web-mode-part-comment-face			((t (:inherit font-lock-comment-face))))
   `(web-mode-part-face				((t (:foreground nil :background "LightYellow1"))))
   `(web-mode-part-string-face			((t (:inherit font-lock-string-face))))
   `(web-mode-preprocessor-face			((t (:inherit font-lock-preprocessor-face))))
   `(web-mode-string-face			((t (:inherit font-lock-string-face))))
   `(web-mode-symbol-face			((t (:foreground "gold" :background nil))))
   `(web-mode-type-face				((t (:inherit font-lock-type-face))))
   `(web-mode-variable-name-face		((t (:inherit font-lock-variable-name-face))))
   `(web-mode-warning-face			((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespace-face			((t (:foreground nil :background "DarkOrchid4"))))

   ;; helm
   `(helm-M-x-key			((t (:foreground ,ample/orange :underline nil))))
   ;;`(helm-action			((t ())))
   `(helm-bookmark-addressbook		((t (:foreground ,ample/red))))
   ;;`(helm-bookmark-directory		((t ())))
   `(helm-bookmark-file			((t (:foreground ,ample/lighter-blue))))
   `(helm-bookmark-gnus			((t (:foreground ,ample/purple))))
   `(helm-bookmark-info			((t (:foreground ,ample/green))))
   `(helm-bookmark-man			((t (:foreground ,ample/orange))))
   `(helm-bookmark-w3m			((t (:foreground ,ample/yellow))))
   `(helm-buffer-directory		((t (:foreground ,ample/green))))
   ;;`(helm-buffer-not-saved		((t ())))
   ;;`(helm-buffer-process		((t ())))
   ;;`(helm-buffer-saved-out		((t ())))
   ;;`(helm-buffer-size			((t ())))
   `(helm-candidate-number		((t (:foreground ,ample/green :background ,ample/darker-gray))))
   `(helm-ff-directory			((t (:foreground ,ample/blue))))
   `(helm-ff-executable			((t (:foreground ,ample/green))))
   `(helm-ff-file			((t (:inherit default))))
   ;;`(helm-ff-invalid-symlink		((t ())))
   `(helm-ff-prefix			((t (:foreground ,ample/red))))
   ;;`(helm-ff-symlink			((t ())))
   ;;`(helm-grep-cmd-line		((t ())))
   `(helm-grep-file			((t (:foreground ,ample/purple :underline t))))
   `(helm-grep-finish			((t (:foreground ,ample/green))))
   `(helm-grep-lineno			((t (:inherit compilation-line-number))))
   ;;`(helm-grep-match			((t ())))
   ;;`(helm-grep-running		((t ())))
   `(helm-header			((t (:foreground ,ample/bg :background ,ample/fg))))
   ;;`(helm-helper			((t ())))
   ;;`(helm-history-deleted		((t ())))
   ;;`(helm-history-remote		((t ())))
   ;;`(helm-lisp-completion-info	((t ())))
   ;;`(helm-lisp-show-completion	((t ())))
   `(helm-locate-finish			((t (:foreground ,ample/green))))
   `(helm-match				((t (:foreground ,ample/blue :background ,ample/darkest-gray))))
   `(helm-moccur-buffer			((t (:inherit compilation-info))))
   `(helm-selection			((t (:foreground ,ample/yellow :background ,ample/region :bold t))))
   `(helm-prefarg			((t (:foreground ,ample/green :bold t))))
   ;;`(helm-selection-line		((t ())))
   ;;`(helm-separator			((t ())))
   `(helm-source-header			((t (:foreground ,ample/darkest-gray :background ,ample/blue))))
   `(helm-visible-mark		        ((t (:foreground ,ample/bg :background ,ample/green))))

   ;; jabber
   `(jabber-activity-face		((t (:inherit font-lock-variable-name-face :bold t))))
   `(jabber-activity-personal-face	((t (:inherit font-lock-function-name-face :bold t))))
   `(jabber-chat-error			((t (:inherit error :bold t))))
   `(jabber-chat-prompt-foreign		((t (:foreground ,ample/green  :background nil :underline nil :bold t))))
   `(jabber-chat-prompt-local		((t (:foreground ,ample/light-blue   :background nil :underline nil :bold t))))
   `(jabber-chat-prompt-system		((t (:foreground ,ample/yellow :background nil :underline nil :bold t))))
   `(jabber-chat-text-foreign		((t (:inherit default :background nil))))
   `(jabber-chat-text-local		((t (:inherit default :bold t))))
   `(jabber-rare-time-face		((t (:foreground ,ample/purple :background nil :underline t))))
   `(jabber-roster-user-away		((t (:inherit font-lock-string-face))))
   `(jabber-roster-user-chatty		((t (:foreground ,ample/orange :background nil :bold t))))
   ;;`(jabber-roster-user-dnd		((t (:foreground "red" :background nil))))
   `(jabber-roster-user-error		((t (:inherit error))))
   `(jabber-roster-user-offline		((t (:inherit font-lock-comment-face))))
   `(jabber-roster-user-online		((t (:inherit font-lock-keyword-face :bold t))))
   `(jabber-roster-user-xa		((t (:inherit font-lock-doc-face))))
   ;;`(jabber-title-large		((t (:foreground nil :background nil :bold t))))
   ;;`(jabber-title-medium		((t (:foreground nil :background nil :bold t))))
   ;;`(jabber-title-small		((t (:foreground nil :background nil :bold t))))


   ;; rainbow delim
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,ample/rb0 :background nil))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,ample/rb1 :background nil))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,ample/rb2 :background nil))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,ample/rb3 :background nil))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,ample/rb4 :background nil))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,ample/rb5 :background nil))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,ample/rb0 :background nil))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,ample/rb1 :background nil))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,ample/rb2 :background nil))))
   `(rainbow-delimiters-unmatched-face ((t (:inherit error))))

   ;; auto complete
   `(ac-candidate-face			((t (:foreground "black" :background ,ample/fg))))
   `(ac-selection-face			((t (:foreground ,ample/fg :background ,ample/blue))))
   `(ac-candidate-mouse-face		((t (:inherit ac-selection-face))))
   `(ac-clang-candidate-face		((t (:inherit ac-candidate-face))))
   `(ac-clang-selection-face		((t (:inherit ac-selection-face))))
   `(ac-completion-face			((t (:inherit font-lock-comment-face :underline t))))
   `(ac-gtags-candidate-face		((t (:inherit ac-candidate-face))))
   `(ac-gtags-selection-face		((t (:inherit ac-selection-face))))
   `(ac-slime-menu-face			((t (:inherit ac-candidate-face))))
   `(ac-slime-selection-face		((t (:inherit ac-selection-face))))
   `(ac-yasnippet-candidate-face	((t (:inherit ac-candidate-face))))
   `(ac-yasnippet-selection-face	((t (:inherit ac-selection-face))))

   ;;`(company-echo			((t (:foreground nil :background nil))))
   ;;`(company-echo-common		((t (:foreground nil :background "firebrick4"))))
   ;;`(company-preview			((t (:foreground "wheat" :background "blue4"))))
   `(company-preview-common		((t (:inherit font-lock-comment-face))))
   ;;`(company-preview-search		((t (:foreground "wheat" :background "blue1"))))
   ;;`(company-template-field		((t (:foreground "black" :background "orange"))))
   `(company-scrollbar-bg               ((t (:foreground nil :background ,ample/darkest-gray))))
   `(company-scrollbar-fg               ((t (:foreground nil :background ,ample/dark-gray))))
   `(company-tooltip			((t (:foreground ,ample/bg :background ,ample/fg))))
   `(company-tooltip-common		((t (:foreground ,ample/blue :background ,ample/fg))))
   `(company-tooltip-common-selection	((t (:foreground ,ample/bg :background ,ample/blue))))
   `(company-tooltip-mouse		((t (:foreground nil :background ,ample/light-blue))))
   `(company-tooltip-selection		((t (:foreground ,ample/fg :background ,ample/blue))))


   ;; w3m
   ;;`(w3m-anchor			((t (:foreground "cyan" :background nil))))
   ;;`(w3m-arrived-anchor		((t (:foreground "LightSkyBlue" :background nil))))
   `(w3m-bold				((t (:foreground ,ample/blue :background nil :bold t))))
   `(w3m-current-anchor			((t (:foreground nil :background nil :underline t :bold t))))
   ;;`(w3m-form				((t (:foreground "red" :background nil :underline t))))
   ;;`(w3m-form-button			((t (:foreground "red" :background nil :underline t))))
   ;;`(w3m-form-button-mouse		((t (:foreground "red" :background nil :underline t))))
   ;;`(w3m-form-button-pressed		((t (:foreground "red" :background nil :underline t))))
   ;;`(w3m-form-inactive		((t (:foreground "grey70" :background nil :underline t))))
   ;;`(w3m-header-line-location-content ((t (:foreground "LightGoldenrod" :background "Gray20"))))
   ;;`(w3m-header-line-location-title	((t (:foreground "Cyan" :background "Gray20"))))
   ;;`(w3m-history-current-url		((t (:foreground "LightSkyBlue" :background "SkyBlue4"))))
   ;;`(w3m-image			((t (:foreground "PaleGreen" :background nil))))
   ;;`(w3m-image-anchor			((t (:foreground nil :background "dark green"))))
   ;;`(w3m-insert			((t (:foreground "orchid" :background nil))))
   `(w3m-italic				((t (:foreground ,ample/orange :background nil :underline t))))
   ;;`(w3m-session-select		((t (:foreground "cyan" :background nil))))
   ;;`(w3m-session-selected		((t (:foreground "cyan" :background nil :underline t :bold t))))
   ;;`(w3m-strike-through		((t (:foreground nil :background nil))))
   ;;`(w3m-tab-background		((t (:foreground "black" :background "white"))))
   ;;`(w3m-tab-mouse			((t (:foreground nil :background nil))))
   ;;`(w3m-tab-selected			((t (:foreground "black" :background "cyan"))))
   ;;`(w3m-tab-selected-background	((t (:foreground "black" :background "white"))))
   ;;`(w3m-tab-selected-retrieving	((t (:foreground "red" :background "cyan"))))
   ;;`(w3m-tab-unselected		((t (:foreground "black" :background "blue"))))
   ;;`(w3m-tab-unselected-retrieving	((t (:foreground "OrangeRed" :background "blue"))))
   ;;`(w3m-tab-unselected-unseen	((t (:foreground "gray60" :background "blue"))))
   `(w3m-underline			((t (:foreground ,ample/green :background nil :underline t))))


   ;; ediff
   `(ediff-current-diff-A((t (:foreground nil :background "#482828"))))
   `(ediff-current-diff-B((t (:foreground nil :background "#284828"))))
   `(ediff-current-diff-C((t (:foreground nil :background "#484828"))))
   ;;`(ediff-current-diff-Ancestor((t ())))
   `(ediff-even-diff-A			((t (:foreground nil :background "#191925"))))
   `(ediff-even-diff-B			((t (:foreground nil :background "#191925"))))
   `(ediff-even-diff-C			((t (:foreground nil :background "#191925"))))
   ;;`(ediff-even-diff-Ancestor		((t ())))

   `(diff-added             ((t (:foreground nil :background "#284828"))))
   `(diff-changed           ((t (:foreground nil :background "#484828"))))
   `(diff-removed           ((t (:foreground nil :background "#482828"))))
   `(diff-context           ((t (:foreground ,ample/gray :background nil))))
   `(diff-file-header       ((t (:foreground ,ample/bg :background "grey60" :bold t))))
   `(diff-function          ((t (:foreground ,ample/bg :background "grey50"))))
   `(diff-header            ((t (:foreground ,ample/bg :background "grey50"))))
   `(diff-hunk-header       ((t (:foreground ,ample/bg :background "grey50"))))
   `(diff-index             ((t (:foreground ,ample/bg :background "grey50"))))
   `(diff-indicator-added   ((t (:inherit diff-added))))
   `(diff-indicator-changed ((t (:inherit diff-changed))))
   `(diff-indicator-removed ((t (:inherit diff-removed))))
   `(diff-nonexistent       ((t (:foreground nil :background "grey70"))))
   `(diff-refine-added      ((t (:foreground nil :background "#649694"))))
   `(diff-refine-changed    ((t (:foreground nil :background "#8f8f40"))))
   `(diff-refine-removed    ((t (:foreground nil :background "#694949"))))

   `(ediff-fine-diff-A			((t (:foreground ,ample/fg :background "#694949"))))
   `(ediff-fine-diff-B			((t (:foreground ,ample/fg :background "#496949"))))
   `(ediff-fine-diff-C			((t (:foreground ,ample/fg :background "#696949"))))
   ;;`(ediff-fine-diff-Ancestor		((t ())))

   `(ediff-odd-diff-A			((t (:foreground nil :background "#171723"))))
   `(ediff-odd-diff-B			((t (:foreground nil :background "#171723"))))
   `(ediff-odd-diff-C			((t (:foreground nil :background "#171723"))))
   ;;`(ediff-odd-diff-Ancestor		((t ())))

   ;; man pages
   `(Man-overstrike ((t (:foreground ,ample/blue))))
   `(Man-underline ((t (:foreground ,ample/yellow))))

   ;; org
   ;;`(org-agenda-calendar-event ((t (:foreground nil :background nil))))
   ;;`(org-agenda-calendar-sexp ((t (:foreground nil :background nil))))
   ;;`(org-agenda-clocking ((t (:foreground nil :background nil))))
   ;;`(org-agenda-column-dateline ((t (:foreground nil :background nil))))
   ;;`(org-agenda-current-time ((t (:foreground nil :background nil))))
   ;;`(org-agenda-date ((t (:foreground nil :background nil))))
   ;;`(org-agenda-date-today ((t (:foreground nil :background nil))))
   ;;`(org-agenda-date-weekend ((t (:foreground nil :background nil))))
   ;;`(org-agenda-diary ((t (:foreground nil :background nil))))
   ;;`(org-agenda-dimmed-todo-face ((t (:foreground nil :background nil))))
   ;;`(org-agenda-done ((t (:foreground nil :background nil))))
   ;;`(org-agenda-filter-category ((t (:foreground nil :background nil))))
   ;;`(org-agenda-filter-tags ((t (:foreground nil :background nil))))
   ;;`(org-agenda-restriction-lock ((t (:foreground nil :background nil))))
   ;;`(org-agenda-structure ((t (:foreground nil :background nil))))
   ;;`(org-archived ((t (:foreground nil :background nil))))
   ;;`(org-beamer-tag ((t (:foreground nil :background nil))))
   ;;`(org-block ((t (:foreground nil :background nil))))
   ;;`(org-block-background ((t (:foreground nil :background nil))))
   ;;`(org-block-begin-line ((t (:foreground nil :background nil))))
   ;;`(org-block-end-line ((t (:foreground nil :background nil))))
   ;;`(org-checkbox ((t (:foreground nil :background nil))))
   ;;`(org-checkbox-statistics-done ((t (:foreground nil :background nil))))
   ;;`(org-checkbox-statistics-todo ((t (:foreground nil :background nil))))
   ;;`(org-clock-overlay ((t (:foreground nil :background nil))))
   ;;`(org-code ((t (:foreground nil :background nil))))
   ;;`(org-column ((t (:foreground nil :background nil))))
   ;;`(org-column-title ((t (:foreground nil :background nil))))
   ;;`(org-date ((t (:foreground nil :background nil))))
   ;;`(org-date-selected ((t (:foreground nil :background nil))))
   ;;`(org-default ((t (:foreground nil :background nil))))
   ;;`(org-document-info ((t (:foreground nil :background nil))))
   ;;`(org-document-info-keyword ((t (:foreground nil :background nil))))
   ;;`(org-document-title ((t (:foreground nil :background nil))))
   `(org-done ((t (:foreground ,ample/green :background nil))))
   `(org-todo ((t (:foreground ,ample/red :background nil))))
   ;;`(org-drawer ((t (:foreground nil :background nil))))
   ;;`(org-ellipsis ((t (:foreground nil :background nil))))
   ;;`(org-footnote ((t (:foreground nil :background nil))))
   ;;`(org-formula ((t (:foreground nil :background nil))))
   ;;`(org-headline-done ((t (:foreground nil :background nil))))
   `(org-hide ((t (:foreground ,ample/bg :background nil))))
   ;;`(org-latex-and-export-specials ((t (:foreground nil :background nil))))
   ;;`(org-level-1 ((t (:foreground nil :background nil))))
   ;;`(org-level-2 ((t (:foreground nil :background nil))))
   ;;`(org-level-3 ((t (:foreground nil :background nil))))
   ;;`(org-level-4 ((t (:foreground nil :background nil))))
   ;;`(org-level-5 ((t (:foreground nil :background nil))))
   ;;`(org-level-6 ((t (:foreground nil :background nil))))
   ;;`(org-level-7 ((t (:foreground nil :background nil))))
   ;;`(org-level-8 ((t (:foreground nil :background nil))))
   ;;`(org-link ((t (:foreground nil :background nil))))
   ;;`(org-list-dt ((t (:foreground nil :background nil))))
   ;;`(org-meta-line ((t (:foreground nil :background nil))))
   ;;`(org-mode-line-clock ((t (:foreground nil :background nil))))
   ;;`(org-mode-line-clock-overrun ((t (:foreground nil :background nil))))
   ;;`(org-property-value ((t (:foreground nil :background nil))))
   ;;`(org-quote ((t (:foreground nil :background nil))))
   ;;`(org-scheduled ((t (:foreground nil :background nil))))
   ;;`(org-scheduled-previously ((t (:foreground nil :background nil))))
   ;;`(org-scheduled-today ((t (:foreground nil :background nil))))
   ;;`(org-sexp-date ((t (:foreground nil :background nil))))
   ;;`(org-special-keyword ((t (:foreground nil :background nil))))
   ;;`(org-table ((t (:foreground nil :background nil))))
   ;;`(org-tag ((t (:foreground nil :background nil))))
   ;;`(org-target ((t (:foreground nil :background nil))))
   ;;`(org-time-grid ((t (:foreground nil :background nil))))
   ;;`(org-upcoming-deadline ((t (:foreground nil :background nil))))
   ;;`(org-verbatim ((t (:foreground nil :background nil))))
   ;;`(org-verse ((t (:foreground nil :background nil))))
   ;;`(org-warning ((t (:foreground nil :background nil))))


   ;; message-mode
   `(message-cited-text  ((t (:inherit font-lock-comment-face))))
   `(message-header-cc  ((t (:foreground ,ample/light-blue :background nil :bold t))))
   `(message-header-name  ((t (:foreground ,ample/orange :background nil))))
   `(message-header-newsgroups  ((t (:foreground ,ample/dark-tan :background nil :bold t))))
   `(message-header-other  ((t (:foreground ,ample/blue :background nil))))
   `(message-header-subject  ((t (:foreground ,ample/tan :background nil))))
   `(message-header-to  ((t (:foreground ,ample/yellow :background nil :bold t))))
   `(message-header-xheader  ((t (:foreground ,ample/purple :background nil))))
   `(message-mml  ((t (:foreground ,ample/dark-tan :background nil))))

   ;; gnus
   `(gnus-button				((t (:foreground nil :background nil :bold t))))
   `(gnus-cite-1				((t (:foreground "light blue" :background nil))))
   `(gnus-cite-10				((t (:foreground "plum1" :background nil))))
   `(gnus-cite-11				((t (:foreground "turquoise" :background nil))))
   `(gnus-cite-2				((t (:foreground "light cyan" :background nil))))
   `(gnus-cite-3				((t (:foreground "light yellow" :background nil))))
   `(gnus-cite-4				((t (:foreground "light pink" :background nil))))
   `(gnus-cite-5				((t (:foreground "pale green" :background nil))))
   `(gnus-cite-6				((t (:foreground "beige" :background nil))))
   `(gnus-cite-7				((t (:foreground "orange" :background nil))))
   `(gnus-cite-8				((t (:foreground "magenta" :background nil))))
   `(gnus-cite-9				((t (:foreground "violet" :background nil))))
   `(gnus-cite-attribution			((t (:foreground nil :background nil))))
   `(gnus-emphasis-bold				((t (:foreground nil :background nil :bold t))))
   `(gnus-emphasis-bold-italic			((t (:foreground nil :background nil :bold t))))
   `(gnus-emphasis-highlight-words		((t (:foreground "yellow" :background "black"))))
   `(gnus-emphasis-italic			((t (:foreground nil :background nil))))
   `(gnus-emphasis-strikethru			((t (:foreground nil :background nil))))
   `(gnus-emphasis-underline			((t (:foreground nil :background nil :underline t))))
   `(gnus-emphasis-underline-bold		((t (:foreground nil :background nil :underline t :bold t))))
   `(gnus-emphasis-underline-bold-italic	((t (:foreground nil :background nil :underline t :bold t))))
   `(gnus-emphasis-underline-italic		((t (:foreground nil :background nil :underline t))))
   `(gnus-group-mail-1				((t (:foreground ,ample/blue :background nil :bold t))))
   `(gnus-group-mail-1-empty			((t (:foreground ,ample/blue :background nil))))
   `(gnus-group-mail-2				((t (:foreground ,ample/lighter-blue :background nil :bold t))))
   `(gnus-group-mail-2-empty			((t (:foreground ,ample/lighter-blue :background nil))))
   `(gnus-group-mail-3				((t (:foreground ,ample/light-blue :background nil :bold t))))
   `(gnus-group-mail-3-empty			((t (:foreground ,ample/light-blue :background nil))))
   `(gnus-group-mail-low			((t (:foreground ,ample/yellow :background nil :bold t))))
   `(gnus-group-mail-low-empty			((t (:foreground ,ample/yellow :background nil))))
   `(gnus-group-news-1				((t (:foreground "PaleTurquoise" :background nil :bold t))))
   `(gnus-group-news-1-empty			((t (:foreground "PaleTurquoise" :background nil))))
   `(gnus-group-news-2				((t (:foreground "turquoise" :background nil :bold t))))
   `(gnus-group-news-2-empty			((t (:foreground "turquoise" :background nil))))
   `(gnus-group-news-3				((t (:foreground nil :background nil :bold t))))
   `(gnus-group-news-3-empty			((t (:foreground nil :background nil))))
   `(gnus-group-news-4				((t (:foreground nil :background nil :bold t))))
   `(gnus-group-news-4-empty			((t (:foreground nil :background nil))))
   `(gnus-group-news-5				((t (:foreground nil :background nil :bold t))))
   `(gnus-group-news-5-empty			((t (:foreground nil :background nil))))
   `(gnus-group-news-6				((t (:foreground nil :background nil :bold t))))
   `(gnus-group-news-6-empty			((t (:foreground nil :background nil))))
   `(gnus-group-news-low			((t (:foreground "DarkTurquoise" :background nil :bold t))))
   `(gnus-group-news-low-empty			((t (:foreground "DarkTurquoise" :background nil))))
   `(gnus-header-content			((t (:inherit message-header-other))))
   `(gnus-header-from				((t (:inherit message-header-other))))
   `(gnus-header-name				((t (:inherit message-header-name))))
   `(gnus-header-newsgroups			((t (:inherit message-header-newsgroups))))
   `(gnus-header-subject			((t (:inherit message-header-subject))))
   `(gnus-server-agent				((t (:foreground "PaleTurquoise" :background nil :bold t))))
   `(gnus-server-closed				((t (:foreground "LightBlue" :background nil))))
   `(gnus-server-denied				((t (:foreground "pink" :background nil :bold t))))
   `(gnus-server-offline			((t (:foreground "yellow" :background nil :bold t))))
   `(gnus-server-opened				((t (:foreground "green1" :background nil :bold t))))
   `(gnus-signature				((t (:foreground nil :background nil))))
   `(gnus-splash				((t (:foreground "#cccccc" :background nil))))
   `(gnus-summary-cancelled			((t (:foreground "yellow" :background "black"))))
   `(gnus-summary-high-ancient			((t (:foreground "SkyBlue" :background nil :bold t))))
   `(gnus-summary-high-read			((t (:foreground "PaleGreen" :background nil :bold t))))
   `(gnus-summary-high-ticked			((t (:foreground "pink" :background nil :bold t))))
   `(gnus-summary-high-undownloaded		((t (:foreground "LightGray" :background nil :bold t))))
   `(gnus-summary-high-unread			((t (:foreground nil :background nil :bold t))))
   `(gnus-summary-low-ancient			((t (:foreground "SkyBlue" :background nil))))
   `(gnus-summary-low-read			((t (:foreground "PaleGreen" :background nil))))
   `(gnus-summary-low-ticked			((t (:foreground "pink" :background nil))))
   `(gnus-summary-low-undownloaded		((t (:foreground "LightGray" :background nil))))
   `(gnus-summary-low-unread			((t (:foreground nil :background nil))))
   `(gnus-summary-normal-ancient		((t (:inherit default))))
   `(gnus-summary-normal-read			((t (:foreground ,ample/green :background nil))))
   `(gnus-summary-normal-ticked			((t (:foreground ,ample/orange :background nil))))
   `(gnus-summary-normal-undownloaded		((t (:foreground ,ample/dark-gray :background nil))))
   `(gnus-summary-normal-unread			((t (:foreground ,ample/light-blue :background nil))))
   `(gnus-summary-selected			((t (:foreground nil :background nil :underline t))))

   `(twittering-timeline-footer-face	((t (:foreground nil :background nil :inherit font-lock-function-name-face))))
   `(twittering-timeline-header-face	((t (:foreground nil :background nil :inherit font-lock-function-name-face))))
   `(twittering-uri-face		((t (:foreground nil :background nil :underline t))))
   `(twittering-username-face		((t (:foreground nil :background nil :inherit font-lock-keyword-face :underline t))))

   ;; whitespace mode
   `(whitespace-empty			((t (:foreground ,ample/gray :background "gray10"))))
   `(whitespace-hspace			((t (:foreground ,ample/gray :background "grey11"))))
   `(whitespace-indentation		((t (:foreground ,ample/gray :background "gray12"))))
   `(whitespace-line			((t (:foreground ,ample/purple :background nil))))
   `(whitespace-newline			((t (:foreground ,ample/gray :background nil))))
   `(whitespace-space			((t (:foreground ,ample/gray :background nil))))
   `(whitespace-space-after-tab		((t (:foreground ,ample/gray :background "gray13"))))
   `(whitespace-space-before-tab	((t (:foreground ,ample/gray :background "gray14"))))
   `(whitespace-tab			((t (:foreground ,ample/gray :background "grey15"))))
   `(whitespace-trailing		((t (:foreground ,ample/rb3 :background ,ample/bg :bold t))))

   ;; magit
   ;; new
   `(magit-section-heading             ((t (:foreground ,ample/blue :background nil))))
   `(magit-section-heading-selection   ((t (:foreground ,ample/lighter-blue))))
   `(magit-hash                        ((t (:foreground ,ample/purple :background nil))))
   `(magit-branch-local                ((t (:foreground ,ample/orange :background nil))))
   `(magit-branch-remote               ((t (:foreground ,ample/yellow :background nil))))

   `(magit-diff-added-highlight        ((t (:background ,ample/region :foreground ,ample/green))))
   `(magit-diff-removed-highlight      ((t (:background ,ample/region :foreground ,ample/red))))
   `(magit-diff-added                  ((t (:background nil :foreground ,ample/green))))
   `(magit-diff-removed                ((t (:background nil :foreground ,ample/red))))
   `(magit-diff-file-heading-selection ((t (:foreground ,ample/lighter-blue :inherit magit-diff-file-heading-highlight))))
   `(magit-diff-hunk-heading-selection ((t (:foreground ,ample/lighter-blue :inherit magit-diff-hunk-heading-highlight))))
   `(magit-diff-lines-heading          ((t (:background ,ample/light-blue :foreground ,ample/bg))))
   `(magit-diffstat-added              ((t (:background nil :foreground ,ample/green))))
   `(magit-diffstat-removed            ((t (:background nil :foreground ,ample/red))))

   `(magit-bisect-bad                  ((t (:foreground ,ample/red))))
   `(magit-bisect-good                 ((t (:foreground ,ample/green))))
   `(magit-bisect-skip                 ((t (:foreground ,ample/orange))))
   `(magit-blame-date                  ((t (:foreground ,ample/purple :background "grey25"))))
   `(magit-blame-hash                  ((t (:foreground ,ample/purple :background "grey25"))))
   `(magit-blame-heading               ((t (:foreground ,ample/blue :background "grey25"))))
   `(magit-blame-name                  ((t (:foreground ,ample/green :background "grey25"))))
   `(magit-blame-summary               ((t (:foreground ,ample/blue :background "grey25"))))

   `(magit-popup-argument              ((t (:foreground ,ample/red :inherit font-lock-warning-face))))
   `(magit-process-ng                  ((t (:foreground ,ample/red :bold t))))
   `(magit-process-ok                  ((t (:foreground ,ample/green))))

   `(magit-reflog-amend                ((t (:foreground ,ample/orange))))
   `(magit-reflog-checkout             ((t (:foreground ,ample/blue))))
   `(magit-reflog-cherry-pick          ((t (:foreground ,ample/green))))
   `(magit-reflog-commit               ((t (:foreground ,ample/green))))
   `(magit-reflog-merge                ((t (:foreground ,ample/green))))
   `(magit-reflog-other                ((t (:foreground ,ample/light-blue))))
   `(magit-reflog-rebase               ((t (:foreground ,ample/orange))))
   `(magit-reflog-remote               ((t (:foreground ,ample/light-blue))))
   `(magit-reflog-reset                ((t (:foreground ,ample/red))))

   `(magit-sequence-head               ((t (:foreground ,ample/light-blue))))
   `(magit-sequence-part               ((t (:foreground ,ample/orange))))
   `(magit-sequence-stop               ((t (:foreground ,ample/green))))

   ;;`(magit-signature-bad             ((t (:foreground "red" :bold t))))
   `(magit-signature-error             ((t (:foreground ,ample/red))))
   ;;`(magit-signature-expired         ((t (:foreground "orange"))))
   ;;`(magit-signature-expired-key     ((t (:inherit magit-signature-expired))))
   `(magit-signature-good              ((t (:foreground ,ample/green))))
   `(magit-signature-revoked           ((t (:foreground ,ample/purple))))
   `(magit-signature-untrusted         ((t (:foreground ,ample/light-blue))))

   ;; old
   `(magit-branch				((t (:foreground ,ample/orange :background nil))))
   `(magit-cherry-equivalent			((t (:foreground ,ample/green :background nil))))
   `(magit-cherry-unmatched			((t (:foreground ,ample/light-blue :background nil))))
   `(magit-diff-add				((t (:foreground nil :background ,ample/dark-green))))
   `(magit-diff-del				((t (:foreground nil :background ,ample/dark-red))))
   `(magit-diff-file-header			((t (:foreground ,ample/bg :background ,ample/blue :bold t))))
   `(magit-diff-hunk-header			((t (:foreground ,ample/bg :background ,ample/blue))))
   `(magit-diff-merge-current			((t (:foreground ,ample/orange :background nil))))
   `(magit-diff-merge-diff3-separator		((t (:foreground ,ample/orange :background nil))))
   `(magit-diff-merge-proposed			((t (:foreground ,ample/orange :background nil))))
   `(magit-diff-merge-separator			((t (:foreground ,ample/orange :background nil))))
   `(magit-diff-none				((t (:foreground ,ample/fg :background ,ample/darker-gray))))
   `(magit-header				((t (:foreground ,ample/blue :background nil))))
   `(magit-item-highlight			((t (:foreground nil :background ,ample/darker-gray))))
   ;;`(magit-item-mark				((t (:foreground nil :background "darkolivegreen"))))
   ;;`(magit-key-mode-args-face			((t (:foreground "black" :background "yellow3"))))
   `(magit-key-mode-button-face			((t (:foreground ,ample/light-blue :background nil))))
   `(magit-key-mode-header-face			((t (:foreground ,ample/blue :background nil))))
   ;;`(magit-key-mode-switch-face		((t (:foreground "red" :background nil :bold t))))
   `(magit-log-author				((t (:foreground ,ample/red :background nil))))
   `(magit-log-author-date-cutoff		((t (:foreground ,ample/red :background nil :bold t))))
   `(magit-log-date				((t (:foreground ,ample/blue :background nil))))
   `(magit-log-graph				((t (:foreground "grey80" :background nil))))
   ;;`(magit-log-head-label-bisect-bad		((t (:foreground "IndianRed4" :background "IndianRed1"))))
   ;;`(magit-log-head-label-bisect-good		((t (:foreground "dark olive green" :background "light green"))))
   ;;`(magit-log-head-label-default		((t (:foreground nil :background "Grey50"))))
   ;;`(magit-log-head-label-head		((t (:foreground "White" :background "Grey20"))))
   ;;`(magit-log-head-label-local		((t (:foreground "LightSkyBlue1" :background "Grey13"))))
   ;;`(magit-log-head-label-patches		((t (:foreground "IndianRed4" :background "IndianRed1"))))
   ;;`(magit-log-head-label-remote		((t (:foreground "DarkSeaGreen2" :background "Grey11"))))
   ;;`(magit-log-head-label-tags		((t (:foreground "goldenrod4" :background "LemonChiffon1"))))
   ;;`(magit-log-message			((t (:foreground nil :background nil))))
   ;;`(magit-log-reflog-label-amend		((t (:foreground "goldenrod4" :background "LemonChiffon1"))))
   ;;`(magit-log-reflog-label-checkout		((t (:foreground "LightSkyBlue1" :background "Grey13"))))
   ;;`(magit-log-reflog-label-cherry-pick	((t (:foreground "dark olive green" :background "light green"))))
   ;;`(magit-log-reflog-label-commit		((t (:foreground "goldenrod4" :background "LemonChiffon1"))))
   ;;`(magit-log-reflog-label-merge		((t (:foreground "goldenrod4" :background "LemonChiffon1"))))
   ;;`(magit-log-reflog-label-other		((t (:foreground nil :background "Grey50"))))
   ;;`(magit-log-reflog-label-rebase		((t (:foreground "DarkSeaGreen2" :background "Grey11"))))
   ;;`(magit-log-reflog-label-remote		((t (:foreground nil :background "Grey50"))))
   ;;`(magit-log-reflog-label-reset		((t (:foreground "IndianRed4" :background "IndianRed1"))))
   `(magit-log-sha1				((t (:foreground ,ample/purple :background nil))))
   `(magit-section-title			((t (:foreground ,ample/blue :background nil))))
   `(magit-tag					((t (:foreground ,ample/blue :background nil))))
   ;;`(magit-valid-signature			((t (:foreground "PaleTurquoise" :background nil :bold t))))
   `(magit-whitespace-warning-face		((t (:foreground ,ample/bg :background "white" :bold t))))

   `(git-gutter:deleted   ((t (:foreground ,ample/red :background nil :bold t))))
   `(git-gutter:modified  ((t (:foreground ,ample/purple :background nil :bold t))))
   `(git-gutter:separator ((t (:foreground ,ample/orange :background nil :bold t))))
   `(git-gutter:unchanged ((t (:foreground ,ample/yellow :background nil))))

   `(highlight-indentation-current-column-face ((t (:foreground nil :background ,ample/gray))))
   `(highlight-indentation-face                ((t (:foreground nil :background ,ample/darkest-gray))))

   ;; trailing whitespace
   `(trailing-whitespace ((t (:background "white" :bold t))))

   ;; auctex
   `(font-latex-bold-face                 ((t (:inherit bold :foreground ,ample/dark-green))))
   `(font-latex-doctex-documentation-face ((t (:background unspecified))))
   `(font-latex-doctex-preprocessor-face  ((t (:inherit (font-latex-doctex-documentation-face font-lock-preprocessor-face)))))
   `(font-latex-italic-face               ((t (:inherit italic :foreground ,ample/dark-green))))
   `(font-latex-math-face                 ((t (:foreground ,ample/purple))))
   ;;`(font-latex-sectioning-0-face       ((t (:inherit font-latex-sectioning-1-face :height 1.1))))
   ;;`(font-latex-sectioning-1-face       ((t (:inherit font-latex-sectioning-2-face :height 1.1))))
   ;;`(font-latex-sectioning-2-face       ((t (:inherit font-latex-sectioning-3-face :height 1.1))))
   ;;`(font-latex-sectioning-3-face       ((t (:inherit font-latex-sectioning-4-face :height 1.1))))
   ;;`(font-latex-sectioning-4-face       ((t (:inherit font-latex-sectioning-5-face :height 1.1))))
   `(font-latex-sectioning-5-face         ((t (:foreground ,ample/red :weight bold))))
   `(font-latex-sedate-face               ((t (:foreground ,ample/brown))))
   `(font-latex-slide-title-face          ((t (:inherit font-lock-type-face :weight bold :height 1.2))))
   `(font-latex-string-face               ((t (:inherit font-lock-string-face))))
   ;;`(font-latex-subscript-face          ((t (:height 0.8))))
   ;;`(font-latex-superscript-face        ((t (:height 0.8))))
   `(font-latex-verbatim-face             ((t (:foreground ,ample/tan))))
   `(font-latex-warning-face              ((t (:inherit font-lock-warning-face))))
   `(TeX-error-description-error          ((t (:inherit error :bold t))))
   `(TeX-error-description-tex-said       ((t (:foreground ,ample/lighter-blue))))
   `(TeX-error-description-warning        ((t (:foreground ,ample/orange :bold t))))

   ;; guide-key
   `(guide-key/prefix-command-face    ((t (:foreground ,ample/green))))
   `(guide-key/highlight-command-face ((t (:foreground ,ample/blue))))
   `(guide-key/key-face               ((t (:foreground ,ample/gray))))

   ;; custom
   `(custom-button                  ((t (:foreground nil :background nil))))
   `(custom-button-mouse            ((t (:foreground nil :background nil))))
   `(custom-button-pressed          ((t (:foreground nil :background nil))))
   `(custom-button-pressed-unraised ((t (:foreground ,ample/purple :background nil))))
   `(custom-button-unraised         ((t (:foreground nil :background nil))))
   `(custom-changed                 ((t (:foreground ,ample/red :background nil))))
   `(custom-comment                 ((t (:foreground ,ample/bg :background ,ample/yellow))))
   `(custom-comment-tag             ((t (:foreground ,ample/fg :background nil))))
   `(custom-documentation           ((t (:foreground nil :background nil))))
   `(custom-face-tag                ((t (:foreground ,ample/light-blue :background nil))))
   `(custom-group-subtitle          ((t (:foreground nil :background nil :bold t))))
   `(custom-group-tag               ((t (:foreground ,ample/light-blue :background nil :bold t))))
   `(custom-group-tag-1             ((t (:foreground ,ample/yellow :background nil :bold t))))
   `(custom-invalid                 ((t (:foreground ,ample/bg :background ,ample/red))))
   `(custom-link                    ((t (:inherit button))))
   `(custom-modified                ((t (:foreground ,ample/red :background nil))))
   `(custom-rogue                   ((t (:foreground ,ample/yellow :background ,ample/bg))))
   `(custom-saved                   ((t (:foreground nil :background nil :underline t))))
   `(custom-set                     ((t (:foreground ,ample/fg :background ,ample/dark-gray))))
   `(custom-state                   ((t (:foreground ,ample/green :background nil))))
   `(custom-themed                  ((t (:foreground ,ample/red :background nil))))
   `(custom-variable-button         ((t (:foreground nil :background nil :underline t :bold t))))
   `(custom-variable-tag            ((t (:foreground ,ample/light-blue :background nil :bold t))))
   `(custom-visibility              ((t (:inherit button))))

   `(neo-banner-face              ((t (:foreground ,ample/light-blue :background nil :bold t))))
   `(neo-button-face              ((t (:foreground nil :background nil))))
   `(neo-dir-link-face            ((t (:foreground ,ample/blue :background nil))))
   `(neo-expand-btn-face          ((t (:foreground ,ample/fg :background nil))))
   `(neo-file-link-face           ((t (:foreground ,ample/fg :background nil))))
   `(neo-header-face              ((t (:foreground ,ample/fg :background nil))))
   `(neo-root-dir-face            ((t (:foreground ,ample/green :background nil :bold t))))
   `(neo-vc-added-face            ((t (:foreground ,ample/green :background nil))))
   `(neo-vc-conflict-face         ((t (:foreground ,ample/orange :background nil))))
   `(neo-vc-default-face          ((t (:foreground ,ample/fg :background nil))))
   `(neo-vc-edited-face           ((t (:foreground ,ample/yellow :background nil))))
   `(neo-vc-ignored-face          ((t (:foreground ,ample/dark-gray :background nil))))
   `(neo-vc-missing-face          ((t (:foreground ,ample/red :background nil))))
   `(neo-vc-needs-merge-face      ((t (:foreground ,ample/orange :background nil))))
   `(neo-vc-needs-update-face     ((t (:foreground nil :background nil :underline t))))
   `(neo-vc-removed-face          ((t (:foreground ,ample/purple :background nil))))
   `(neo-vc-unlocked-changes-face ((t (:foreground ,ample/red :background "Blue"))))
   `(neo-vc-unregistered-face     ((t (:foreground nil :background nil))))
   `(neo-vc-up-to-date-face       ((t (:foreground ,ample/fg :background nil))))

   ;; realgud
   `(realgud-overlay-arrow1         ((t (:foreground ,ample/green))))
   `(realgud-overlay-arrow2         ((t (:foreground ,ample/bright-yellow))))
   `(realgud-overlay-arrow3         ((t (:foreground ,ample/orange))))
   `(realgud-bp-enabled-face        ((t (:inherit error))))
   `(realgud-bp-disabled-face       ((t (:foreground ,ample/dark-gray))))
   `(realgud-bp-line-enabled-face   ((t (:box (:color ,ample/red)))))
   `(realgud-bp-line-disabled-face  ((t (:box (:color "grey50")))))
   `(realgud-line-number            ((t (:foreground ,ample/yellow))))
   `(realgud-backtrace-number       ((t (:foreground ,ample/yellow, :weight bold))))

   ;; widget
   `(widget-field  ((t (:foreground ,ample/fg :background ,ample/dark-gray))))

   ) ;; end of custom-theme-set-faces

  (custom-theme-set-variables
   'ample
   `(ansi-color-names-vector
     [,ample/darker-gray ,ample/red ,ample/green ,ample/yellow ,ample/blue ,ample/purple ,ample/lighter-blue ,ample/fg])))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(defun ample-theme()
  "Apply the ample-theme."
  (interactive)
  (load-theme 'ample t))


(provide-theme 'ample)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ample-theme.el ends here
