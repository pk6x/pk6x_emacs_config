;;; Summary --- Beginning of my Emacs init file: Commentary:
;; .
;; .
;; .
;; .
;; .
;; .
;; ---------------------------------------------------- Startup ----------------------------------------------------
;; Load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Startup windowing
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(setq next-line-add-newlines nil)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(split-window-horizontally)

;; Disable mouse middle-click
(global-unset-key [mouse-2])

;; Report matched paren
(show-paren-mode t)

(defadvice show-paren-function (after show-matching-paren-offscreen activate)
  (interactive)
  (let* ((cb (char-before (point)))
	 (matching-text (and cb (char-equal (char-syntax cb) ?\))
                             (blink-matching-open))))))

;; Colors
(add-to-list 'default-frame-alist '(cursor-color . "green yellow"))

;; Auto-save
(defun full-auto-save ()
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (buffer-file-name) (buffer-modified-p))
          (basic-save-buffer)))))
(add-hook 'auto-save-mode-hook 'full-auto-save)

;; Disable extra buffers
(add-hook 'minibuffer-setup-hook
	  '(lambda ()
	     (let ((buffer "*Messages*"))
	       (and (get-buffer buffer)
		    (kill-buffer buffer)))))
(add-hook 'minibuffer-setup-hook
	  '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
		    (kill-buffer buffer)))))
(add-hook 'minibuffer-setup-hook
	  '(lambda ()
             (let ((buffer "*Flycheck error messages*"))
               (and (get-buffer buffer)
		    (kill-buffer buffer)))))
(add-hook 'minibuffer-setup-hook
	  '(lambda ()
	     (let ((buffer "*lsp-log*"))
	       (and (get-buffer buffer)
		    (kill-buffer buffer)))))
(add-hook 'minibuffer-setup-hook
	  '(lambda ()
	     (let ((buffer "*Compile-Log*"))
	       (and (get-buffer buffer)
		    (kill-buffer buffer)))))
(add-hook 'minibuffer-setup-hook 
	  '(lambda ()
             (let ((buffer "*compilation*"))
               (and (get-buffer buffer)
		    (kill-buffer buffer)))))
(add-hook 'minibuffer-setup-hook 
	  '(lambda ()
             (let ((buffer "*grep*"))
               (and (get-buffer buffer)
		    (kill-buffer buffer)))))
(add-hook 'minibuffer-setup-hook 
	  '(lambda ()
             (let ((buffer "*Ido Completions*"))
               (and (get-buffer buffer)
		    (kill-buffer buffer)))))


;; hs-minor-mode
(add-hook 'c++-mode-hook 'hs-minor-mode)
(defun folded-all()
  (interactive)
  (hs-minor-mode)
  (hs-hide-all)
  )
(add-hook 'c++-mode-hook 'folded-all)
(setq hs-hide-comments-when-hiding-all nil)

;; Added syntax class
;; (modify-syntax-entry ?– "w")
;; (modify-syntax-entry ?+ "w")
;; (modify-syntax-entry ?- "w")
;; (modify-syntax-entry ?* "w")
;; (modify-syntax-entry ?& "w")
;; (modify-syntax-entry ?_ "w")
;; (modify-syntax-entry ?# "w")
;; (modify-syntax-entry ?, "w")
;; (global-superword-mode)

;; Insert time of the day
;; (defun insert-timeofday ()
;; (interactive "*")
;; (insert (format-time-string "---------------- %a, %d %b %y: %I:%M%p"))
;; )

;; ---------------------------------------------------- End of Startup ----------------------------------------------------

;; ------------------------------------- From Casey Muratori (C/C++ style and compilation ------------------
;; To determine the underlying operating system
(setq system-aquamacs (featurep 'aquamacs))
(setq system-linux (featurep 'x))
(setq system-win32 (not (or system-aquamacs system-linux)))

(setq todo-file "w:/me_handmadehero/code/todo.txt")
(setq log-file "w:/me_handmadehero/code/log.txt")

(setq compilation-directory-locked nil)

(when system-win32 
  (setq build-script "build.bat")
  )

(when system-aquamacs 
  (cua-mode 0) 
  (osx-key-mode 0)
  (tabbar-mode 0)
  (setq mac-command-modifier 'meta)
  (setq x-select-enable-clipboard t)
  (setq aquamacs-save-options-on-quit 0)
  (setq special-display-regexps nil)
  (setq special-display-buffer-names nil)
  (define-key function-key-map [return] [13])
  (setq mac-command-key-is-meta t)
  (scroll-bar-mode nil)
  (setq mac-pass-command-to-system nil)
  (setq build-script "./build.macosx")
  )

(when system-linux
  (setq build-script "./build.linux")
  (display-battery-mode 1)
  )

(load-library "view")
;; (require 'cc-mode)
(require 'ido)
(require 'compile)
(ido-mode t)

(defun ediff-setup-windows (buffer-A buffer-B buffer-C control-buffer)
  (ediff-setup-windows-plain buffer-A buffer-B buffer-C control-buffer)
  )

(setq ediff-window-setup-function 'ediff-setup-windows)
(setq ediff-split-window-function 'split-window-horizontally)

;; C/C++ mode handling

;; Unique comments style
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(make-face 'font-lock-important-face)
(make-face 'font-lock-study-face)
(mapc (lambda (mode)
	(font-lock-add-keywords
	 mode
	 '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
	   ("\\<\\(NOTE\\)" 1 'font-lock-note-face t)
	   ("\\<\\(STUDY\\)" 1 'font-lock-study-face t)
	   ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
	   )))
      fixme-modes)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)
(modify-face 'font-lock-study-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-important-face "Yellow" nil nil t nil t nil nil)

;; Accepted file extensions and their appropriate modes
(setq auto-mode-alist
      (append
       '(("\\.cpp$"    . c++-mode)
	 ("\\.hin$"    . c++-mode)
	 ("\\.cin$"    . c++-mode)
	 ("\\.inl$"    . c++-mode)
	 ("\\.rdc$"    . c++-mode)
	 ("\\.h$"    . c++-mode)
	 ("\\.c$"   . c++-mode)
	 ("\\.cc$"   . c++-mode)
	 ("\\.c8$"   . c++-mode)
	 ("\\.txt$" . indented-text-mode)
	 ("\\.emacs$" . emacs-lisp-mode)
	 ("\\.gen$" . gen-mode)
	 ("\\.ms$" . fundamental-mode)
	 ("\\.m$" . objc-mode)
	 ("\\.mm$" . objc-mode)
	 ) auto-mode-alist)
      )

;; C++ indentation style
(defconst c-default-style
  '((c-electric-pound-behavior   . nil)
    (c-tab-always-indent         . t)
    (c-comment-only-line-offset  . 0)
    (c-hanging-braces-alist      . ((class-open)
				    (class-close)
				    (enum-open)
				    (enum-close)
                                    (defun-open)
                                    (defun-close)
                                    (inline-open)
                                    (inline-close)
                                    (brace-list-open)
                                    (brace-list-close)
                                    (brace-list-intro)
                                    (brace-list-entry)
                                    (block-open)
                                    (block-close)
                                    (substatement-open)
                                    (statement-case-open)
                                    (class-open)))
    (c-hanging-colons-alist      . ((inher-intro)
                                    (case-label)
                                    (label)
                                    (access-label)
                                    (access-key)
                                    (member-init-intro)))
    (c-cleanup-list              . (scope-operator
                                    list-close-comma
                                    defun-close-semi))
    (c-offsets-alist             . ((arglist-close         .  c-lineup-arglist)
                                    (label                 . -4)
                                    (access-label          . -4)
                                    (substatement-open     .  0)
				    (statement-case-intro  .  4)
                                    (statement-block-intro .  c-lineup-for)
                                    (case-label            .  4)
                                    (block-open            .  4)
                                    (inline-open           .  0)
                                    (topmost-intro-cont    .  0)
                                    (knr-argdecl-intro     . -4)
                                    (brace-list-open       .  0)
                                    (brace-list-intro      .  4)))
    (c-echo-syntactic-information-p . nil))
  ;; "Big Fun C++ Style."
  )

(defun big-fun-c-hook ()
  ;; Set my style for the current buffer
  (c-add-style "BigFun" c-default-style t)
  
  ;; 4-space tabs
  (setq tab-width 4
        indent-tabs-mode nil)
  
  ;; Newline indents, semi-colon wont
  (setq c-hanging-semi&comma-criteria '((lambda () 'stop)))
  
  ;; Additional style stuff
  (c-set-offset 'member-init-intro '++)
  
  ;; No hungry backspace
  (c-toggle-auto-hungry-state -1)
  
  ;; Abbrevation expansion
  (abbrev-mode 1)
  
  ;; Format the given file as a header file
  (defun header-format ()
    (interactive)
    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    (insert "#ifndef ")
    (push-mark)
    (insert BaseFileName)
    (upcase-region (mark) (point))
    (pop-mark)
    (insert "_H\n")
    (insert "/* ........................................................................\n")
    (insert "   $File: $\n")
    (insert "   $Date: $\n")
    (insert "   $Revision: $\n")
    (insert "   $Creator: $\n")
    (insert "   $Notice: $\n")
    (insert "   ........................................................................ */\n")
    (insert "\n")
    (insert "#define ")
    (push-mark)
    (insert BaseFileName)
    (upcase-region (mark) (point))
    (pop-mark)
    (insert "_H\n")
    (insert "#endif")
    )
  
  ;; Format the given file as a source file
  (defun source-format ()
    (interactive)
    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
    (insert "/* ........................................................................\n")
    (insert "   $File: $\n")
    (insert "   $Date: $\n")
    (insert "   $Revision: $\n")
    (insert "   $Creator: $\n")
    (insert "   $Notice: $\n")
    (insert "   ........................................................................ */\n")
    )
  
  (cond ((file-exists-p buffer-file-name) t)
	((string-match "[.]hin" buffer-file-name) (source-format))
	((string-match "[.]cin" buffer-file-name) (source-format))
	((string-match "[.]h" buffer-file-name) (header-format))
	((string-match "[.]cpp" buffer-file-name) (source-format))
	)
  
  (defun find-corresponding-file ()
    ;; "Find the file that corresponds to this one."
    (interactive)
    (setq CorrespondingFileName nil)
    (setq BaseFileName (file-name-sans-extension buffer-file-name))
    (if (string-match "\\.c" buffer-file-name)
	(setq CorrespondingFileName (concat BaseFileName ".h")))
    (if (string-match "\\.h" buffer-file-name)
	;; (if (file-exists-p (concat BaseFileName ".c")) (setq CorrespondingFileName (concat BaseFileName ".c"))
	(setq CorrespondingFileName (concat BaseFileName ".cpp")))
    (if (string-match "\\.hin" buffer-file-name)
	(setq CorrespondingFileName (concat BaseFileName ".cin")))
    (if (string-match "\\.cin" buffer-file-name)
	(setq CorrespondingFileName (concat BaseFileName ".hin")))
    (if (string-match "\\.cpp" buffer-file-name)
	(setq CorrespondingFileName (concat BaseFileName ".h")))
    (if CorrespondingFileName (find-file CorrespondingFileName)
      (error "Unable to find a corresponding file"))
    )
  (defun find-corresponding-file-other-window ()
    ;; "Find the file that corresponds to this one."
    (interactive)
    (find-file-other-window buffer-file-name)
    (find-corresponding-file)
    (other-window -1))
  (define-key c++-mode-map [f4] 'find-corresponding-file)
  (define-key c++-mode-map [f3] 'find-corresponding-file-other-window)
  ;; (define-key c++-mode-map [C-tab] 'indent-region)
  (define-key c++-mode-map "\C-y" 'indent-for-tab-command)
  (define-key c++-mode-map "^[  " 'indent-region)
  )

(add-hook 'c-mode-common-hook 'big-fun-c-hook)

;; Hook the added syntax class
;; (add-hook 'c++-mode-hook 'superword-mode)
;; (add-hook 'c++-mode-hook (lambda () (modify-syntax-entry ?- "w")))
;; (add-hook 'c++-mode-hook (lambda () (modify-syntax-entry ?+ "w")))
;; (add-hook 'c++-mode-hook (lambda () (modify-syntax-entry ?- "w")))
;; (add-hook 'c++-mode-hook (lambda () (modify-syntax-entry ?* "w")))
;; (add-hook 'c++-mode-hook (lambda () (modify-syntax-entry ?& "w")))
;; (add-hook 'c++-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
;; (add-hook 'c++-mode-hook (lambda () (modify-syntax-entry ?# "w")))

;; C/C++ compilation
(setq compilation-context-lines 0)
(setq compilation-error-regexp-alist
      (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
	    compilation-error-regexp-alist)
      )

;; (add-to-list 'compilaton-error-regexp-alist 'amgun-devenv)
;; (add-to-list 'compilation-error-regexp-alist-alist '(amgun-devenv
;; "*\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) :
;; \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
;; 2 3 nil (4)))

(defun find-project-directory-recursive ()
  ;; "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p build-script) t
    (cd "../")
    (find-project-directory-recursive))
  )

(defun lock-compilation-directory ()
  ;; "The compilation process should NOT hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked.")
  )

(defun unlock-compilation-directory ()
  ;; "The compilation process SHOULD hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is roaming.")
  )

(defun find-project-directory ()
  ;; "Find the project directory."
  (interactive)
  (setq find-project-from-directory default-directory)
  (switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
    (cd find-project-from-directory)
    (find-project-directory-recursive)
    (setq last-compilation-directory default-directory))
  )

(defun make-without-asking ()
  ;; "Make the current build."
  (interactive)
  (if (find-project-directory) (compile build-script))
  (other-window 1)
  )
(define-key global-map [f5] 'make-without-asking)

;; Commands
(set-variable 'grep-command "grep -irHn ")
(when system-win32
  (set-variable 'grep-command "findstr -s -n -i -l "))

;; Prevent consecutive marks activating bloody `transient-mark-mode'.
;; (defadvice set-mark-command (after no-bloody-t-m-m activate)
;; (if transient-mark-mode (setq transient-mark-mode nil)))

;; Prevent mouse commands activiating bloody `transient-mark-mode'.
(defadvice mouse-set-region-1 (after no-bloody-t-m-m activate)
  (if transient-mark-mode (setq transient-mark-mode nil))
  )

;; Replace a string without moving point
(defun replace-string (FromString ToString)
  (interactive "sReplace: \nsReplace: %s With: ")
  (save-excursion
    (replace-string FromString ToString))
  )

;; Perform a replace-string in the current region
(defun replace-in-region (old-word new-word)
  (interactive "sReplace: \nsReplace: %s With: ")
  (save-excursion (save-restriction
		    (narrow-to-region (mark) (point))
		    (beginning-of-buffer)
		    (replace-regexp-in-region old-word new-word)))
  )
;; -------------------------------- End of From Casey Muratori (C/C++ style and compilation -------------------

;; ---------------------------------------------------- Packages ----------------------------------------------------
;; Enabling melpa package archiver

;; latest development version from GNU-Devel ELPA
;; (add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(use-package try :ensure t)
(use-package which-key :ensure t :config (which-key-mode)
  )

;; lsp-mode
;; To cycle between function prototypes (overloaded functions in c++) in lsp
;; after typing in the function prototype, in the echo section it will display how many
;; prototypes are there for the same function signature, e.g. (1/4) function ...
;; Press M-n to cycle to the next prototype (2/4) or M-p to the previous one (1/4) etc
(require 'lsp-mode)
(use-package lsp-mode
  :ensure t
  :hook ((c++-mode . lsp))
  )
;; (use-package lsp-ui)
;; (use-package lsp-treemacs)
;; (setq lsp-ui-doc-enable 1)
;; (setq lsp-ui-sideline-show-diagnostics 1)
;; (setq lsp-ui-sideline-update-mode 1)
;; (setq lsp-ui-sideline-show-hover nil)
(setq lsp-lens-enable 1)
(setq lsp-headerline-breadcrumb-enable 1)
(setq lsp-modeline-code-actions-enable 1)
(setq lsp-modeline-diagnostics-enable 1)
(setq +format-with-lsp nil)
(setq lsp-enable-on-type-formatting nil)
(setq lsp-format-region nil)
(setq lsp-format-buffer nil)
(setq lsp-clients-clangd-args
      '("-j=2"
        "--background-index"
        "--clang-tidy"
        "--completion-style=bundled"
        "--pch-storage=memory"
        "--header-insertion=never"
        "--header-insertion-decorators=0"))
;; (add-hook 'c++-mode-hook #'lsp)

;; Ligature
;; This assumes you've installed the package via MELPA.
(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; Soft-charchoal theme
;; (load-theme 'soft-charcoal)

(require 'flycheck)
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  )

;; Rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Dimmer
(require 'dimmer)
(dimmer-configure-which-key)
(dimmer-configure-helm)
(dimmer-mode t)
(setq dimmer-fraction 0.65)

;; C++ compilation
;; (defun code-compile ()
;; (interactive)
;; (unless (file-exists-p "Makefile")
;; (set (make-local-variable 'compile-command)
;; (let ((file (file-name-nondirectory buffer-file-name)))
;; (format "%s -o %s %s"
;; (if  (equal (file-name-extension file) "cpp") "clang++ -o *.exe"  )
;; (file-name-sans-extension file)
;; file)))
;; (compile compile-command)))

;; (global-set-key [f5] 'code-compile)
;; ---------------------------------------------------- End of Packages ----------------------------------------------------

;; ---------------------------------------------------- Keybindings ----------------------------------------------------
(define-key global-map "\ea" 'c-beginning-of-statement)
(define-key global-map "\eb" 'ido-switch-buffer)
(define-key global-map "\eB" 'ido-switch-buffer-other-window)
(define-key global-map "\ec" 'copy-word)
(define-key global-map "\ed" 'kill-word)
(define-key global-map "\eD" 'delete-char)
(define-key global-map "\ee" 'c-end-of-statement)
(define-key global-map "\ef" 'move-previous-line-beginning-of-text)
(define-key global-map "\eg" 'backward-char )
(define-key global-map "\eG" 'find-file)
(define-key global-map "\eh" 'forward-char)
(define-key global-map "\eH" 'find-file-other-window)
(define-key global-map "\ei" 'other-window)
(define-key global-map "\ej" 'move-next-line-beginning-of-text)
(define-key global-map "\eJ" 'imenu)
(define-key global-map "\ek" 'delete-backward-char)
(define-key global-map "\eK" 'previous-buffer)
(define-key global-map "\el" 'copy-line)
(define-key global-map "\eL" 'cut-line)
(define-key global-map "\em" 'forward-word)
(define-key global-map "\eM" 'downcase-word)
(define-key global-map "\en" 'next-error)
(define-key global-map "\eN" 'previous-error)
(define-key global-map "\eo" 'end-of-line-and-indent-new-line)
(define-key global-map "\eO" 'beginning-of-line-and-indent-new-line)
(define-key global-map "\ep" 'paste-with-indentation-on)
(define-key global-map "\eq" 'keyboard-escape-quit)
(define-key global-map "\er" 'query-replace)
(define-key global-map "\eR" 'replace-in-region)
(define-key global-map "\es" 'isearch-forward)
(define-key global-map "\eS" 'save-buffer)
(define-key global-map "\et" 'move-beginning-of-line)
(define-key global-map "\eu" 'undo)
(define-key global-map "\eU" 'upcase-word)
(define-key global-map "\ev" 'backward-word)
(define-key global-map "\e9" 'recenter-top-bottom)
(define-key global-map "\ew" 'kill-region)
(define-key global-map "\eX" 'kill-this-buffer)
(define-key global-map "\ey" 'move-end-of-line)
(define-key global-map "\ez" 'open-line-with-indent)
(define-key global-map "\e " 'set-mark-command)
(define-key global-map "\e/" 'copy-region-as-kill)
(define-key global-map "\e[" 'hs-show-all)
(define-key global-map "\e]" 'hs-hide-block)
(define-key global-map "\e;" 'next-blank-line)
(define-key global-map "\e'" 'previous-blank-line)
(define-key global-map "\e:" 'hs-show-block)
(define-key global-map "\e\""'hs-hide-all)
(define-key global-map "\e<" 'end-of-buffer)
(define-key global-map "\e>" 'beginning-of-buffer)
(define-key global-map "\e." 'exchange-point-and-mark)
(define-key global-map "\e," 'isearch-forward-symbol-at-point)
(define-key global-map "\e?" 'comment-line)
(define-key global-map [f7] 'header-format)
(define-key global-map [f6] 'source-format)
(define-key global-map [f9] 'first-error)
(define-key global-map [f10] 'previous-error)
(define-key global-map [f11] 'next-error)
;; (define-key global-map [backtab] 'indent-recursively)
(define-key global-map [M-down] 'scroll-other-window)
(define-key global-map [M-up] 'scroll-other-window-down)
(define-key global-map [C-right] 'forward-word)
(define-key global-map [C-left] 'backward-word)
(define-key global-map [C-up] 'previous-blank-line)
(define-key global-map [C-down] 'next-blank-line)
(define-key global-map [home] 'forward-sentence)
(define-key global-map [end] 'backward-sentence)
(define-key global-map [pgup] 'forward-page)
(define-key global-map [pgdown] 'backward-page)
(define-key global-map [C-M-n] 'forward-list)
(define-key global-map [C-M-p] 'backward-lis)
(define-key global-map [C-M-k] 'kill-sexp)
(define-key global-map [C-M-h] 'c-mark-function)
(define-key global-map [?\C-\M- ] 'mark-sexp)

(defun previous-blank-line ()
  ;; "Moves to the previous line containing nothing but whitespace."
  (interactive)
  (search-backward-regexp "^[ \t]*\n")
  )

(defun next-blank-line ()
  ;; "Moves to the next line containing nothing but whitespace."
  (interactive)
  (forward-line)
  (search-forward-regexp "^[ \t]*\n")
  (forward-line -1)
  )

;; Open line with indent
(defun open-line-with-indent(&optional arg)
  (interactive)
  (open-line 1)
  (next-line arg)
  (c-indent-command arg)
  (previous-line arg)
  (move-end-of-line arg)
  )

;; Move lines to begnning of text
(defun move-next-line-beginning-of-text ()
  (interactive)				
  (next-line)				
  (beginning-of-line-text)
  (c-indent-command)
  )

(defun move-previous-line-beginning-of-text ()
  (interactive)
  (previous-line)
  (beginning-of-line-text)
  (c-indent-command)
  )

;; Indent Recursively
(defun indent-recursively ()
  (interactive)
  (c-indent-command)
  (next-line)
  )

;; Yank with indentation
(defun paste-with-indentation-on ()
  (interactive)
  (c-indent-command)
  (yank)
  )

;; Insert new line below current line
(defun end-of-line-and-indent-new-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (c-indent-command)
  )

;; Insert new line above current line
(defun beginning-of-line-and-indent-new-line ()
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (previous-line)
  (c-indent-command)
  )

;; Copy words and lines
(defun get-point (symbol &optional arg)
  (funcall symbol arg)
  (point)
  )

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end)))
  )

(defun paste-to-mark (&optional arg)
  (unless (eq arg 1)
    (if (string= "shell-mode" major-mode)
        (comint-next-prompt 25535)
      (goto-char (mark)))
    (yank))
  )

;; Copy word
(defun copy-word (&optional arg)
  (interactive "P")
  (copy-thing 'forward-word 'backward-word arg)
  ;;(paste-to-mark arg)
  )

;; Copy Line
(defun copy-line (&optional arg)
  (interactive "P")
  (copy-thing 'beginning-of-line-text 'end-of-line arg)
  ;; (paste-to-mark arg)
  )

;; Cut line
(defun cut-line ()
  (interactive)
  (beginning-of-line-text)
  (kill-line)
  (c-indent-command)
  )
;; ---------------------------------------------------- End of Keybindings ----------------------------------------------------

;; ---------------------------------------------------- Modeline configuration ----------------------------------------------------
;; (setq-default mode-line-format nil)
(setq-default mode-line-format
	      '("%e"
		"  "
		(:eval (format "%s" (buffer-file-name)))
		"  "
		(:eval (format "%s" (propertize (symbol-name major-mode) 'face 'bold)))
		"  "
		mode-line-position
		)
	      )
;; (add-to-list 'global-mode-string '(" %i"))
;; ---------------------------------------------------- End of Modeline configuration ----------------------------------------------------

;; ---------------------------------------------------- Auto-generated by Emacs  ----------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(auto-save-interval 1)
 '(auto-save-list-file-prefix nil)
 '(auto-save-timeout 1)
 '(auto-save-visited-interval 1)
 '(auto-save-visited-mode t)
 '(auto-show-mode t t)
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(custom-enabled-themes '(soft-charcoal))
 '(custom-safe-themes
   '("b06115632754daeade91a7ddfbbe8a149c8cefd8af4099e5da6fd722b6d95dc2" default))
 '(delete-auto-save-files nil)
 '(delete-old-versions 'other)
 '(ede-project-directories '("w:/"))
 '(electric-pair-mode t)
 '(fit-frame-to-buffer-margins '(5 nil nil nil))
 '(gdb-many-windows t)
 '(global-auto-revert-mode t)
 '(global-display-line-numbers-mode t)
 '(global-hl-line-mode 1)
 '(imenu-auto-rescan t)
 '(imenu-auto-rescan-maxout 500000)
 '(inhibit-startup-screen t)
 '(kept-new-versions 5)
 '(kept-old-versions 5)
 '(kill-buffer "*Messages*")
 '(kill-whole-line nil)
 '(lsp-clangd-version "19.1.2")
 '(make-backup-file-name-function 'ignore)
 '(make-backup-files nil)
 '(max-lisp-eval-depth 10000)
 '(message-log-max nil)
 '(mosue-wheel-follow-mouse nil)
 '(next-line-add-newlines nil)
 '(package-selected-packages
   '(dimmer posframe treemacs lsp-treemacs lsp-ui color-theme-modern soft-charcoal-theme ligature auto-complete company rainbow-delimiters visual-fill-column fill-column-indicator which-key use-package flycheck))
 '(safe-local-variable-values
   '((eval setq flycheck-clang-include-path
	   (list
	    (expand-file-name "code/")))))
 '(scroll-conservatively 10000)
 '(scroll-margin 1)
 '(scroll-preserve-screen-position 1)
 '(scroll-setup 1)
 '(split-window-horizontally nil)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(truncate-partial-width-windows nil)
 '(undo-limit 2000000)
 '(undo-strong-limit 4000000))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#232323" :foreground "#c2c2c2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 113 :width normal :foundry "outline" :family "Cascadia Code"))))
 '(company-tooltip ((t (:background "gray26" :foreground "#b19779"))))
 '(company-tooltip-annotation ((t (:foreground "gray46"))))
 '(company-tooltip-common ((t (:background "gray36" :foreground "gray20"))))
 '(company-tooltip-common-selection ((t (:background "gray26" :foreground "#b19779"))))
 '(company-tooltip-selection ((t (:background "gray" :foreground "gray21"))))
 '(flycheck-error ((t nil)))
 '(flycheck-fringe-error ((t (:inherit error))))
 '(font-lock-doc-face ((t (:background "gray34" :foreground "#679fad"))))
 '(font-lock-string-face ((t (:foreground "#c2c2c2" :weight normal))))
 '(font-lock-type-face ((t (:foreground "#8885b2"))))
 '(header-line ((t (:inherit mode-line :background "#191919" :foreground "grey90" :box nil))))
 '(highlight ((t (:background "gray24"))))
 '(hl-line ((t (:extend t :background "gray19"))))
 '(lsp-headerline-breadcrumb-path-error-face ((t (:inherit lsp-headerline-breadcrumb-path-face :background "IndianRed2" :underline (:color "Red1" :style wave :position nil) :weight extra-bold))))
 '(lsp-headerline-breadcrumb-path-face ((t (:inherit font-lock-string-face))))
 '(lsp-headerline-breadcrumb-symbols-face ((t (:inherit font-lock-doc-face :weight normal))))
 '(mode-line ((t (:background "#202020" :foreground "#babdb6" :box nil :weight bold :height 1.3))))
 '(mode-line-active ((t (:background "chartreuse4" :foreground "black"))))
 '(mode-line-emphasis ((t nil)))
 '(mode-line-highlight ((t nil)))
 '(mode-line-inactive ((t (:background "#202020" :foreground "#808080" :box nil))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#f8f8ff"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#ffd700"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#7fffd4"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#ff8c00"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#7cfc00"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#d75fff"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#a28469"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#3b895f"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#cd96cd"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "red"))))
 '(show-paren-match ((t (:background "royal blue")))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
