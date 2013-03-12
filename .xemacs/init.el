(setq load-path (cons (expand-file-name "/home/n543654/lisp") load-path))
(setq exec-path (append exec-path (list "/home/n543654/bin")))

(defvar running-xemacs (string-match "XEmacs" emacs-version))

;------------------------------------------------------------
;;; --- Func-menu ---
;------------------------------------------------------------
;;; func-menu is a package that scans your source file for function
;;; definitions and makes a menubar entry that lets you jump to any
;;; particular function definition by selecting it from the menu.  The
;;; following code turns this on for all of the recognized languages.
;;; Scanning the buffer takes some time, but not much.
;;;
;;; Send bug reports, enhancements etc to:
;;; David Hughes <ukchugd@ukpmr.cs.philips.nl>
;;;
(cond (running-xemacs
       (require 'func-menu)
       (define-key global-map 'f8 'function-menu)
       (add-hook 'find-file-hooks 'fume-add-menubar-entry)
       (define-key global-map "\C-cl" 'fume-list-functions)
       (define-key global-map "\C-cg" 'fume-prompt-function-goto)

       ;; The Hyperbole information manager package uses (shift button2) and
       ;; (shift button3) to provide context-sensitive mouse keys.  If you
       ;; use this next binding, it will conflict with Hyperbole's setup.
       ;; Choose another mouse key if you use Hyperbole.
       (define-key global-map '(shift button3) 'mouse-function-menu)

       ;; For descriptions of the following user-customizable variables,
       ;; type C-h v <variable>
       (setq fume-max-items 25
             fume-fn-window-position 3
             fume-auto-position-popup t
             fume-display-in-modeline-p t
             fume-menubar-menu-location "File"
             fume-buffer-name "*Function List*"
             fume-no-prompt-on-valid-default nil)
       ))

;------------------------------------------------------------
; Begin scroll on mouse wheel
;------------------------------------------------------------
(cond (running-xemacs
       (define-key global-map 'button4
	 '(lambda (&rest args)
	    (interactive)
	    (let ((curwin (selected-window)))
	      (select-window (car (mouse-pixel-position)))
	      (scroll-down 5)
	      (select-window curwin)
	      )))
       (define-key global-map [(shift button4)]
	 '(lambda (&rest args)
	    (interactive)
	    (let ((curwin (selected-window)))
	      (select-window (car (mouse-pixel-position)))
	      (scroll-down 1)
	      (select-window curwin)
	      )))
       (define-key global-map [(control button4)]
	 '(lambda (&rest args)
	    (interactive)
	    (let ((curwin (selected-window)))
	      (select-window (car (mouse-pixel-position)))
	      (scroll-down)
	      (select-window curwin)
	      )))

       (define-key global-map 'button5
	 '(lambda (&rest args)
	    (interactive)
	    (let ((curwin (selected-window)))
	      (select-window (car (mouse-pixel-position)))
	      (scroll-up 5)
	      (select-window curwin)
	      )))
       (define-key global-map [(shift button5)]
	 '(lambda (&rest args)
	    (interactive)
	    (let ((curwin (selected-window)))
	      (select-window (car (mouse-pixel-position)))
	      (scroll-up 1)
	      (select-window curwin)
	      )))
       (define-key global-map [(control button5)]
	 '(lambda (&rest args)
	    (interactive)
	    (let ((curwin (selected-window)))
	      (select-window (car (mouse-pixel-position)))
	      (scroll-up)
	      (select-window curwin)
	      )))
       ))
;------------------------------------------------------------
; End scroll on mouse wheel
;------------------------------------------------------------

;------------------------------------------------------------
; Customize Keys
;------------------------------------------------------------

(global-set-key [f7] 'compile)
(global-set-key [f8] 'eshell)
(global-set-key [f9] 'delete-trailing-spaces)
(global-set-key [f12] 'goto-line)

(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key [home] "\M-<")
(global-set-key [end] "\M->")
(global-set-key "\M-%" 'query-replace-regexp)
(global-set-key "\M-r" 'replace-string)
;; (global-set-key "\C-x\C-r" 'recursive-edit)
;; (global-set-key "\C-xs" 'word-search-forward)

;; Make the sequence "C-x w" execute the `what-line' command,
;; which prints the current line number in the echo area.
(global-set-key "\C-xw" 'what-line)

;------------------------------------------------------------
; Miscellaneous Options and Customizations
;------------------------------------------------------------

; fonts and faces
(cond (running-xemacs
        ;; If you want the default colors, you could do this:
       ;; (setq font-lock-use-default-fonts nil)
       ;; (setq font-lock-use-default-colors t)
       ;; but I want to specify my own colors, so I turn off all
       ;; default values.
       (setq font-lock-use-default-fonts nil)
       (setq font-lock-use-default-colors nil)

       (require 'font-lock)

       ;; Mess around with the faces a bit.  Note that you have
       ;; to change the font-lock-use-default-* variables *before*
       ;; loading font-lock, and wait till *after* loading font-lock
       ;; to customize the faces.

       ;; string face is green
       (set-face-foreground 'font-lock-string-face "forest green")

       ; comments are italic and red; doc strings are italic
       ; but not small font
       ;(copy-face 'italic 'font-lock-comment-face)

       ;; Underlining comments looks terrible on tty's
       (set-face-underline-p 'font-lock-comment-face nil 'global 'tty)
       (set-face-highlight-p 'font-lock-comment-face t 'global 'tty)
       (copy-face 'font-lock-comment-face 'font-lock-doc-string-face)
       (set-face-foreground 'font-lock-comment-face "red")

       ;; function names are bold and blue
       (copy-face 'bold 'font-lock-function-name-face)
       (set-face-foreground 'font-lock-function-name-face "blue")

       ; misc. faces
       (and (find-face 'font-lock-preprocessor-face) ; 19.13 and above
            (copy-face 'bold 'font-lock-preprocessor-face))

       ;do not use italic for decrlaration
       ;(copy-face 'italic 'font-lock-type-face)
       (copy-face 'bold 'font-lock-keyword-face)

       (when (featurep 'menubar)
	 ;; Add `dired' to the File menu
	 (add-menu-button '("File") ["Edit Directory" dired t])

	 ;; Here's a way to add scrollbar-like buttons to the menubar
	 (add-menu-button nil ["Kill" kill-buffer-and-window t])
	 )
       ))

;;; etags - location of TAGS files
(setq tag-table-list
      '(
        ; some override examples
        ; ("/usr/src/public/perl/" . "/usr/src/public/perl/perl-3.0/")
        ; ("\\.el$" . "/usr/local/emacs/src/")
        ; ("/jbw/gnu/" . "/usr15/degree/stud/jbw/gnu/")
	("" . "/home/n543654/src/qlib/trunk/code/drlib")
        ))

;;; various options
(font-lock-mode)
(setq fill-column 80)    ;; default text-mode line length
(setq scroll-step 1)
(setq font-lock-maximum-decoration t) ;; syntax highlighting most

;;; defaults
(setq compile-command "make OSVER=Linux64 OPT= PARALLEL_COMPILE= -C /home/n543654/src/qlib/trunk/code/drlib -j12")
(setq-default find-grep-options "-s")

;;; For C mode
(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode    "cc-mode" "C Editing Mode" t)
(autoload 'objc-mode "cc-mode" "Objective-C Editing Mode" t)
(autoload 'java-mode "cc-mode" "Java Editing Mode" t)

(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'statement-block-intro 4)
  (c-set-offset 'substatement 4)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (set-fill-column 80)
  (auto-fill-mode 1)
  ;; other customizations can go here
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(add-hook 'awk-mode-hook '(lambda () (setq c-basic-offset 4)))
(add-hook 'sh-mode-hook  '(lambda () (setq c-basic-offset 4)))
(add-hook 'lisp-mode-hook  '(lambda () (setq c-basic-offset 4)))
(add-hook 'emacs-lisp-mode-hook  '(lambda () (setq c-basic-offset 4)))
(add-hook 'python-mode-hook '(lambda () (setq c-basic-offset 4)))
(add-hook 'news-inews-hook 'ispell-message)
(add-hook 'mail-send-hook  'ispell-message)
(add-hook 'mh-before-send-letter-hook 'ispell-message)

;;; auto-mode-alist controls file type associations
(setq auto-mode-alist (cons '("/\\.bash[^/]*$"  . shell-script-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("/\\.h$"          . c++-mode)          auto-mode-alist))
(setq auto-mode-alist (cons '("/\\.mkh$"        . makefile-mode)     auto-mode-alist))
(setq auto-mode-alist (cons '("/\\.mrxvtrc$"    . xrdb-mode)         auto-mode-alist))
(setq auto-mode-alist (cons '("/\\.py[^/]*$"    . python-mode)       auto-mode-alist))
(setq auto-mode-alist (cons '("/\\.XML$"        . xml-mode)          auto-mode-alist))
;;; For ksh mode, should be at the end of auto-mode-alist
(autoload 'bash-mode "ksh-mode" "Major mode for editing sh Scripts." t)

;------------------------------------------------------------
; Custom Commands
;------------------------------------------------------------
(fset 'delete-trailing-spaces "\M-xpicture-mode\C-m\C-c\C-c")

;***************************************
; Load Customized Options Menu Settings
;***************************************
(cond
 ((and (string-match "XEmacs" emacs-version)
       (boundp 'emacs-major-version)
       (or (and
            (= emacs-major-version 19)
            (>= emacs-minor-version 14))
           (>= emacs-major-version 20))
       (fboundp 'load-options-file))
  (load-options-file "/home/n543654/.xemacs/custom.el")))
;; ============================
;; End of Options Menu Settings

(if (featurep 'xemacs) ;; error in ediff-init.el
    (eval-after-load "ediff-init"
      '(if (equal ediff-coding-system-for-write 'emacs-internal)
           (setq ediff-coding-system-for-write 'escape-quoted))))
