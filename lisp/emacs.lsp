; The following can not be run under lisp mode for some mysterious reason; 
; Changing it to any other mode would make it work.  11/3/2001
; the default mode when file name does not match any of the regular expression 
; in auto-mode-alist;
(setq default-major-mode 'text-mode)
(setq default-major-mode 'fundamental-mode)
(setq default-major-mode 'lisp-interaction-mode)

; load the ada package when function ada-mode is invoked for the first time
; where ada is the filename
(autoload 'ada-mode "ada")

; add extra associations to auto-mode-alist, where '(x . y) means "make a pair
; out of x and y
(setq auto-mode-alist (cons '("\\.a$" . ada-mode) auto-mode-alist))


;  C-mode has several indent styles, including CC-MODE, GNU, K&R, BSD, 
; Stroustrup, Whitesmith and Ellemtel
; c-set-style can be used to change the style
(add-hook 'c-mode-hook
	'(lambda ()
	(c-toggle-auto-state)
	(c-toggle-hungry-state)
	(c-set-style "BSD")))
; C-c C-a: auto-newline - provide a newline auto when necessary; 
; C-c C-d: hungry-delete-key - delete all the white space up to the non-white

; Any key-bindings starting with C-c is mode-related; C-c C-h would show the 
; list under that mode


; add the path where possible library will be located
(setq load-path (append load-path (list "~zhuang/lisp")))

; this will be useful when you want to replace one of the standard packages 
; with you own
(setq load-path (cons "~zhuang/lisp" load-path))


; use the fonts and colors in lisp package font-lock.el
(add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
(add-hook 'c-mode-hook 'turn-on-font-lock)

; add a function to the c-mode. Why not working? how to debug
(defun count-functions-buffer()
  (goto-char (point-min))
  (let ((count 0))
       (while (re-search-forward "^{" nil t)
	 (setq count (1+ count))
	 (message "%d" function defined. count))))
(add-hook 'c-mode-hook 
	  '(lambda ()
	     (define-key c-mode-map "\C-cf" 'count-functions-buffer)))



; Eliminate the menu bar
(menu-bar-mode nil)

; looks not working
(setq pop-up-frames t)

; gray backgound hightlight is always on in the region between and mark; 
; But haven't seen working
(transient-mark-mode t)
