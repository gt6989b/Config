;; add dirs to front of load path, recurse into sub-dirs
(let ((default-directory "~/lisp/"))
      (normal-top-level-add-subdirs-to-load-path))

;; libraries
(require 'icicles)
(icy-mode 1)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(compile-command "cd /home/n543654/src/qlib/trunk/code/drlib; rm -rf bin-linux64.opt/models.exe; make OSVER=Linux64 PARALLEL_COMPILE= -j18 OPT=; rm bin-linux64.opt/libxml*; bin-linux64.opt/models.exe")
 '(debug-on-error t)
 '(fill-column 80)
 '(inhibit-startup-screen t)
 '(kill-whole-line t)
 '(load-home-init-file t t)
 '(py-shell-name "~/bin/drip")
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(user-mail-address "dmitry.m.kreslavskiy@jpmchase.com"))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 83 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

;------------------------------------------------------------
; Global Options
;------------------------------------------------------------
;(set-default 'truncate-lines t)

;------------------------------------------------------------
; Custom Commands
;------------------------------------------------------------
;(fset 'delete-trailing-whitespace "\M-xpicture-mode\C-m\C-c\C-c")

;------------------------------------------------------------
; Customize Keys
;------------------------------------------------------------
(global-set-key [f6] 'py-shell)
(global-set-key [f7] 'compile)
(global-set-key [f8] 'shell)
(global-set-key [f9] 'delete-trailing-whitespace)
;; [f10] is menu
(global-set-key [f11] 'add-comment)
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
; Custom Packages
;------------------------------------------------------------
; woman is a nice mode for looking at manpages
(autoload 'woman "woman" "Decode and browse a man page." t)
(autoload 'woman-find-file "woman"
  "Find, decode and browse a specific man-page file." t)

(load "python-mode.el")

; location of TAGS files
(setq tags-table-list
      '("~/src/qlib/trunk/code/drlib")) ; make a dir list ("a/b" "c/d") etc

; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
(defun my-c-mode-init ()
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'statement-block-intro 4)
    (c-set-offset 'substatement 4)
    (setq indent-tabs-mode nil)
    (setq c-basic-offset 4)
    (setq truncate-lines t)
    (set-fill-column 80)
    (auto-fill-mode 1)
    (imenu-add-menubar-index)
    ;; other customizations can go here
    )
(add-hook 'c-mode-common-hook 'my-c-mode-init)

(defun my-python-mode-init ()
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'statement-block-intro 4)
    (c-set-offset 'substatement 4)
    (setq indent-tabs-mode nil)
    (setq c-basic-offset 4)
    (setq truncate-lines t)
    (set-fill-column 80)
    (auto-fill-mode 1)
    (imenu-add-menubar-index)
    ;; other customizations can go here
    )
(add-hook 'python-mode-hook 'my-python-mode-init)

(add-hook 'awk-mode-hook '(lambda () (setq c-basic-offset 4)))
(add-hook 'sh-mode-hook  '(lambda () (setq c-basic-offset 4)))
(add-hook 'lisp-mode-hook  '(lambda () (setq c-basic-offset 4)))
(add-hook 'emacs-lisp-mode-hook  '(lambda () (setq c-basic-offset 4)))
(add-hook 'news-inews-hook 'ispell-message)
(add-hook 'mail-send-hook  'ispell-message)
(add-hook 'mh-before-send-letter-hook 'ispell-message)

;;; auto-mode-alist controls file type associations
(setq auto-mode-alist (cons '("\\.bash[^/]*$" . shell-script-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.h$"         . c++-mode)          auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mkh$"       . makefile-mode)     auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mrxvtrc$"   . xrdb-mode)         auto-mode-alist))
(setq auto-mode-alist (cons '("\\.py[^/]*$"   . python-mode)       auto-mode-alist))
(setq auto-mode-alist (cons '("\\.XML$"       . xml-mode)          auto-mode-alist))
