; The following comments are added 8/30/2001

; 1. Debugging lisp code I: After setting
(setq debug-on-error t)
; the backtrace will be shown in a buffer when evaluate any lisp function with 
; error on it. To turn the trace off, set the following
(setq debug-on-error nil)

; 2. Debugging lisp code II: run 
(debug-on-entry 'sum)
; the on the backtrace buffer pulled up after evaluating any functioin, typing
; d repeatedly will show the current statement executed. To disable debugging, 
; run
(cancel-debug-on-entry 'sum)

; 3. Debugger list code II: run
(setq debug-on-quit t)
; and insert a (debug) into the code as follow; It is said that debug mode can
; be entered when C-g to interrupt, but haven't verified. 

(defun sum (number)
 " sum from 1, 2, upto number"
  (interactive "nGoto Percent: ")
 (let ((total 0) (tmp number))
   (while (> number 0)
     (setq total (+ total number))
     (debug)
     (setq number (1- number)))
   (message "the sumber from 1 to %d is %d" number total)))

(sum 100)

(defun goto-percent (pct)
  (interactive "nGoto Percent: ")
  (let* ((size (point-max))
	(charpos (/ (* size pct) 100)))
    (goto-char charpos)))
(goto-percent 40)


(defun zap-to-charr (arg char) 
  "Kill up to and including ARG'th occurrence of CHAR.
Goes backward if ARG is negative"
  (interactive "*p\ncZap to char: ")
  ; *: beep if the buffer if read-only
  ; p: the first argument to the function is passed the value of 
  ;    "process prefix", defaulted to 1. 
  ; c: expects to character entered. 
  (kill-region (point)
               (progn
                 (search-forward
                  (char-to-string char) nil nil arg)
                 (point))))

(zap-to-charr 1 10)
; how to represent a charater, 'c' won't work1




(defun test-let(x)
  (let* ((a 1))))

(test-let 0)
    
    

(defun get-sum(lvl)
  "print list element one by one"
  (if (not (listp lvl))
      (setq lvl (list lvl)))
  (cond ((null lvl) 0)
	(t (+ (car lvl) (get-sum (cdr lvl))))))

(get-sum (list 1 2 3 4))

(debug-on-entry 'cumulative-normal)

