(buffer-name)

(buffer-file-name)
; use C-u C-x C-e to obtain the return value in the current buffer, instead of ; in the echo area. 

(current-buffer)
; buffer itsself is returned, not just its name

(other-buffer)

(switch-to-buffer (other-buffer))
; do it visually

(set-buffer (other-buffer))
; only switch the attention of the computer program to a different buffer. 

(buffer-size)


(defun mark-whole-buffer ()
  "Put point at beginning and mark at end of buffer."
  (interactive)
  (push-mark (point))
  (push-mark (point-max))
  (goto-char (point-min)))
; mark the whole buffer as a  region


(defun append-to-buffer (buffer start end)
  "Append to specified buffer the text of the region.
It is inserted into that buffer before its point.

When calling from a program, give three arguments:
a buffer or the name of one, and two character numbers
specifying the portion of the current buffer to be copied."
  (interactive "BAppend to buffer: \nr")
; B: ask for the name of the buffer
; \n: separate the first part of the argument from the second part
; r: bind the two argument that follows in the argument list to the values of 
; point and mark. 
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (set-buffer (get-buffer-create buffer))
; get-buffer-create either gets the named buffer or creates one with the given
; name. 
      (insert-buffer-substring oldbuf start end))))

; exercise
(defun count-number-words (buffer start end)
  (interactive "BWhat buffer: \nr")
  (- end start))
(count-number-words (other-buffer) (point) (mark))

(get-buffer-create "mybuffer")
(kill-buffer "mybuffer")
(generate-new-buffer "mybuffer")
; create a new buffer with this name regardless of whether the same buffer has 
; been in existence. 

(int-to-string (/ 9 5.1))
(print (/ 9 5.1))

; an example from emacs news group
(defun write-to-buffer (number buffer)
"Print the quotient of dividend/divisor at the end of buffer"
  (set-buffer (if (bufferp buffer) buffer
		(get-buffer-create buffer)))

  (end-of-buffer)
  (insert (format "%s\n" number)))
(write-to-buffer (int-to-string (/ 8 3.0)) "mybuffer")

(setq animals '(iraffe gazelle lion tiger))

(defun print-elements-of-list (list buffer)
  "Print each element of LIST on a line of its own."
    (while list
      (write-to-buffer (car list) buffer)
      (setq list (cdr list)))
    (write-file "myfile" nil))

(print-elements-of-list animals "mybuffer")


; calculate grid point delta for bond that has a cash flow in 3m 11/29/2001
(defun get-gdp(rate time bpshift)
  "calculate the grid point delta"
  (let* ((price-up (expt (/ 1 (+ 1
			      (/ (+ rate (/ bpshift 10000.0))  2)))
		       (* 2 time)))
        (price-old (expt (/ 1 (+ 1
			      (/ (- rate (/ bpshift 10000.0))  2)))
			 (* 2 time)))
	(gdp (/ (- price-up price-old) (/ 2 1.0))))
    (- gdp)))

(debug-on-entry 'get-gdp)
(cancel-debug-on-entry 'get-gdp)


(* 4 (get-gdp 0.000896 0.25 10))


! why grep is not working. How to use grep
(defun gre(string)
  "grep string from the current buffer"
  (interactive "ssearch for string:")
  (grep string (buffer-name)))

(grep "this" (buffer-name))


(list '("abc" . "edf"))

; constructed list using cons, list and append
(let ((a "")  (c "cde") (d (list "this first")))
  (cons a c)
  (append d (cons a c)))

