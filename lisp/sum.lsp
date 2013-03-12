; summation test in lisp 
; How to calculate the square root, or arbitrary expotencial

(defun simple-sum(n)
  (interactive "n enter n: ")
  (let ((total 0) (i 1))
    (while (< i (1+ n))
      (setq total (+ total i))
      (setq i (1+ i)))
    total))


; recursive depth can has an upper bond of 39
(defun simple-sum-recursive(n)
  (interactive "n enter n: ")
  (let ((total 0))
    (if (= n 1)
	1
      (setq total (+ n (simple-sum-recursive (1- n))))
    total)))
(simple-sum-recursive 39)


(defun square-sum(n)
  (interactive "n enter n: ")
  (let ((total 0) (i 1))
    (while (< i (1+ n))
      (setq total (+ total (/ 1.0 (* i i))))
      (setq i (1+ i)))
    (setq total (* 6 total))))
(square-sum 10000)

(defun sum(n choice)
  (interactive "nEnter n:  \nnEnter a choice: ")
  (cond ((= choice 1)
	 (simple-sum n))
	((= choice 2)
	 (square-sum n))
	(t (message "choice not avaiable"))))
(sum 100 1)


(defun  sqr(real_num)
  (interactive "nEnter a real number: ")
  (* real_num real_num))
(sqr 3.1415971)


; this does not work, why?
(defun  convest-list()
  ((let  (nlist (list 1 2 3 4))
   (mapcar ((lambda (n) (* n n)) nlist)))))


(convest-list)


((lambda (n) (* n n)) 49)
   
