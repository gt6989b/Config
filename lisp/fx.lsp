(defun calc-polynomial(x coef)
  "calculate the polynomial of a0+a1*x+a2*x^2+...+an*x^n"
  (if (not (listp coef))
      (setq coef (list coef)))
  (cond ((null coef) 0)
	(t (+ (car coef) (* x (calc-polynomial x (cdr coef)))))))

(defun cumulative-normal(x)
  (let* ((gamma 0.2316419)
    (pai 3.1415169)
    (expv (exp (- (/ (* x x) 2.0))))
    (sqrp (sqrt (* 2 pai)))
    (k (/ 1 (+ 1 (* x gamma))))
    (tmp (calc-polynomial k (list 0 0.31938153 -0.356563782 1.78147793  -1.821255978 1.330274429))))
    (- 1 (/ (* tmp expv) sqrp))))

;(defun test-normal(hwide n k)
;  (let ((x 0) (i k) (value))
;    (if (
;      (setq value (cumulative-normal (* k (/ hwide n))))
;      (cons value (test-normal hwide n (1+ k))))
      
	
(cumulative-normal)

(calc-polynomial 4 (list 1 1 1 1 1))


