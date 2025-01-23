#lang sicp

;; 1.10

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))
(A 1 10)
(A 2 4)
(A 3 3)

;; (f n) = (A 0 n) = 2n
;; (g n) = (A 1 n) = (A 0 (A 1 n-1) = 2(A 1 n-1) = ... 2^(n-1)(A 1 1) = 2^(n-1)2 = 2^n 
;; (h n) = (A 2 n) = (A 1 (A 2 n-1) = (A 1 (A 2 n-.....


;; 1.11
(define (f n)
  (if (< n 3)
      n
      (+
       (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))
(display "f recursive\n")
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)
(f 6)
(f 7)

(define (f2  n)
  (define (iter a b c n)
    (if (= n 0)
	a
	(iter (+ a (* 2 b) (* 3 c)) a b (- n 1))))
  (if (< n 3)
      n
      (iter 2 1 0 (- n 2))))
(display "f iterative\n")
(f2 1)
(f2 2)
(f2 3)
(f2 4)
(f2 5)
(f2 6)
(f2 7)



;; 1.12

(define (pascal-triangle-element row col)
  ;; let both row and col start at 1
  (if (= row 1)
      1
      (cond ((= 1 col) 1)
	    ((= row col) 1)
	    (else (+ (pascal-triangle-element (- row 1) (- col 1))
		     (pascal-triangle-element (- row 1) col))))))
		     
"-"
(pascal-triangle-element 1 1)
"-"
(pascal-triangle-element 2 1)
(pascal-triangle-element 2 2)
"-"
(pascal-triangle-element 3 1)
(pascal-triangle-element 3 2)
(pascal-triangle-element 3 3)
"-"
(pascal-triangle-element 4 1)
(pascal-triangle-element 4 2)
(pascal-triangle-element 4 3)
(pascal-triangle-element 4 4)
"-"
(pascal-triangle-element 5 1)
(pascal-triangle-element 5 2)
(pascal-triangle-element 5 3)
(pascal-triangle-element 5 4)
(pascal-triangle-element 5 5)
"-"

;; 1.14
; grows linearly by amount, but n^2 with #coins

;; 1.15
;; 12.15/3^calls < 0.1 =>
;; 12.15/0.1 < 3^calls =>
;; cuberoot(121.5) < calls =>
;; a bit less than 5 < calls =>
;; calls = 5

;; 1.16

(define (square x)
  (* x x))
(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(fast-expt 2 16)
(fast-expt 2 31)
(fast-expt 2 32)
(fast-expt 2 1000)

(define (fast-expt2 b n)
  (define (iterate n a)
    (cond ((= n 0) a)
	  ((even? n) (iterate (/ n 2) (iterate (/ n 2) a)))
	  (else (iterate (- n 1) (* b a)))))
  (iterate n 1))

(fast-expt2 2 16)
(fast-expt2 2 31)
(fast-expt2 2 32)
(fast-expt2 2 1000)

;; 1.17
(define (mult a b)
  (define (double x) (* x 2))
  (define (halve x) (/ x 2))
  (cond ((= b 0) 0)
	((even? b) (mult (double a) (halve b)))
	(else (+ a (mult a (- b 1))))))

(mult 5 7)
(mult 6 8)
(mult 7 9)


  


	 


