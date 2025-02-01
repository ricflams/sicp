#lang sicp

;; exercise 1.10
(display "exercise 1.10")
(newline)

(#%require "expt.rkt")

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))
(A 1 10)
(A 2 4)
(A 3 3)

(define (verify f1 f2 n)
  (define (verify-values a b)
    (display
     (if (= a b)
	 "true: "
	 "NOT TRUE: "))
    (display a)
    (display " = ")
    (display b)
    (newline))
  (verify-values (f1 n) (f2 n)))
      

;;    (f n)
;; := (A 0 n)
;;  = (* 2 n)
(define (f n)
  (A 0 n))
(define (ff n)
  (* 2 n))
(verify f ff 1)
(verify f ff 2)
(verify f ff 3)
(verify f ff 7)
(verify f ff 10)

;;    (g n)
;; := (A 1 n)
;;  = (A 0 (A 1 n-1))
;;  = (A 0 (A 0 (A 1 n-2)))
;;  = (A 0 (A 0 (A 0 (A 1 n-3)))
;;  = (A 0 (A 0 ..... (A 0 (A 1 1)))))  n times
;;  = (A 0 (A 0 ..... (A 0 2))))
;;  = (A 0 (A 0 ..... 2*2)))
;;  = 2*2*2.....*2
;;  = 2^n 
(define (g n)
  (A 1 n))
(define (gg n)
  (expt 2 n))
(verify g gg 1)
(verify g gg 2)
(verify g gg 3)
(verify g gg 7)
(verify g gg 10)

;;    (h n)
;; := (A 2 n)
;;  = (A 1 (A 2 n-1))
;;  = (A 1 (A 1 (A 2 n-2))
;;  = (A 1 (A 1 (A 1 (A 2 n-3))
;;  = (A 1 (A 1 ..... (A 1 (A 2 1)))))  n times
;;  = (A 1 (A 1 ..... (A 1 2))))
;;  = (A 1 (A 1 ..... 2^2)))   we know from f above that (A 1 n) = 2^n
;;  = (A 1 2^2^.....^2))
;;  = 2^(2^(2^(....^2)))
;;  = 2^^n

;; Tetration - it's a power-tower
;; https://waitbutwhy.com/2014/11/1000000-grahams-number.html
(define (power-tower base power)
  (if (= power 0)
      1
      (expt base (power-tower base (- power 1)))))

(define (h n)
  (A 2 n))
(define (hh n)
  (power-tower 2 n))
(verify h hh 1)
(verify h hh 2)
(verify h hh 3)
(verify h hh 4)
;(verify h hh 5) ;; is enourmous
;; don't go higher!

