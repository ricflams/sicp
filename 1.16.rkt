#lang sicp
(#%require "util.rkt")

;; exercise 1.16
(display "exercise 1.16\n")


(#%require "math.rkt")

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(print-eval (fast-expt 2 16))
(print-eval (fast-expt 2 31))
(print-eval (fast-expt 2 32))
(print-eval (fast-expt 2 1000))

(define (fast-expt-iterative b n)
  (define (iterate b n a)
    (cond ((= n 0) a)
	  ((even? n) (iterate (square b) (/ n 2) a))
	  (else (iterate b (- n 1) (* b a)))))
  (iterate b n 1))

(print-eval (fast-expt-iterative 2 16))
(print-eval (fast-expt-iterative 2 31))
(print-eval (fast-expt-iterative 2 32))
(print-eval (fast-expt-iterative 2 1000))

