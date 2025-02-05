#lang sicp
(#%require "util.rkt")

;; exercise 1.8
(display "exercise 1.8\n")


(define (cube-root x)
  (define (iter guess)
    (if (good-enough? guess)
	guess
	(iter (improve guess))
	))
  (define (improve guess)
    (/
     (+ (/ x (* guess guess)) (* 2 guess))
     3))
  (define (good-enough? guess)
    ;;(< (abs (- x (* guess guess))) 0.001))
    (< (abs (- x (* guess guess guess))) (/ guess 100000000)))
  (iter 1.0))

(print-eval (cube-root 27))
(print-eval (cube-root 1000000))
(print-eval (cube-root 10000000000))
;;(print-eval (cube-root 100000000000000000))
;;(print-eval (cube-root 1.2))
;;(print-eval (cube-root 0.001))

