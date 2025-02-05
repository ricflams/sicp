#lang sicp
(#%require "util.rkt")

;; exercise 1.18
(display "exercise 1.18\n")


(#%require "math.rkt")

(define (mult a b)
  (define (double x) (* x 2))
  (define (halve x) (/ x 2))
  (define (iterate a b sum)
    (cond ((= b 0) sum)
	  ((even? b) (iterate (double a) (halve b) sum))
	  (else (iterate a (- b 1) (+ sum a)))))
  (iterate a b 0))

(print-eval (mult 5 7))
(print-eval (mult 6 8))
(print-eval (mult 7 9))
