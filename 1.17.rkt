#lang sicp
(#%require "util.rkt")

;; exercise 1.17
(display "exercise 1.17\n")


(define (mult a b)
  (define (double x) (* x 2))
  (define (halve x) (/ x 2))
  (cond ((= b 0) 0)
	((even? b) (mult (double a) (halve b)))
	(else (+ a (mult a (- b 1))))))

(print-eval (mult 5 7))
(print-eval (mult 6 8))
(print-eval (mult 7 9))
