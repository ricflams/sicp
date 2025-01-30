#lang sicp

;; exercise 1.17
(display "exercise 1.17")
(newline)

(define (mult a b)
  (define (double x) (* x 2))
  (define (halve x) (/ x 2))
  (cond ((= b 0) 0)
	((even? b) (mult (double a) (halve b)))
	(else (+ a (mult a (- b 1))))))

(mult 5 7)
(mult 6 8)
(mult 7 9)
