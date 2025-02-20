#lang sicp
(#%require "util.rkt")

;; exercise 1.30
(display "exercise 1.30\n")

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))

(define (next x)
  (+ 2 x))
(define (f x)
  (- x 2))


(print-eval-verify (sum-iter f 1 next 20) (sum f 1 next 20))
(print-eval-verify (sum-iter f 9 next 97) (sum f 9 next 97))

