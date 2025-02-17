#lang sicp
(#%require "util.rkt")

;; exercise 1.21
(display "exercise 1.21\n")


(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (define (square x) (* x x))
    (define (divides? a b)
      (= (remainder b a) 0))
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (prime? n)
  (= (smallest-divisor n) n))

(print-eval (smallest-divisor 199))
(print-eval (smallest-divisor 1999))
(print-eval (smallest-divisor 19999))



