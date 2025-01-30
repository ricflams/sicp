#lang sicp

;; exercise 1.27
(display "exercise 1.27")
(newline)

;; Bring in the prime-test from exercise 1.21
(#%require "expmod.rkt")
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


(define (is-congruent n)
  (define (is-congruent-with n a)
    (= (expmod a n n) a))
  (define (iterate n a)
    (cond ((= a 0) true)
	  ((is-congruent-with n a) (iterate n (- a 1)))
	  (else false)))
  (iterate n (- n 1)))

(define (test n)
  (display n)
  (if (is-congruent n)
      (if (prime? n)
	  (display " is congruent and prime")
	  (display " is congruent BUT NOT PRIME !!!"))
      (display " is not congruent"))
  (newline))


(define (test-range n count)
  (test n)
  (if (> count 0)
      (test-range (+ n 2) (- count 1))))

(test-range 1 10)
(test-range 10001 10)
(test 561)
(test 1105)
(test 1729)
(test 2465)
(test 2821)
(test 6601)


