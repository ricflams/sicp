#lang sicp
(#%require "util.rkt")
(#%require "math.rkt")

;; exercise 1.33
(display "exercise 1.33\n")

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (filtered-term x)
    (if (filter x)
	(term x)
	null-value))
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (filtered-term a) result))))
  (iter a null-value))



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

(define (identity x) x)
(define (inc x) (+ x 1))


(define (sum-of-square-of-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))

(print-eval-verify
 (sum-of-square-of-primes 3 12)
 (+ (square 3) (square 5) (square 7) (square 11)))

(print-eval-verify
 (sum-of-square-of-primes 6 17)
 (+ (square 7) (square 11) (square 13) (square 17)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-of-relative-primes n)
  (define (is-relative-prime? i)
    (= (gcd i n) 1))
  (filtered-accumulate * 1 identity 1 inc (- n 1) is-relative-prime?))

(print-eval-verify
 (product-of-relative-primes 12)
 (* 1 5 7 11)) ;; 12 has GCD>1 for 2 3 4 6 8 10

(print-eval-verify
 (product-of-relative-primes 15)
 (* 1 2 4 7 8 11 13 14)) ;; 15 has GCD>1 for 3 5 6 9 10 12



