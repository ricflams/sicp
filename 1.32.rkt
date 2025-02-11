#lang sicp
(#%require "util.rkt")

;; exercise 1.32
(display "exercise 1.32\n")


(define (accumulate-iterative combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (accumulate-recursive combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
	 (accumulate-recursive combiner null-value term (next a) next b))))

(define (sum-by-accumulator-iterative term a next b)
  (accumulate-iterative + 0 term a next b))
(define (sum-by-accumulator-recursive term a next b)
  (accumulate-recursive + 0 term a next b))

(define (product-by-accumulator-iterative term a next b)
  (accumulate-iterative * 1 term a next b))
(define (product-by-accumulator-recursive term a next b)
  (accumulate-recursive * 1 term a next b))


(define (identity x) x)
(define (inc x) (+ x 1))

(print-eval-verify (sum-by-accumulator-iterative identity 1 inc 10) 55)
(print-eval-verify (sum-by-accumulator-recursive identity 1 inc 10) 55)
(print-eval-verify (product-by-accumulator-iterative identity 1 inc 5) 120)
(print-eval-verify (product-by-accumulator-recursive identity 1 inc 5) 120)

