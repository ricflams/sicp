#lang sicp
(#%require "util.rkt")
(#%require "math.rkt")

;; exercise 1.31
(display "exercise 1.31\n")

;; First iteratively

(define (product-iterative term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))

(define (identity x) x)

(define (factorial-iterative n)
  (product-iterative identity 1 inc n))

(print-eval (factorial-iterative 1))
(print-eval (factorial-iterative 3))
(print-eval (factorial-iterative 5))



(define (pi-part x)
  (/ (* (- x 1) (+ x 1)) (square x)))
(define (pi-next x)
  (+ x 2))
(define (pi-iterative n)
  (* 4 (product-iterative pi-part 3.0 pi-next n)))

(print-eval (pi-iterative 10))
(print-eval (pi-iterative 100))
(print-eval (pi-iterative 10000))


;; Then recursively

(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product-recursive term (next a) next b))))

(define (factorial-recursive n)
  (product-recursive identity 1 inc n))

(print-eval (factorial-recursive 1))
(print-eval (factorial-recursive 3))
(print-eval (factorial-recursive 5))

(define (pi-recursive n)
  (* 4 (product-recursive pi-part 3.0 pi-next n)))

(print-eval (pi-recursive 10))
(print-eval (pi-recursive 100))
(print-eval (pi-recursive 10000))
