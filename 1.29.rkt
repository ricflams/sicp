#lang sicp
(#%require "util.rkt")
(#%require racket/trace)

;; exercise 1.29
(display "exercise 1.29\n")

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(print-eval (integral cube 0 1 0.01))
(print-eval (integral cube 0 1 0.001))


(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (value-at k)
    (*
     (if (= (remainder k 2) 1) 4 2)
     (f (+ a (* k h)))))
  (define (sum k)
    (if (= k n)
	0
	(+ (value-at k)	(sum (+ k 1)))))
  ;;(trace sum)
  (* (/ h 3)
     (+ (f a)
	(f b)
	(sum 1))))

(print-eval (simpsons-rule cube 0 1 100.0))
(print-eval (simpsons-rule cube 0 1 1000.0))
(print-eval (simpsons-rule cube 0 1 100))
(print-eval (simpsons-rule cube 0 1 1000))

