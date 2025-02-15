#lang sicp

;;(#%provide even?)
;;(define (even? n)
;;  (= (remainder n 2) 0))

(#%provide square)
(define (square x)
  (* x x))

(#%provide cube)
(define (cube x) (* x x x))

(#%provide average)
(define (average x y)
  (/ (+ x y ) 2))
