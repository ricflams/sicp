#lang sicp
(#%require "util.rkt")
(#%require "math.rkt")

;; exercise 1.34
(display "exercise 1.34\n")

(define (f g)
  (g 2))

(f square)

(f (lambda (x) (* x (+ x 1))))

;; (f f)
;; would evaluate to (2 2) and cause an error:
;; application: not a procedure;
;;  expected a procedure that can be applied to arguments
;;   given: 2
;;   arguments...:
;;    2
;;   context...:

