#lang sicp
(#%require "util.rkt")

;; exercise 1.1
(display "exercise 1.1\n")

(print-eval-verify
 10
 10)

(print-eval-verify
 (+ 5 3 4)
 12)

(print-eval-verify
 (- 9 1)
 8)

(print-eval-verify
 (/ 6 2)
 3)

(print-eval-verify
 (+ (* 2 4) (- 4 6))
 6)

(define a 3)

(define b (+ a 1))

(print-eval-verify
 (+ a b (* a b))
 19)

(print-eval-verify
 (= a b)
 #f)

(print-eval-verify
 (if (and (> b a) (< b (* a b)))
     b
     a)
 4)

(print-eval-verify
 (cond ((= a 4) 5)
       ((= b 4) (+ 5 7 a))
       (else 25))
 15)

(print-eval-verify
 (+ 2 (if (> b a) b a))
 6)

(print-eval-verify
 (* (cond ((> a b) a)
	  ((< a b) b)
	  (else -1))
    (+ a 1))
 16)
