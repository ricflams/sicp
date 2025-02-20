#lang sicp
(#%require "util.rkt")
(#%require "math.rkt")

;; exercise 1.38
(display "exercise 1.38\n")

(define (cont-frac n d k)
  (define (iter i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i) (iter (inc i))))))
  (iter 1))

;; The quotient Di follow this pattern
;;
;;          i: 1   2 3  4   5 6  7   8 9 10  11 12 ...
;;  i-1 mod 3: 0   1 2  0   1 2  0   1 2  0   1 2 ...
;;    i+1 / 3: 0   1 1  1   2 2  2   3 3  3   4 4 ...
;;         Di: 1 2*1 1  1 2*2 1  1 2*3 1  1 2*4 1 ...
;;
;; For all i-1 mod 3 that is 0 or 2 we want the value 1
;; For all i-1 mod 3 that is 1 we want the value 2 * (i+1)/3
(define (d i)
  (let ((rem (remainder (dec i) 3)))
    (cond ((= 0 rem) 1)
          ((= 1 rem) (* 2 (/ (inc i) 3)))
          ((= 2 rem) 1))))

;; The approximated value of e
(define (e-approx k)
  (+ 2
     (cont-frac (lambda (i) 1.0)
                d
                k)))

;; The real value of e
(define (e-real-val)
  (exp 1))

(print-eval (e-approx 1))
(print-eval (e-approx 2))
(print-eval (e-approx 3))
(print-eval (e-approx 4))
(print-eval (e-approx 5))
(print-eval (e-approx 6))
(print-eval (e-approx 7))
(print-eval (e-approx 8))
(print-eval (e-approx 9))
(print-eval (e-real-val)) ;; the real e

