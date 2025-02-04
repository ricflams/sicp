#lang sicp

;; exercise 1.5
(display "exercise 1.5")
(newline)

(define (p) (p)) ;; if evalated then p will loop forever

(define (test x y)
  (if (= x 0)
      0
      y))

;;(test 0 (p))

;; Applicative-order evaluation:
;; Arguments are evaluated prior to calling test so p is evaluated and will loop forever


;; Normal-order evaluation:
;; Arguments are substituted and because x is 0 then (p) is never evaluated
