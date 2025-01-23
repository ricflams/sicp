#lang sicp

(display "exercise 1.22\n")
(#%require "prime.rkt")

;; 1.22

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; (define (test-for-primes start n)
;;   (if (> n 0)
;;       (search-for-primes (next-prime-pos start



