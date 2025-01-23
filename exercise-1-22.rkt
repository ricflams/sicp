#lang sicp

(display "exercise 1.22\n")
(#%require "prime.rkt")

;; 1.22

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      #f))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display (/ elapsed-time 1000.0))
  (newline)
  #t)



(define (test-for-primes start count)
  (cond ((even? start)
	 (test-for-primes (+ start 1) count))
	((> count 0)
	 (test-for-primes
	  (+ 2 (find-next-prime start))
	  (- count 1)))))

(define (find-next-prime x)
  (if (timed-prime-test x)
      x
      (find-next-prime (+ x 2))))


(test-for-primes 100000000 3)
(test-for-primes 1000000000 3)
(test-for-primes 10000000000 3)
(test-for-primes 100000000000 3)
(test-for-primes 1000000000000 3)
(test-for-primes 10000000000000 3)





