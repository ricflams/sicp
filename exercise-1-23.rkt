#lang sicp

;; 1.23
(display "exercise 1.23\n")

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (define (square x) (* x x))
    (define (divides? a b)
      (= (remainder b a) 0))
    (define (next n)  ;; (next n) is only change from 1.22
      (if (= n 2) 3 (+ n 2)))
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (next test-divisor)))))
  (find-divisor n 2))
(define (prime? n)
  (= (smallest-divisor n) n))


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


(test-for-primes 10000000000 3)
(test-for-primes 100000000000 3)
(test-for-primes 1000000000000 3)
(test-for-primes 10000000000000 1)





