#lang sicp
(#%require "util.rkt")

;; exercise 1.24
(display "exercise 1.24\n")


(#%require "expmod.rkt")

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))



(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)  ;; test fast-prime
      (report-prime n (- (runtime) start-time))
      #f))

(define (report-prime n elapsed-time)
  (print n " *** " (/ elapsed-time 1000.0) " msec")
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


(define (repeat func arg times)
  (if (= times 1)
      (func arg)
      (repeat func arg (- times 1))))

(test-for-primes 10000000 3)
(test-for-primes 100000000 3)
(test-for-primes 1000000000 3)
(test-for-primes 4000000000 3)





