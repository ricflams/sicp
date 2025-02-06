#lang sicp
(#%require "util.rkt")

;; exercise 1.28
(display "exercise 1.28\n")

(#%require "math.rkt")

(define (is-non-trivial-root-of-1-modulo-n a n)
  (and (not (= a 1))
       (not (= a (- n 1)))
       (= (remainder (square a) n) 1)))

(define (non-trivial-root-test a m)
  (if (is-non-trivial-root-of-1-modulo-n a m)
      0
      (remainder (square a) m)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp) 

	 ;;(remainder (square ) m))
	 (non-trivial-root-test (expmod base (/ exp 2) m) m))
	  
	(else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((miller-rabin-test n) (fast-prime? n (- times 1)))
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


(test-for-primes 10000000 3)
(test-for-primes 100000000 3)
(test-for-primes 1000000000 3)
(test-for-primes 4000000000 3)

;; All these Carmichal numbers should now test as "not prime", ie false
(fast-prime? 561 10)
(fast-prime? 1105 10)
(fast-prime? 1729 10)
(fast-prime? 2465 10)
(fast-prime? 2821 10)
(fast-prime? 6601 10)

;; WIP TODO
