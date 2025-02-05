#lang sicp
(#%require "util.rkt")

;; exercise 1.19
(display "exercise 1.19\n")


(#%require "math.rkt")

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
	b
	(fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(print-eval (fib 3))
(print-eval (fib 4))
(print-eval (fib 5))
(print-eval (fib 10))
(print-eval (fib 20))
(print-eval (fib 30))


;;       a' = bq + aq + ap
;;       b' = bp + aq
;; <=>
;;       a'' = b'q + a'q + a'p
;;       b'' = b'p + a'q
;; <=>
;;       a'' = (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;;       b'' = (bp + aq)p + (bq + aq + ap)q 
;; <=>
;;       a'' = bpq + aq^2 + bq^2 + aq^2 + apq + bqp + aqp + ap^2
;;       b'' = bp^2 + aqp + bq^2 + aq^2 + apq
;; <=>
;;       a'' = b(pq + q^2 + qp) + a(q^2 + q^2 + pq + qp + p^2)
;;       b'' = b(p^2 + q^2) + a(qp + q^2 + pq)
;; <=>
;;       a'' = b(q^2 + 2qp) + a(q^2 + 2pq) + a(p^2 + q^2)
;;       b'' = b(p^2 + q^2) + a(q^2 + 2pq)
;; <=>
;;       a'' = bq' + aq' + ap'
;;       b'' = bp' + aq'
;; where p' = p^2 + q^2
;; and   q' = q^2 + 2pq
;; 
(define (fib-fast n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
	  ((even? count)
	   (fib-iter a
		     b
		     
		     (+ (square p) (square q))  ;; p' = p^2 + q^2
		     (+ (square q) (* 2 p q))   ;; q' = q^2 + 2pq
		     
		     (/ count 2)))
	  (else
	   (fib-iter (+ (* b q) (* a q) (* a p))
		     (+ (* b p) (* a q))
		     p
		     q
		     (- count 1)))))
  (fib-iter 1 0 0 1 n))

(print-eval (fib-fast 3))
(print-eval (fib-fast 4))
(print-eval (fib-fast 5))
(print-eval (fib-fast 10))
(print-eval (fib-fast 20))
(print-eval (fib-fast 30))


		 

