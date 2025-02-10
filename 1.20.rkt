#lang sicp
(#%require "util.rkt")

;; exercise 1.20
(display "exercise 1.20\n")


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;      (gcd 206 40)
;;
;; ->   (if (= 40 0)
;;          206
;;          (gcd 40 (rem 206 40)))
;; 
;; ->       (if (= (rem 206 40) 0)
;;              40
;;              (gcd (rem 206 40) (rem 40 (rem 206 40)))
;;
;; ->           (if (= (rem 40 (rem 206 40)) 0)
;;                  (rem 206 40)
;;                  (gcd (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40)))
;;
;; ->               (if (= (rem (rem 206 40) (rem 40 (rem 206 40))) 0)
;;                      (rem 40 (rem 206 40))
;;                      (gcd (rem (rem 206 40) (rem 40 (rem 206 40))) (rem (rem 40 (rem 206 40)) (rem 40 (rem 40 (rem 206 40))) (rem 40 (rem 40 (rem 40 (rem 206 40))))))
;;
;; ->               (if (= (rem (rem 40 (rem 206 40)) (rem 40 (rem 40 (rem 206 40))) (rem 40 (rem 40 (rem 40 (rem 206 40)))))) 0)
;;                      (rem (rem 206 40) (rem 40 (rem 206 40)))
;;                      (gcd ....)
;;
;; needs >1 iteration:



;;      (gcd 206 40)
;;
;; ->   (if (= 40 0)
;;          206
;;          (gcd 40 B1))   B1=(rem 206 40)
;; 
;; ->       (if (= B1 0)
;;              40
;;              (gcd B1 B2)   B2=(rem 40 B1)
;;
;; ->           (if (= B2 0)
;;                  B1
;;                  (gcd B2 B3)   B3=(rem B1 B2)
;;
;; ->               (if (= B3 0)
;;                      B2
;;                      (gcd B3 B4   B4=(rem B2 B3)
;;
;; ->                   (if (= B4 0)
;;                          B3
;;                          (gcd B4 B5   B5=(rem B3 B4)
;;
;; ->                   (if (= Bn+1 0)
;;                          Bn
;;                          (gcd Bn+1 Bn+2)  Bn+1=(rem Bn-1 Bn)
;;
;; needs >1 iteration:

(#%require racket/trace)

(define (my-remainder a b)
  (remainder a b))

(define-syntax my-remainder-2
  (syntax-rules ()
    [(my-remainder-2 a b) (my-remainder a b)]))

(define-syntax gcd-normal-order-2
  (syntax-rules ()
    [(gcd-normal-order-2 a b)
     (if (= (b) 0)
	 (a)
	 (gcd-normal-order-2 b (my-remainder-2 a b)))]))

(define (gcd-normal-order a b)
  (if (= (force b) 0)
      (force a)
      (gcd-normal-order b (delay (my-remainder a b)))))

(trace my-remainder)

(gcd-normal-order-2 (lambda () 206) (lambda () 40))




;; (trace gcd)

;; (gcd 206 40)
;; (gcd 40 206)






;; TODO
