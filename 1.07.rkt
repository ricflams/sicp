#lang sicp

;; exercise 1.7
(display "exercise 1.7")
(newline)

(#%require "math.rkt")
(#%require "util.rkt")

(define (average x y)
  (/ (+ x y ) 2))

(define (sqrt x)
  (define (sqrt-iter prev-guess guess)
    (if (good-enough? prev-guess guess)
	guess
	(sqrt-iter guess (improve guess))))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? prev-guess guess)
    (or (= prev-guess guess)
	(< (abs (- (/ prev-guess guess) 1)) 0.000001)))
  (sqrt-iter 0.0 1.0))


(print "(sqrt 0.0001)=" (sqrt 0.0001))
(print "(sqrt 0.01)=" (sqrt 0.01))
(print "(sqrt 1.2)=" (sqrt 1.2))
(print "(sqrt 9)=" (sqrt 9))
(print "(sqrt 1000000)=" (sqrt 1000000))
(print "(sqrt 100000000)=" (sqrt 100000000))
(print "(sqrt 1e26)=" (sqrt 1e26))

(print "(square (sqrt 0.001))=" (square (sqrt 0.001)))
(print "(square (sqrt 100))=" (square (sqrt 100)))
(print "(square (sqrt 1000))=" (square (sqrt 1000)))
(print "(square (sqrt 10000))=" (square (sqrt 10000)))
(print "(square (sqrt 100000))=" (square (sqrt 100000)))
