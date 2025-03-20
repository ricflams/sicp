#lang sicp
(#%require "util.rkt")
(#%require "math.rkt")
(#%require "expt.rkt")
(#%require "gcd.rkt")
(#%require racket/trace)

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
  
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

  

;; exercise 2.1
(display "\nexercise 2.1\n\n")

(define (rat->string x)
  (to-string (numer x) "/" (denom x)))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

(print-eval (rat->string one-half))
(print-eval (rat->string (add-rat one-half one-third)))
(print-eval (rat->string (mul-rat one-half one-third)))
(print-eval (rat->string (add-rat one-third one-third)))

;; exercise 2.2
(display "\nexercise 2.2\n\n")

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (point->string p)
  (to-string (x-point p) "," (y-point p)))

(define (make-segment a b) (cons a b))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (midpoint-segment seg)
  (let ((start (start-segment seg))
        (end (end-segment seg)))
    (make-point
     (/ (+ (x-point start) (x-point end)) 2.0)
     (/ (+ (y-point start) (y-point end)) 2.0))))

(define s1 (make-segment
           (make-point -4 2)
           (make-point 6 7)))
(print-eval (point->string (midpoint-segment s1)))

(define s2 (make-segment
           (make-point 0 1)
           (make-point 0 3)))
(print-eval (point->string (midpoint-segment s2)))

 
;; exercise 2.3
(display "\nexercise 2.3\n\n")

;; D'OH!
;; I misread rectangle as triangle - so here's code for triangles first

(define (make-tri x1 y1 x2 y2 x3 y3)
  (make-tri-from-points
   (make-point x1 y1)
   (make-point x2 y2)
   (make-point x3 y3)))
;; (define (make-tri-from-points p1 p2 p3)
;;   (cons p1 (cons p2 p3)))
;; (define (tri-p1 t) (car t))
;; (define (tri-p2 t) (car (cdr t)))
;; (define (tri-p3 t) (cdr (cdr t)))
(define (make-tri-from-points p1 p2 p3)
  (cons (cons p3 p1) p2))
(define (tri-p1 t) (cdr (car t)))
(define (tri-p2 t) (cdr t))
(define (tri-p3 t) (car (car t)))

;; A = (1/2) |x1(y2 − y3) + x2(y3 − y1) + x3(y1 − y2)|
(define (tri-area t)
  (let ((p1 (tri-p1 t))
        (p2 (tri-p2 t))
        (p3 (tri-p3 t)))
    (/ (abs
        (+ (* (x-point p1) (- (y-point p2) (y-point p3)))
           (* (x-point p2) (- (y-point p3) (y-point p1)))
           (* (x-point p3) (- (y-point p1) (y-point p2)))))
       2)))

(define (tri-perimeter t)
  (define (length p1 p2)
    (let ((dx (abs (- (x-point p1) (x-point p2))))
          (dy (abs (- (y-point p1) (y-point p2)))))
      (sqrt (+ (square dx) (square dy)))))
  (let ((p1 (tri-p1 t))
        (p2 (tri-p2 t))
        (p3 (tri-p3 t)))
    (+ (length p1 p2)
       (length p2 p3)
       (length p3 p1))))

(define t1 (make-tri 0 0 10 0 5 5))
(tri-area t1)
(tri-perimeter t1)

(define t2 (make-tri 0 -5 5 0 0 5))
(tri-area t2)
(tri-perimeter t2)

(define t3 (make-tri 1 1 7 3 9 3))
(tri-area t3)
(tri-perimeter t3)


(define (make-rect botx boty topx topy)
  (make-rect-from-points
   (make-point botx boty)
   (make-point topx topy)))
;; (define (make-rect-from-points bot top) ;; alternative impl
;;   (cons bot top))
;; (define (rect-bot r) (car r))
;; (define (rect-top r) (cdr r))
(define (make-rect-from-points bot top)
  (cons top bot))
(define (rect-bot r) (cdr r))
(define (rect-top r) (car r))

(define (rect-height r)
  (abs (- (x-point (rect-top r))
          (x-point (rect-bot r)))))
(define (rect-width r)
  (abs (- (y-point (rect-top r))
          (y-point (rect-bot r)))))
(define (rect-area r)
  (* (rect-height r) (rect-width r)))
(define (rect-perimeter r)
  (* 2 (+ (rect-height r) (rect-width r))))

(define r1 (make-rect 0 0 10 10))
(rect-area r1)
(rect-perimeter r1)

(define r2 (make-rect 2 2 -10 20))
(rect-area r2)
(rect-perimeter r2)

(define r3 (make-rect 8 2 -2 -8))
(rect-area r3)
(rect-perimeter r3)


;; exercise 2.4
(display "\nexercise 2.4\n\n")

(define (pick-first a b) a)
(define (pick-second a b) b)
(define (cons-v2 x y)
  (lambda (some-picker) (some-picker x y)))
(define (car-v2 z)
  (z pick-first))
(define (cdr-v2 z)
  (z pick-second))

(define test-a (cons 6 9))
(print-eval (car test-a))
(print-eval (cdr test-a))
  
(define test-b (cons "hello" "world"))
(print-eval (car test-b))
(print-eval (cdr test-b))


;; exercise 2.5
(display "\nexercise 2.5\n\n")

;;     2^a * 3^b = x
;; => log(2^a) + log(3^b) = log(x)
;; => a*log(2) + b*log(3) = log(x)
;; Let's assume that a,b is not a unique solution, then
;;    (a+n)*log(2) + (b+m)*log(3) = log(x) for some integers n,m
;; => n*log(2) + m*log(3) = 0
;; => n/m = -log(3)/log(2)
;; => impossible because log(3)/log(2) is irrational
;; => no such n,m exists
;; => a,b is the unique solution
(define (cons-v3 a b)
  (* (expt 2 a)
     (expt 3 b)))
(define (decompose x picker)
  (define (iterate x a b)
    (cond ((= x 1) (picker a b))
          ((= 0 (remainder x 3)) (iterate (/ x 3) a (inc b)))
          ((= 0 (remainder x 2)) (iterate (/ x 2) (inc a) b))
          (else (error "unexpected value of x =" x))))
  ;;(trace iterate)
  (iterate x 0 0))
           
(define (car-v3 x)
  (decompose x pick-first))
(define (cdr-v3 x)
  (decompose x pick-second))

(define (test-cons-v3 a b)
  (let ((x (cons-v3 a b)))
    (print "Testing " a " and " b ":")
    (print-eval x)
    (print-eval (car-v3 x))
    (print-eval (cdr-v3 x))
    (print "")))

(test-cons-v3 6 9)
(test-cons-v3 10 2)
(test-cons-v3 2 10)
(test-cons-v3 1 0)
(test-cons-v3 0 1)
(test-cons-v3 0 0)
(test-cons-v3 99 77)


;; exercise 2.6
(display "\nexercise 2.6\n\n")

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(print-eval-verify ((zero inc) 0) 0)
(print-eval-verify (((add-1 zero) inc) 0) 1)
(print-eval-verify (((add-1 (add-1 zero)) inc) 0) 2)
(print-eval-verify (((add-1 (add-1 (add-1 zero))) inc) 0) 3)

(define one
  (lambda (f) (lambda (x) (f x))))
(print-eval-verify ((one inc) 0) 1)
(print-eval-verify (((add-1 one) inc) 0) 2)

(define two
  (lambda (f) (lambda (x) (f (f x)))))
(print-eval-verify ((two inc) 0) 2)
(print-eval-verify (((add-1 two) inc) 0) 3)
(print-eval-verify ((two square) 2) 16)
(print-eval-verify (((add-1 two) square) 2) 256)

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
(print-eval-verify (((add zero zero) inc) 0) 0)
(print-eval-verify (((add one zero) inc) 0) 1)
(print-eval-verify (((add zero one) inc) 0) 1)
(print-eval-verify (((add one one) inc) 0) 2)
(print-eval-verify (((add one two) inc) 0) 3)
(print-eval-verify (((add two one) inc) 0) 3)
(print-eval-verify (((add two two) inc) 0) 4)


;; exercise 2.7
(display "\nexercise 2.7\n\n")

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
;; why isn't it just the two lowerbounds and two upperbounds?

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(define (make-interval a b)
  (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(print-eval-verify (lower-bound (make-interval 3 4)) 3)
(print-eval-verify (upper-bound (make-interval 3 4)) 4)
                     
;; exercise 2.8
(display "\nexercise 2.8\n\n")

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; exercise 2.9
(display "\nexercise 2.9\n\n")

(define (fmt i)
  (to-string
   (with-precision (lower-bound i) 5)
   ";"
   (with-precision (upper-bound i) 5)))

(define (width-of-interval x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

(define (test-interval-width i1 i2)
  (print "Testing intervals i1=" (fmt i1) " and i2=" (fmt i2))
  (print-eval (width-of-interval i1))
  (print-eval (width-of-interval i2))
  (print-eval (width-of-interval (add-interval i1 i2)))
  (print-eval (width-of-interval (sub-interval i1 i2)))
  (print-eval (width-of-interval (div-interval i1 i2)))
  (print-eval (width-of-interval (mul-interval i1 i2))))

(test-interval-width (make-interval 7 13) (make-interval 37 43))
(test-interval-width (make-interval 0 1) (make-interval 20 22))


;; exercise 2.10
(display "\nexercise 2.10\n\n")

(define (div-interval-safe x y)
  (if (= (lower-bound y) (upper-bound y))
      (error "can't divide by zero interval")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

;; will throw error
(with-handlers-sicp 
  (lambda ()
    (div-interval-safe (make-interval 2 4) (make-interval 3 3)))
  (lambda (message) (print "Error: " message)))

;; exercise 2.11
(display "\nexercise 2.11\n\n")

(define (mul-interval-why-not-just-this-simple? x y)
  (make-interval (* (lower-bound x) (lower-bound y))
                 (* (upper-bound x) (upper-bound y))))

(define (mul-interval-optimized x y)
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
    (if (< x1 x2)
        (if (< y1 y2)
            (make-interval (* x1 y1) (* x2 y2))
            (make-interval (* x1 y2) (* x2 y1)))
        (if (< y1 y2)
            (make-interval (* x2 y1) (* x1 y2))
            (make-interval (* x2 y2) (* x1 y1))))))

(define (test-mul-interval x1 x2 y1 y2)
  (let ((x (make-interval x1 x2))
        (y (make-interval y1 y2)))
    (let ((orig (mul-interval x y))
          (opti (mul-interval-optimized x y))
          (simp (mul-interval-why-not-just-this-simple? x y)))
      (print (fmt x) " mul " (fmt y) ": orig=" (fmt orig) " opti=" (fmt opti) " simp=" (fmt simp)))))

;; Now test all three methods
(test-mul-interval 3 3 3 3)
(test-mul-interval 3 3 3 9)
(test-mul-interval 3 3 9 3)
(test-mul-interval 3 9 3 3)
(test-mul-interval 3 9 3 9)
(test-mul-interval 3 9 9 3)
(test-mul-interval 9 3 3 3)
(test-mul-interval 9 3 3 9)
(test-mul-interval 9 3 9 3)


;; exercise 2.12
(display "\nexercise 2.12\n\n")

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c pct)
  (make-center-width c (* c pct)))
(define (percent i)
  (/ (width i) (center i)))

(print-eval (fmt (make-center-width 50 7)))
(print-eval (center (make-center-width 50 7)))
(print-eval (width (make-center-width 50 7)))
(print-eval (fmt (make-center-percent 50 0.14)))
(print-eval (center (make-center-percent 50 0.14)))
(print-eval (width (make-center-percent 50 0.14)))


;; exercise 2.13
(display "\nexercise 2.13\n\n")

(define (approx-tolerance-of-mul x y)
  (- (* (+ (percent x) 1)
        (+ (percent y) 1))
     1))
(define (real-tolerance-of-mul x y)
  (percent (mul-interval x y)))

(define (test-mul-tolerance c1 pct1 c2 pct2)
  (let ((x (make-center-percent c1 pct1))
        (y (make-center-percent c2 pct2)))
    (let ((real (real-tolerance-of-mul x y))
          (approx (approx-tolerance-of-mul x y)))
      (print (fmt x) " mul " (fmt y)
             ": real%=" (with-precision real 8)
             " approx%=" (with-precision approx 8)))))

(test-mul-tolerance 10 0.0001 20 0.0001)
(test-mul-tolerance 10 0.001 20 0.001)
(test-mul-tolerance 10 0.01 20 0.01)
(test-mul-tolerance 10 0.1 20 0.1)
(test-mul-tolerance 10 0.5 20 0.5)


;; exercise 2.14
(display "\nexercise 2.14\n\n")

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one
     (add-interval (div-interval one r1)
                   (div-interval one r2)))))

(define (test-par c1 pct1 c2 pct2)
  (let ((r1 (make-center-percent c1 pct1))
        (r2 (make-center-percent c2 pct2)))
    (let ((p1 (par1 r1 r2))
          (p2 (par2 r1 r2)))
      (print (fmt r1) " || " (fmt r2) ": par1=" (fmt p1) " par2=" (fmt p2)))))

(test-par 100 0.01 100 0.01)

;; exercise 2.15

;; If the same object (eg r1) appear multiple times in an
;; expression then it must should have the same exact value in
;; all places, but the indepedently calculated intervals don't
;; know about that.
;; Eg for interval x then x/x should always be 1, but it's not:
(define rx (make-interval 10 50))
(print (fmt (div-interval rx rx)))

;; exercise 2.16

;; hmmm
