#lang sicp
(#%require "util.rkt")
(#%require racket/trace)

(display "\nexercise 2.53\n\n")

(print-eval (list 'a 'b 'c)) ;; (a b c)
(print-eval (list 'george)) ;; (george) 
(print-eval (cdr '((x1 x2) (y1 y2)))) ;; ((y1 y2))
(print-eval (cadr '((x1 x2) (y1 y2)))) ;; (y1 y2)
(print-eval (pair? (car '(a short list)))) ;; false - is a, a symbol
(print-eval (memq 'red '((red shoes) (blue socks)))) ;; false 
(print-eval (memq 'red '(red shoes blue socks))) ;; (red shoes blue socks)


(display "\nexercise 2.54\n\n")

(define (equal? a b)
  (cond
   ;; if either is null then they must both be null
   ((or (null? a) (null? b))
    (and (null? a) (null? b)))
   ;; if either is a pair then they must both be pairs and equal
   ((or (pair? a) (pair? b))
    (and
     (pair? a)
     (pair? b)
     (equal? (car a) (car b))
     (equal? (cdr a) (cdr b))))
   ;; if either is a symbol then they must both be symbols and be equal
   ((or (symbol? a) (symbol? b))
    (and
     (symbol? a)
     (symbol? b)
     (eq? a b)))
   ;; else their values must be equal
   (else (= a b))))

;;(trace equal?)

(equal? '(this is a list) '(this is a list))
(print-eval (equal? '(this is a list) '(this is a list)))
(print-eval (equal? '(this is a list) '(this (is a) list)))
(print-eval (equal? '(x 9 '() (a 007 8)) '(x 9 '() (a 007 8))))
(print-eval (equal? '() '()))
(print-eval (equal? 2 2))
(print-eval (equal? 2 3))
(print-eval (equal? 2 'a))
(print-eval (equal? 'a 'b))
(print-eval (equal? '(a 2) '(b 2)))


(display "\nexercise 2.55\n\n")

(print-eval (car ''abracadabra)) ;; = (car (quote (quote aracadabra))) = (car 'quote 'abracadabra) = 'quote
(print-eval 'quote)
(print-eval 'quiz)


;;(display "\nexercise 2.56\n\n")

(let ()
  (define (deriv exp var)
    (cond ((number? exp) 0)
	  ((variable? exp) (if (same-variable? exp var) 1 0))
	  ((sum? exp) (make-sum (deriv (addend exp) var)
				(deriv (augend exp) var)))
	  ((product? exp)
	   (make-sum
	    (make-product (multiplier exp)
			  (deriv (multiplicand exp) var))
	    (make-product (multiplicand exp)
			  (deriv (multiplier exp) var))))
	  (else
	   (error "Cannot deal with expression: DERIV" exp))))

  (define (variable? x) (symbol? x))
  (define (same-variable? x y)
    (and (variable? x) (variable? y) (eq? x y)))
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (make-product a1 a2) (list '* a1 a2))
  (define (sum? x) (and (pair? x) (eq? (car x) '+)))
  (define (addend x) (cadr x))  
  (define (augend x) (caddr x))  
  (define (product? x) (and (pair? x) (eq? (car x) '*)))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))

  (print-eval (deriv '(+ x 3) 'x))
  (print-eval (deriv '(* x y) 'x))
  (print-eval (deriv '(* (* x y) (+ x 3)) 'x))
  )


(let ()
  (define (deriv exp var)
    (cond ((number? exp) 0)
	  ((variable? exp) (if (same-variable? exp var) 1 0))
	  ((sum? exp) (make-sum (deriv (addend exp) var)
				(deriv (augend exp) var)))
	  ((product? exp)
	   (make-sum
	    (make-product (multiplier exp)
			  (deriv (multiplicand exp) var))
	    (make-product (multiplicand exp)
			  (deriv (multiplier exp) var))))
	  (else
	   (error "Cannot deal with expression: DERIV" exp))))
  
  (define (variable? x) (symbol? x))
  (define (same-variable? x y)
    (and (variable? x) (variable? y) (eq? x y)))

  (define (=number? exp val)
    (and (number? exp) (= exp val)))

  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
	  ((=number? a2 0) a1)
	  ((and (number? a1) (number? a2)) (+ a1 a2))
	  (else (list '+ a1 a2))))

  (define (make-product a1 a2)
    (cond ((or (=number? a1 0) (=number? a2 0)) 0)
	  ((=number? a1 1) a2)
	  ((=number? a2 1) a1)
	  ((and (number? a1) (number? a2)) (* a1 a2))
	  (else (list '* a1 a2))))

  (define (sum? x) (and (pair? x) (eq? (car x) '+)))
  (define (addend x) (cadr x))  
  (define (augend x) (caddr x))  
  (define (product? x) (and (pair? x) (eq? (car x) '*)))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))

  (print-eval (deriv '(+ x 3) 'x))
  (print-eval (deriv '(* x y) 'x))
  (print-eval (deriv '(* (* x y) (+ x 3)) 'x))
)


(let ()
  (define (deriv exp var)
    (cond ((number? exp) 0)
	  ((variable? exp) (if (same-variable? exp var) 1 0))
	  ((sum? exp) (make-sum (deriv (addend exp) var)
				(deriv (augend exp) var)))
	  ((product? exp)
	   (make-sum
	    (make-product (multiplier exp)
			  (deriv (multiplicand exp) var))
	    (make-product (multiplicand exp)
			  (deriv (multiplier exp) var))))

	  ((exponentiation? exp)
	   (make-product (exponent exp)
			 (make-exponentiation (base exp) (- (exponent exp) 1))))
	   
	  (else
	   (error "Cannot deal with expression: DERIV" exp))))
  
  (define (variable? x) (symbol? x))
  (define (same-variable? x y)
    (and (variable? x) (variable? y) (eq? x y)))
  (define (=number? exp val)
    (and (number? exp) (= exp val)))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
	  ((=number? a2 0) a1)
	  ((and (number? a1) (number? a2)) (+ a1 a2))
	  (else (list '+ a1 a2))))
  (define (make-product a1 a2)
    (cond ((or (=number? a1 0) (=number? a2 0)) 0)
	  ((=number? a1 1) a2)
	  ((=number? a2 1) a1)
	  ((and (number? a1) (number? a2)) (* a1 a2))
	  (else (list '* a1 a2))))
  (define (sum? x) (and (pair? x) (eq? (car x) '+)))
  (define (addend x) (cadr x))  
  (define (augend x) (caddr x))  
  (define (product? x) (and (pair? x) (eq? (car x) '*)))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))

  (define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
  (define (base x) (cadr x))
  (define (exponent x) (caddr x))
  (define (make-exponentiation b pow)
    (cond ((= pow 0) 1)
	  ((= pow 1) b)
	  (else (list '** b pow))))

  (print-eval (deriv '(** x 0) 'x))
  (print-eval (deriv '(** x 1) 'x))
  (print-eval (deriv '(** x 2) 'x))
  (print-eval (deriv '(** x 7) 'x))
  (print-eval (deriv '(+ (* 3 x) (** x 0)) 'x))
)


(let ()
  (define (deriv exp var)
    (cond ((number? exp) 0)
	  ((variable? exp) (if (same-variable? exp var) 1 0))
	  ((sum? exp) (make-sum (deriv (addend exp) var)
				(deriv (augend exp) var)))
	  ((product? exp)
	   (make-sum
	    (make-product (multiplier exp)
			  (deriv (multiplicand exp) var))
	    (make-product (multiplicand exp)
			  (deriv (multiplier exp) var))))
	  (else
	   (error "Cannot deal with expression: DERIV" exp))))
  
  (define (variable? x) (symbol? x))
  (define (same-variable? x y)
    (and (variable? x) (variable? y) (eq? x y)))

  (define (=number? exp val)
    (and (number? exp) (= exp val)))

  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
	  ((=number? a2 0) a1)
	  ((and (number? a1) (number? a2)) (+ a1 a2))
	  (else (list '+ a1 a2))))

  (define (make-product a1 a2)
    (cond ((or (=number? a1 0) (=number? a2 0)) 0)
	  ((=number? a1 1) a2)
	  ((=number? a2 1) a1)
	  ((and (number? a1) (number? a2)) (* a1 a2))
	  (else (list '* a1 a2))))

  (define (sum? x) (and (pair? x) (eq? (car x) '+)))
  (define (addend x) (cadr x))  
  (define (augend x) (caddr x))  
  (define (product? x) (and (pair? x) (eq? (car x) '*)))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))

  (print-eval (deriv '(+ x 3) 'x))
  (print-eval (deriv '(* x y) 'x))
  (print-eval (deriv '(* (* x y) (+ x 3)) 'x))
)



;;(display "\nexercise 2.57\n\n")

;; (let ()
;;   (define (deriv exp var)
;;     (cond ((number? exp) 0)
;; 	  ((variable? exp) (if (same-variable? exp var) 1 0))
;; 	  ((sum? exp) (make-sum (deriv (addend exp) var)
;; 				(deriv (augend exp) var)))
;; 	  ((product? exp)
;; 	   (make-sum
;; 	    (make-product (multiplier exp)
;; 			  (deriv (multiplicand exp) var))
;; 	    (make-product (multiplicand exp)
;; 			  (deriv (multiplier exp) var))))
;; 	  (else
;; 	   (error "Cannot deal with expression: DERIV" exp))))
  
;;   (define (variable? x) (symbol? x))
;;   (define (same-variable? x y)
;;     (and (variable? x) (variable? y) (eq? x y)))

;;   (define (=number? exp val)
;;     (and (number? exp) (= exp val)))

;;   (define (make-sum a1 a2)
;;     (cond ((=number? a1 0) a2)
;; 	  ((=number? a2 0) a1)
;; 	  ((and (number? a1) (number? a2)) (+ a1 a2))
;; 	  (else (list '+ a1 a2))))
;;   (define (sum? x) (and (pair? x) (eq? (car x) '+)))
;;   (define (addend x) (cadr x))
;;   (define (augend x) (caddr x))
  
;;   (define (product? x) (and (pair? x) (eq? (car x) '*)))
;;   (define (multiplier p) (cadr p))
;;   (define (multiplicand p) (caddr p))
;;   (define (make-product a1 a2)
;;     (cond ((or (=number? a1 0) (=number? a2 0)) 0)
;; 	  ((=number? a1 1) a2)
;; 	  ((=number? a2 1) a1)
;; 	  ((and (number? a1) (number? a2)) (* a1 a2))
;; 	  (else (list '* a1 a2))))

;;   (print-eval (deriv '(+ x 3) 'x))
;;   (print-eval (deriv '(* x y) 'x))
;;   (print-eval (deriv '(* (* x y) (+ x 3)) 'x))
;; )


;;(display "\nexercise 2.58\n\n")
;; TODO


(display "\nexercise 2.59\n\n")

(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))


(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1) (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

(print-eval-verify (element-of-set? 5 '(1 2 3)) #f)
(print-eval-verify (element-of-set? 2 '(1 2 3)) #t)
(print-eval (adjoin-set 5 '()))
(print-eval (adjoin-set 5 '(1 2 3)))
(print-eval (intersection-set '(1 2 3) '(4 5 6)))
(print-eval (intersection-set '(1 2 3) '()))
(print-eval (intersection-set '() '(4 5 6)))
(print-eval (intersection-set '(1 2 3) '(2 1 4)))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((element-of-set? (car set1) set2)
	 (union-set (cdr set1) set2))
	(else (cons (caro set1) (union-set (cdr set1) set2)))))

(print-eval (union-set '(1 2 3) '(4 5 6)))
(print-eval (union-set '(1 2 3) '()))
(print-eval (union-set '() '(4 5 6)))
(print-eval (union-set '(1 2 3) '(2 1 4)))


;;(display "\nexercise 2.60\n\n")
;; TODO


;;(display "\nexercise 2.61\n\n")
;; TODO


;;(display "\nexercise 2.62\n\n")
;; TODO


;;(display "\nexercise 2.63\n\n")
;; TODO


;;(display "\nexercise 2.64\n\n")
;; TODO


;;(display "\nexercise 2.65\n\n")
;; TODO


;;(display "\nexercise 2.66\n\n")
;; TODO


;;(display "\nexercise 2.67\n\n")
;; TODO


;;(display "\nexercise 2.68\n\n")
;; TODO


;;(display "\nexercise 2.69\n\n")
;; TODO


;;(display "\nexercise 2.70\n\n")
;; TODO


;;(display "\nexercise 2.71\n\n")
;; TODO


;;(display "\nexercise 2.72\n\n")
;; TODO

