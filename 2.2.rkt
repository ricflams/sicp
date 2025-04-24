#lang sicp
(#%require "util.rkt")
(#%require "math.rkt")
(#%require "expt.rkt")
(#%require "gcd.rkt")
(#%require "fib.rkt")
(#%require "paint.rkt")
(#%require sicp-pict)
(#%require racket/trace)


(display "\nexercise 2.17\n\n")

(define (last-pair lst)
  (cond ((null? lst) nil)
        ((null? (cdr lst)) (car lst))
        (else (last-pair (cdr lst)))))

(last-pair (list))
(last-pair (list 1))
(last-pair (list 23 72 149 34))


(display "\nexercise 2.18\n\n")

(define (reverse-bad lst)
  (if (null? lst)
      nil
      (list (reverse-bad (cdr lst)) (car lst))))

(reverse-bad (list 1 4 8 16 25))

(define (reverse lst)
  (define (iterate in out)
    (if (null? in)
        out
        (iterate (cdr in) (cons (car in) out))))
  (iterate lst (list)))

(reverse (list 1 4 8 16 25))
(reverse (list))
(reverse (list 1))

(display "\nexercise 2.19\n\n")

(define (count-change-orig amount)
  (cc-orig amount 5))
(define (cc-orig amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc-orig amount
                          (- kinds-of-coins 1))
                 (cc-orig (- amount
                             (first-denomination-orig kinds-of-coins))
                          kinds-of-coins)))))
(define (first-denomination-orig kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(print-eval-verify (count-change-orig 100) 292)

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values)))))
(define (no-more? coin-values)
  (null? coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (first-denomination coin-values)
  (car coin-values))

(count-change-orig 100)
(cc 100 us-coins)

(count-change-orig 180)
(cc 180 us-coins)
(cc 180 (reverse us-coins))

(cc 12.5 uk-coins)
(cc 12.5 (reverse uk-coins))
(cc 100 uk-coins)
;;(cc 253.5 uk-coins) ;; very very slow; result is 9975315
(cc 253 (list 100 50 20 10 5 2 1)) ;; omitting 0.5 makes it 50x faster (210590)


(display "\nexercise 2.20\n\n")

;; surprisingly tricky to get right
(define (same-parity first . lst)
  (define (has-same-parity a b)
    (= (remainder a 2) (remainder b 2)))
  (define (iterate rest)
    (cond ((null? rest) nil)
          ((has-same-parity first (car rest))
           (cons (car rest) (iterate (cdr rest))))
          (else (iterate (cdr rest)))))
  (cons first (iterate lst)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
(same-parity 3 5 7 8 8 7 5 3 1 2 2 2 3)
(same-parity 0 5 7 8 8 7 5 3 1 2 2 2 3)
(same-parity 1)


(display "\nexercise 2.21\n\n")

(define (square-list-1 items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list-1 (cdr items)))))
(square-list-1 (list 1 2 3 4))

(define (square-list-2 items)
  (map (lambda (x) (square x)) items))
(square-list-2 (list 1 2 3 4))

(display "\nexercise 2.22\n\n")

;; Doesn't work because each answer (square ...) is always places ahead of
;; all the existing answers. So first we calculate (square 1) and put that
;; first, ie (1). Then (square 2) and put that first, ie (4 1). Etc (9 4 1)
;; and (16 9 4 1)
(define (square-list-iterative items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (trace iter)
  (iter items nil))

(square-list-iterative (list 1 2 3 4))

;; Doesn't work well because we keep puting the answer generated so
;; far, which is a list, ahead of the element
(define (square-list-iterative-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(square-list-iterative-2 (list 1 2 3 4))


(display "\nexercise 2.23\n\n")

(define (for-each f items)
  (cond ((not (null? items))
         (f (car items))
         (for-each f (cdr items)))))
(for-each
 (lambda (x) (display x) (newline))
 (list 57 321 88))
       

(display "\nexercise 2.24\n\n")

(print-eval (list 1 (list 2 (list 3 4))))


(display "\nexercise 2.25\n\n")

(print-eval (car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9)))))))
(print-eval (car (car (list (list 7)))))
(print-eval (cdr (cdr (cdr (cdr (cdr (cdr (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 7)))))))))))))


;; exercise 2.26
(display "\nexercise 2.26\n\n")

(define x (list 1 2 3))
(define y (list 4 5 6))

(print-eval (append x y))
(print-eval (cons x y))
(print-eval (list x y))


(display "\nexercise 2.27\n\n")

(define (deep-reverse lst)
  (cond ((null? lst) nil)
        ((not (pair? lst)) lst)
        (else (reverse (map deep-reverse lst)))))

(define xx (list (list 1 2) (list 3 4)))
(print-eval xx)
(print-eval (reverse xx))
(print-eval (reverse (list )))
(print-eval (reverse (list 1)))
(print-eval (reverse xx))

;;(trace deep-reverse)
;;(trace reverse)

(print-eval (deep-reverse xx))


(display "\nexercise 2.28\n\n")

;; Lesson: pair? just means "is it a list?", not specifically a pair of two values
;; Lesson: (cons a (list b c)) => (a b c)

(define (fringe x)
  (cond ((null? x) x)
        ((not (pair? x)) (list x))
        (else (append (fringe (car x)) (fringe (cdr x))))))

(define xf (list (list 1 2) (list 3 4)))
(print-eval (fringe xf))
(print-eval (fringe (list xf xf)))


(display "\nexercise 2.29\n\n")

(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

;;(define (make-mobile left right) (cons left right)) ;; task d
;;(define (make-branch length structure) (cons length structure)) ;; task d

(define mob1 (make-mobile
	      (make-branch 1
			   (make-mobile
			    (make-branch 2 3)
			    (make-branch 4
					 (make-mobile
					  (make-branch 5 6)
					  (make-branch 7 8)))))
	      (make-branch 6
			   (make-mobile
			    (make-branch 9 10)
			    (make-branch 11 12)))))
  
(define (left-branch mob) (car mob))
(define (right-branch mob) (cadr mob))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))
;;(define (right-branch mob) (cdr mob)) ;; task d
;;(define (branch-structure branch) (cdr branch)) ;; task d

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (pair? structure)
	(total-weight structure)
	structure)))

(define (total-weight mob)
  (+ (branch-weight (left-branch mob))
     (branch-weight (right-branch mob))))

;;(trace total-weight)
;;(trace branch-structure)

(print-eval-verify (total-weight mob1) (+ 3 6 8 10 12))

(define (mobile-balanced? mob)
  (let ((left (left-branch mob))
	(right (right-branch mob)))
    (= (* (branch-length left) (branch-weight left))
       (* (branch-length right) (branch-weight right)))))

(print-eval (mobile-balanced? mob1))

;; Make a balanced mobile:
;; 2*(4+6) == 1*20
(define mob2 (make-mobile
	      (make-branch 2
			   (make-mobile
			    (make-branch 3 4)
			    (make-branch 5 6)))
	      (make-branch 1 20)))

(print-eval (mobile-balanced? mob2))

;; Using cons instead of list: only the lines marked "task d" are needed


(display "\nexercise 2.30\n\n")

(define (square-tree tree)
  (map (lambda (sub)
	 (if (pair? sub)
	     (square-tree sub)
	     (square sub)))
	 tree))

(print-eval (square-tree
	     (list 1
		   (list 2 (list 3 4) 5)
		   (list 6 7))))

;; Now direcly, not using map
;; A bit tricky not to get reversed
(define (square-tre2 tree)
  (define (iter squared in)
    (cond ((null? in) squared)
	  ((not (pair? in)) (square in))
	  (else (cons (square-tre2 (car in)) (iter squared (cdr in))))))
  (iter (list) tree))
  
(print-eval (square-tre2
	     (list 1
		   (list 2 (list 3 4) 5)
		   (list 6 7))))


(display "\nexercise 2.31\n\n")

(define (tree-map f tree)
  (map (lambda (sub)
	 (if (pair? sub)
	     (tree-map f sub)
	     (f sub)))
	 tree))

(print-eval (tree-map square
	     (list 1
		   (list 2 (list 3 4) 5)
		   (list 6 7))))
(print-eval (tree-map cube
	     (list 1
		   (list 2 (list 3 4) 5)
		   (list 6 7))))


(display "\nexercise 2.32\n\n")

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	;; the result is all these subsets plus the subsets that
	;; consists of the omitted first element and each subset
	(append
	 rest
	 (map
	  (lambda (subset)
	    (cons (car s) subset)) ;; first element plus the subset(s)
	  rest)))))

(print-eval (subsets (list 1 2 3)))


(display "\nexercise 2.33\n\n")

(define (filter predicate? sequence)
  (cond ((null? sequence) nil)
	((predicate? (car sequence))
	 (cons (car sequence)
	       (filter predicate? (cdr sequence))))
	(else (filter predicate? (cdr sequence)))))

(print-eval (filter odd? (list 1 2 3 4 5)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(print-eval (accumulate + 0 (list 1 2 3 4 5)))
(print-eval (accumulate * 1 (list 1 2 3 4 5)))
(print-eval (accumulate cons nil (list 1 2 3 4 5)))


(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(print-eval (enumerate-interval 2 7))


(define (enumerate-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

(print-eval (enumerate-tree (list 1 (list 2 (list 3 4) 5))))

(define (sum-odd-squares tree)
  (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

(print-eval (sum-odd-squares (list 1 (list 2 (list 3 4) 5))))

(define (even-fibs n)
  (accumulate cons nil (filter even? (map fib (enumerate-interval 1 n)))))

(print-eval (even-fibs 10))


(define (map2 p sequence)
  (accumulate
   (lambda (x y) (cons (p x) y))
   nil sequence))

(print-eval (map square (list -2 3 7)))


(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1))

(print-eval (append2 (list 1 2 3) (list 4 5 6)))
(print-eval (append2 (list) (list 4 5 6)))
(print-eval (append2 (list 1 2 3) (list)))


(define (length sequence)
  (accumulate (lambda (_ sum) (+ sum 1)) 0 sequence))

(print-eval (length (list 1 2 3 4 5)))
(print-eval (length (list 1)))
(print-eval (length (list)))


(display "\nexercise 2.34\n\n")

(define (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
   0
   coefficient-sequence))

;;   1 + 3x  5x^3 + x^5 for x=2
;; = 1 + 3*2 + 5*8 + 32
;; = 1 + 6 + 40 + 32 = 79
(print-eval-verify (horner-eval 2 (list 1 3 0 5 0 1)) 79)


(display "\nexercise 2.35\n\n")

;; TODO
(define (count-leaves tree)
  (accumulate
   (lambda (node sum)
     (if (pair? node)
	 (count-leaves node)
	 1))
   0
   ))
;(print-eval (count-leaves
;	     (list 1
;		   (list 2 (list 3 4) 5)
;		   (list 6 7))))



(display "\nexercise 2.36\n\n")

;; uhh, so elegant
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

(print-eval (accumulate-n
	     +
	     0
	     (list
	      (list 1 2 3)
	      (list 4 5 6)
	      (list 7 8 9)
	      (list 10 11 12))))


(display "\nexercise 2.37\n\n")

(define v1 (list 1 2 3 4))
(define v2 (list 4 5 6 6))
(define v3 (list 6 7 8 9))
(define m1 (list v1 v2 v3))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(print-eval-verify (dot-product v1 v1) (+ 1 4 9 16))
(print-eval-verify (dot-product v2 v2) (+ 16 25 36 36))

;; Interestingly,
;;  (map op v1 v2 v3 ...) == (accumulate-n op <init> (list v1 v2 v3 ...)
;;
;; For example
;;  (map * v w) == (accumulate-n * 1 (list v w))


(define (matrix-*-vector m v)
  (map (lambda (row) (accumulate + 0 (map * row v))) m))

(print-eval (matrix-*-vector m1 v1))
(print-eval (matrix-*-vector m1 v2))

;; ohhhh, elegant
(define (transpose mat)
  (accumulate-n cons nil mat))

(print-eval (transpose m1))


;; TODO
;;(define (matrix-*-matrix m n)
;;  (let ((cols (transpose n)))
;;    (map  m)))
;;
;;(print-eval (matrix-*-matrix m1 m1))


(display "\nexercise 2.38\n\n")
;; TODO

(display "\nexercise 2.39\n\n")
;; TODO

(display "\nexercise 2.40\n\n")
;; TODO

(display "\nexercise 2.41\n\n")
;; TODO

(display "\nexercise 2.42\n\n")
;; TODO

(display "\nexercise 2.43\n\n")
;; TODO


(1)

;; exercise 2.44
(display "\nexercise 2.44\n\n")

(paint-to-png wave "wave.png")


(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(paint-to-png wave2 "wave2.png")
(paint-to-png wave4 "wave4.png")

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))


(paint-to-png (flipped-pairs wave) "wave4-2.png")


(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(paint-to-png (right-split wave 4) "wave-right-split-4.png")


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(paint-to-png (corner-split wave 4) "wave-corner-split-4.png")

(define (four-corner-split painter n)
  (let ((corner (corner-split painter n)))
    (let ((upper (beside (flip-horiz corner) corner)))
      (below (flip-vert upper) upper))))

(paint-to-png (four-corner-split wave 4) "wave-four-corner-split-4.png")

;; exercise 2.45
(display "\nexercise 2.45\n\n")


(define (split grow dup)
  (lambda (painter n)
    (if (= n 0)
      painter
      (let ((smaller ((split grow dup) painter (- n 1))))
        (grow painter (dup smaller smaller))))))
(define right-splitter (split beside below))
(define up-splitter (split below beside))
  

(paint-to-png (right-split wave 4) "wave-right-split-4.png")
(paint-to-png (up-split wave 4) "wave-up-split-4.png")
(paint-to-png (right-splitter wave 4) "wave-right-splitter-4.png")
(paint-to-png (up-splitter wave 4) "wave-up-splitter-4.png")

`(1 2 3)
