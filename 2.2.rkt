#lang sicp
(#%require "util.rkt")
(#%require "math.rkt")
(#%require "expt.rkt")
(#%require "gcd.rkt")
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
       
