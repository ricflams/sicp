#lang sicp
(#%require "util.rkt")
(#%require "paint.rkt")
(#%require sicp-pict)

(paint-to-png wave "wave.png")

;; exercise 1.16
(display "exercise 1.16\n")


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
