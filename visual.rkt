#lang racket
(require racket/draw)


(define target (make-bitmap 200 200))
(define dc (new bitmap-dc% [bitmap target]))

(for ([i 10])
  (for ([j 10])
    (send dc set-brush "black" 'solid)
    (send dc draw-rectangle (+ 0 (* 10 i) 1) (+ 0 (* 10 j) 1) 10 10))) 