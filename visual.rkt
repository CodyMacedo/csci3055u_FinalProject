#lang racket
(require racket/draw)
(require racket/gui)

(provide draw)

(define target (make-bitmap 220 220))
(define dc (new bitmap-dc% [bitmap target]))
(make-object image-snip% target)

(define (draw)
  (for ([i 10])
    (for ([j 10])
      (send dc set-brush "black" 'solid)
      (send dc draw-rectangle (+ i (* 20 i)) (+ j (* 20 j)) 20 20))))

