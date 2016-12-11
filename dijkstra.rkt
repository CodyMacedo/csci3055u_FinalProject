#lang racket


(define (dijkstra origin destination graph)
  (define (update x)
    (begin
      (set! new-dist (+ (dist-between n x graph)
                         (dist-info-node (get-info-node i n))))
      (when (< new-dist (dist-info-node (get-info-node i v)))
        (update-previous-dist-node i v new-dist n))))

(define (get-info-node i n)
  (define (get-info-node-aux i n cont)
    (if (equal? n (vector-ref (no-info-no i) cont))
        (vector-ref i cont)
        (get-info-node-aux i n (+ cont 1))))
  (get-info-node-aux i n 0))


(define (dist-info-node i)
  (vector-ref i 1))

   (define new-dist 0)

(define (update-previous-dist-node! i n d a)
  (define (update-previous-dist-node!-aux i n d a cont)
    (if (equal? n (vector-ref (no-info-no i) cont))
        (begin
          (modify-dist! (vector-ref i cont) d)
          (modify-previous! (vector-ref i cont) a))
        (update-previous-dist-node!-aux i n d a (+ cont 1))))
  (update-previous-dist-node!-aux i n d a 0))