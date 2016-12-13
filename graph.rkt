#lang racket

(provide new-graph)

(define graph-edges
  '((a b 25) (a c 7) (a d 13) (c d 1) (c b 10) (c e 7) (b e 100) (b d 5) (d e 2)))


(define (graph-undirected edges)
  (for/fold ([graph (hash)])
            ([edge edges])
    (define-values (node1 node2 v)
      (values (first edge) (second edge) (third edge)))
    (hash-update
     (hash-update graph node1 (lambda (adj-list) (cons '(node2 v) adj-list)) '())
     node2 (lambda (adj-list) (cons '(node1 v) adj-list)) '()) 
    ))


(define new-graph (graph-undirected graph-edges))
