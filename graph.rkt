#lang racket

(provide new-graph)

(define graph-edges
  '((1 2 25) (1 3 7) (1 4 13) (3 4 1) (3 2 10) (3 5 7) (2 5 100) (2 4 5) (4 5 2)))


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
