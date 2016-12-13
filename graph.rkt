#lang racket

(define new-graph
  '((a b 25) (a c 7) (a d 13) (c d 1) (c b 10) (c e 7) (b e 100) (b d 5) (d e 2)))


(define (first-node graph)
  (first (first graph)))

(define (second-node graph)
  (second (first graph)))

(define (weight graph)
  (third (first graph)))

(define (nodes graph)
  (remove-duplicates (flatten-graph graph)))


(define (flatten-graph graph)
  (if (equal? 1 (length graph))
      (cons (cons (first-node graph)) (second-node graph))
      (cons (cons (cons (first-node graph)) (second-node graph)) (flatten-graph (rest graph))))) 