#lang racket
(require "graph.rkt")
(require "visual.rkt")

;data structure for a node
(define-struct node (cost path) #:transparent)

;the main algorithm
(define (dijkstra graph start finish)

  ;set initial cost table
  (define initial (costs-hash graph start))

  ;recursively go through all nodes, building the cost table along the way
  (define (solve-dijkstra current remaining costs-table)
    (if (equal? current finish) 
        
        costs-table 
        
        (let* ([current-node      (hash-ref costs-table current)]
               [current-node-cost (node-cost current-node)]
               [neighbours        (hash-ref remaining current)])
          
          (define-values [costs-table-new current-new _]
            (for/fold ([costs costs-table]
                       [min-neighbour #f]
                       [min-val +inf.0])
                      ([n neighbours]
                       #:when (hash-has-key? remaining (first n)))
              
              (define-values [neighbour-name edge-cost] (values (first n) (second n))) 
              
              (define new-cost-neighbour (+ current-node-cost edge-cost))
              (define prev-cost-neighbour (node-cost (hash-ref costs neighbour-name)))
              
              (define-values [min-neighbour-new min-val-new]
                (if (< new-cost-neighbour min-val)
                    (values (first n) new-cost-neighbour)
                    (values min-neighbour min-val)))
              
              (define new-costs
                (if (< new-cost-neighbour prev-cost-neighbour)
                    (hash-update costs 
                                 neighbour-name
                                 (lambda (nd) 
                                   (struct-copy node nd 
                                                (cost new-cost-neighbour)
                                                (path (cons current (node-path current-node))))))
                    costs))
              
              (values new-costs min-neighbour-new min-val-new)))
          
          (solve-dijkstra current-new 
                          (hash-remove remaining current) 
                          costs-table-new)
          )))
  (solve-dijkstra start graph initial)) 


;sets the initial values for the cost table (infinity for everything but the initial node)
(define (costs-hash graph start) 
  (make-immutable-hash 
   (map (lambda (n) (cons n (make-node (if (equal? start n) 0 +inf.0) '())))
        (hash-keys graph))))


