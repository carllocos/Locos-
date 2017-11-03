#lang racket

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*       Unweighted Graphs (Adjacency List Representation)         *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2009  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(require scheme/mpair)
(provide new
         delete-edge!
         order
         nr-of-edges
         directed?
         node-box
         node-index
         node-box!
         for-each-node
         for-each-edge
         adjacent?
         add-edge!
         delete-edge!
         display-g
         get-neighbours
       
         )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Box Weighted Graph is simply a node- and edge labeled weighted graph.
;;
;;     For each node index i we will store:
;;            1. In storage we keep the neighbours:  neighbour is a pair (neighbour-index, box, weight)
;;                Box can be anything the user want, I use it to store the track between i and neighbour
;;            2.In boxes vector we keep information about node i: a pair (box, parent-index)
;;                box is a way of keeping information about the node. Parent-index is the index of the parent of this node i. Its
;;                usefull when the graph is directed, to backtrack.
;;            3. Indexes is a hash-table where the key is the label of a node and the value his associeted index in the graph:  ex.(label, 7)
;;               This table is usefull if we want to access information from the graph based on labels.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(struct graph (order directed? [nr-of-edges #:mutable] storage boxes indexes))

(define (new order directed)
  (graph order directed 0 (make-vector order '()) (make-vector order #f) (make-hash)))


;;;Getters & Setters
(define (order g) (graph-order g))

(define (nr-of-edges g) (graph-nr-of-edges g))
(define (nr-of-edges! g new) (set-graph-nr-of-edges! g new))

(define (directed? g) (graph-directed? g))
(define (node-box g index)
  (vector-ref (graph-boxes g) index))

(define (node-index g id)
  (hash-ref (graph-indexes g) id))

(define (node-box! g node-index box id)
  (vector-set! (graph-boxes g) node-index box)
  (hash-set! (graph-indexes g) id node-index))


(define (for-each-node g f)
  (define amount (graph-order g))
  (define boxes (graph-boxes g))
  (let loop ((i 0))
    (when (< i amount)
      (f i (vector-ref boxes i))
      (loop (+ i 1)))))

;;;;;;Small ADT to keep information about a neighbour
(define (make-neighbour index box weight)
  (vector index box weight))
(define (get-weight ng)
  (vector-ref ng 2))
(define (get-box ng)
  (vector-ref ng 1))
(define (get-index ng)
  (vector-ref ng 0))



(define (get-neighbours g id)
  (define index id)
  (unless (number? id)
    (set! index (node-index g id)))
  (define neighbourhood (graph-storage g))
  (define neighbours (vector-ref neighbourhood index))
  (let loop(( lst neighbours) (result '()))
    (if (null? lst)
        result
        (loop (mcdr lst) (cons (get-box (mcar lst)) result)))))
    
    
(define (for-each-edge g from proc)
  (define neighbours (graph-storage g))
  (let iter-edges
    ((edges (vector-ref neighbours from)))
    (when (not (null? edges))
      (proc  (get-index (mcar edges)) (get-box (mcar edges)) (get-weight (mcar edges)))
      (iter-edges (mcdr edges)))))

(define (adjacent? g from to)
  (define lists (graph-storage g))
  (let search-sorted
    ((current (vector-ref lists from)))
    (cond 
      ((or (null? current)
           (< (get-index (mcar current)) to))
       #f)
      ((= (get-index (mcar current)) to)
       #t)
      (else
       (search-sorted (mcdr current))))))

(define (add-edge! g from to box weight)
  (define lists (graph-storage g))
  (define (insert-sorted to prev next! next box weight)
    (cond 
      ((or (null? next)
           (> to (get-index (mcar next))))
       (next! prev (mcons (make-neighbour to box weight) next))
       #t)
      ((= to (get-index (mcar next)))
       #f)
      (else
       (insert-sorted to next set-mcdr! (mcdr next) box weight))))
  (define (head-setter head) 
    (lambda (ignore next)
      (vector-set! lists head next)))
  (when (insert-sorted to '() (head-setter from) (vector-ref lists from) box weight)
    (nr-of-edges! g (+ 1 (nr-of-edges g))))
  (when (not (graph-directed? g))
    (insert-sorted from '() (head-setter to) (vector-ref lists to) box weight)))


(define (delete-edge! g from to)
  (define lists (graph-storage g))
  (define (delete-sorted to prev next! next)
    (cond
      ((or (null? next)
           (> to (get-index (mcar next))))
       #f)
      ((= to (get-index (mcar next)))
       (next! prev (mcdr next))
       #t)
      (else
       (delete-sorted to next set-mcdr! (mcdr next)))))
  (define (head-setter head) 
    (lambda (ignore next)
      (vector-set! lists head next)))
  (when (delete-sorted to '() (head-setter from) (vector-ref lists from))
    (nr-of-edges! g (- (nr-of-edges g) 1)))     
  (when (not (directed? g))
    (delete-sorted from '() (head-setter to) (vector-ref lists to))))


(define (display-g g)
  (define storage (graph-storage g))
  (let loop ((ctr 0))
    (when (< ctr (vector-length storage))
      (display ((node-box g ctr) 'get-id))
      (display " index=")
      (display ctr)
      (newline)
      (for-each-edge g
                     ctr
                     (lambda(neigh box weight)
                       (display "   neighbour: ")
                       (display ((node-box g neigh) 'get-id))
                       (display "  box: ")
                       (display (box 'get-type))
                       (display " weight:")
                       (display weight)
                       (newline)))
      (loop (+ ctr 1))))
  (newline))