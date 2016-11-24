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
(provide new-weighted-label-graph
         delete-edge!
         order
         nr-of-edges
         directed?
         label
         label!
         for-each-node
         for-each-edge
         adjacent?
         add-edge!
         delete-edge!
         display-g
         )


(define set-cdr! set-mcdr!)
(define cons mcons)
(define car mcar)
(define cdr mcdr)


    
(struct graph (order directed? [nr-of-edges #:mutable] storage labels))

(define (new-weighted-label-graph order directed)
  (graph order directed 0 (make-vector order '()) (make-vector order #f)))

(define (order g) (graph-order g))

(define (nr-of-edges g) (graph-nr-of-edges g))
(define (nr-of-edges! g new) (set-graph-nr-of-edges! g new))

(define (directed? g) (graph-directed? g))

(define (label g i) (vector-ref (graph-labels g) i))
(define (label! g i l) (vector-set! (graph-labels g) i l))
(define (for-each-node g f)
  (define amount (graph-order g))
  (define labels (graph-labels g))
  (let loop ((i 0))
    (when (< i amount)
      (f i (vector-ref labels i))
      (loop (+ i 1)))))

;;;;;;Hulp ADT dat dient als element in de adjacentielijst van een node. De buur van een node
(define (make-neighbour index label weight)
  (vector index label weight))
(define (get-weight ng)
  (vector-ref ng 2))
(define (get-label ng)
  (vector-ref ng 1))
(define (get-index ng)
  (vector-ref ng 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (for-each-edge g from proc)
    (define neighbours (graph-storage g))
    (let iter-edges
      ((edges (vector-ref neighbours from)))
      (when (not (null? edges))
        (proc  (get-index (car edges)) (get-label (car edges)) (get-weight (car edges)))
        (iter-edges (cdr edges)))))

(define (adjacent? g from to)
   (define lists (graph-storage g))
   (let search-sorted
     ((current (vector-ref lists from)))
     (cond 
       ((or (null? current)
            (< (get-index (car current)) to))
        #f)
       ((= (get-index (car current)) to)
        #t)
       (else
        (search-sorted (cdr current))))))

(define (add-edge! g from to symbol weight)
   (define lists (graph-storage g))
   (define (insert-sorted to prev next! next symbol weight)
     (cond 
       ((or (null? next)
            (> to (get-index (car next))))
        (next! prev (cons (make-neighbour to symbol weight) next))
        #t)
       ((= to (get-index (car next)))
        #f)
       (else
        (insert-sorted to next set-cdr! (cdr next) symbol weight))))
   (define (head-setter head) 
     (lambda (ignore next)
       (vector-set! lists head next)))
   (when (insert-sorted to '() (head-setter from) (vector-ref lists from) symbol weight)
     (nr-of-edges! g (+ 1 (nr-of-edges g))))
   (when (not (graph-directed? g))
     (insert-sorted from '() (head-setter to) (vector-ref lists to) symbol weight)))


(define (delete-edge! g from to)
   (define lists (graph-storage g))
   (define (delete-sorted to prev next! next)
     (cond
       ((or (null? next)
            (> to (get-index (car next))))
        #f)
       ((= to (get-index (car next)))
        (next! prev (cdr next))
        #t)
       (else
        (delete-sorted to next set-cdr! (cdr next)))))
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
             (display "node: ") (display ctr) (display "  label:") (display (label g ctr))
             (display "  buren: ") (display (vector-ref storage ctr)) (newline)
             (loop (+ ctr 1)))))

  
;(define g (new-weighted-label-graph 4 #f))
;(label! g 2 'Gent)
;(label! g 1 'Brussel)
;(label! g 0 'Zaventem)
;(label! g 3 'Leuven)

;(add-edge! g 2 1 'hey '20km)
;(add-edge! g 1 0 'Wow '40km)
;(add-edge! g 0 3 'Ha '600km)
;(add-edge! g 1 3 'lol '10km)

;(display-g g)
;(adjacent? g 2 1)
;(adjacent? g 2 3)

;(order g)
;(nr-of-edges g)
;(directed? g)

;(for-each-edge g 1 (lambda(i l w)
  ;                   (display "n: ") (display i) 
  ;                   (display "  l:") (display l)  
  ;                   (display "  w:") (display w) (newline)))