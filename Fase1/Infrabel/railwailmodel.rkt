#lang racket
(require "locoADT"
         "trainStopADT"
         "switch"
         "rail-segmentADT"
         "signalADT"
         "label-unweighted-graph")



(provide new-rwm
         get-graph
         get-locos
         get-switchs
         get-nodes
         get-tracks
         get-detections
         get-signals
         give-node
         give-index)

(define make-table make-hash)
(define table-get-element hash-ref)
(define table-add-element hash-set!)

(define WEIGHT 500)
;;;;;;HULP ADT's  
(define (make-storage) (vector '() (make-table) (make-table) (make-table) (make-table) (make-table) (make-table)))
  ;Dit storage zal het railwaymodel voorstellen met als elementen respk. graph locos-, nodes-, switchs-, tracks-,
  ;detections- en signals table

(define (get-graph rwm) (vector-ref rwm 0))
(define (get-locos rwm) (vector-ref rwm 1))
(define (get-nodes rwm) (vector-ref rwm 2))
(define (get-switchs rwm) (vector-ref rwm 3))
(define (get-tracks rwm) (vector-ref rwm 4))
(define (get-detections rwm) (vector-ref rwm 5))
(define (get-signals rwm) (vector-ref rwm 6))

(define (set-graph! rwm g) (vector-set! rwm 0 g))
;(define (set-locos! rwm l) (vector-set! rwm 1 l))
;(define (set-nodes! rwm n) (vector-set! rwm 2 n))
;(define (set-switchs! rwm s) (vector-set! rwm 3 s))
;(define (set-tracks! rwm t) (vector-set! rwm 4 t))
;(define (set-detections! rwm d) (vector-set! rwm 5 d))
;(define (set-signals! rwm s) (vector-set! rwm 6 s))

;;;ADT's voor de tables
(define (new-node-index node index) (cons node index))
(define (give-node p) (car p))
(define (give-index p) (cdr p))

(define (get-loco rwm id) (table-get-element (get-locos rwm) id))
(define (get-node-index rwm id) (give-index (table-get-element (get-nodes rwm) id)))
(define (get-node rwm id) (give-node (table-get-element (get-nodes rwm) id)))
(define (get-switch rwm id) (table-get-element (get-switchs rwm) id))
(define (get-track rwm id) (table-get-element (get-tracks rwm) id))
(define (get-detection rwm id) (table-get-element (get-detections rwm) id))
(define (get-signal rwm id) (table-get-element (get-signals rwm) id))

(define (add-loco rwm id loco) (table-add-element (get-locos rwm) id loco))
(define (add-node-AND-index rwm id node graph-index)
  (table-add-element (get-nodes rwm) id (new-node-index node graph-index)))
(define (add-switch rwm id switch) (table-add-element (get-switchs rwm) id switch))
(define (add-track rwm id track) (table-add-element (get-tracks rwm) id track))
(define (add-detection rwm id d) (table-add-element (get-detections rwm) id d))
(define (add-signal rwm id s) (table-add-element (get-signals rwm) id s))

(define (new-rwm filename)
  (initiete-rwm (make-storage) filename))

;;;;;;;HULP Procedure om de railwaymodel te initialiseren
(define (initiete-rwm rwm filename)  
  (define lines (map string-split (file->lines filename)))
  (define graph '())
  (define ls (make-table))
  (define ns '())
  (define amount-n 0)
  (define ss '())
  (define ts '())
  (define ds '())
  (define sigs '());;;Sigs stelt de lijst van signals voor en de aantal is evengroot als de aantal detections
  
  (define (add-nodes)
    (let loop ((node-index 0) (current ns))
      (when (not (null? current))
        (add-node-AND-index rwm ((car current) 'get-id) (car current) node-index)
        (label! graph node-index ((car current) 'get-id))
        (loop (+ node-index 1) (cdr current)))))

  (define (add-tracks)
    ;een negatieve id wordt gemaakt voor elke track.
    ;Via zijn Id kan de track uit een track-table gehaald worden
    (let loop ((current ts) (id -1))
      (when (not (null? current))
        (let* ((t (car current))
               (node-index1 (get-node-index rwm (t 'get-n1-id)))
               (node-index2 (get-node-index rwm (t 'get-n2-id))))
          (add-edge! graph node-index1 node-index2 id WEIGHT)
          (add-track rwm id t))
        (loop (cdr current) (- id 1)))))
  (define (add-detections)
    (let loop ((current-d ds) (current-s sigs))
      (when (not (null? current-d))
        (let*((d (car current-d))
              (s (car current-s))
              (t (d 'get-track))
              (node-index1 (get-node-index rwm (t 'get-n1-id)))
              (node-index2 (get-node-index rwm (t 'get-n2-id))))
          (add-edge! graph node-index1 node-index2 (d 'get-id) WEIGHT)
          (add-detection rwm (d 'get-id) d)
          (add-signal rwm (d 'get-id)  s))
        (loop (cdr current-d) (cdr current-s)))))
  (define (add-switchs)
    (let loop ((current ss))
      (when (not (null? current))
        (let* ((s (car current))
               (nm-i (get-node-index rwm (s 'get-id-mn)))
               (n0-i (get-node-index rwm (s 'get-id-n0)))
               (n1-i (get-node-index rwm (s 'get-id-n1)))
               (n2-i (get-node-index rwm (s 'get-id-n2))))
          (add-edge! graph nm-i n1-i 's WEIGHT)
          (add-edge! graph n0-i nm-i 's WEIGHT)
          (add-edge! graph nm-i n2-i 's WEIGHT)
          (add-switch rwm (s 'get-id-mn) s))
        (loop (cdr current)))))
        
  
  (for-each
   (lambda (l)
     (case (string->symbol (car l))
       [(L) (let* ([lid (string->symbol (list-ref l 1))]  
                   [n1 (string->symbol (list-ref l 2))]
                   [n2 (string->symbol (list-ref l 3))]
                   [res (new-loco lid n1 n2)])
              (add-loco rwm (res 'get-id) res)
              )]
       [(N) (let* ([id (string->symbol (list-ref l 1))] 
                   [x (string->number (list-ref l 2))]
                   [y (string->number (list-ref l 3))]
                   [res (new-node id x y)])
              (set! ns (cons res ns))
              (set! amount-n (+ amount-n 1)))]
       [(S) (let* ([nm (string->symbol (list-ref l 1))]  
                   [n0 (string->symbol (list-ref l 2))]
                   [n1 (string->symbol (list-ref l 3))]
                   [n2 (string->symbol (list-ref l 4))]
                   [res (new-switch nm n0 n1 n2)])
              (set! ss (cons res ss))
              )]
       [(T) (let* ([n1 (string->symbol (list-ref l 1))] 
                   [n2 (string->symbol (list-ref l 2))]
                   [res (new-track n1 n2)])
              (set! ts (cons res ts))
              )]
       [(D) (let* ([id (string->symbol (list-ref l 1))]    
                   [n1 (string->symbol (list-ref l 2))]
                   [n2 (string->symbol (list-ref l 3))]
                   [res (new-detection id n1 n2)]
                   [signal (new-signal id)])
              (set! ds (cons res ds))
              (set! sigs (cons signal sigs))
              )]))
   lines)
  (set! graph (new-weighted-label-graph amount-n #f))
  
  (add-nodes)
  (add-tracks)
  (add-detections)
  (add-switchs)
  (set-graph! rwm graph)
  rwm)


(define rwm (new-rwm "be_simple.txt"))
rwm
(define g (get-graph rwm))

(get-node rwm 'Bg)
(display-g g)