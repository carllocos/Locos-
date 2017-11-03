#lang racket

(require (prefix-in loco: Project/SharedADTs/locoADT))
(require (prefix-in spot: Project/SharedADTs/StationADT))
(require (prefix-in switch: Project/SharedADTs/switchADT))
(require (prefix-in rs: Project/SharedADTs/railsADT))
(require (prefix-in sig: Project/SharedADTs/signalADT))
(require (prefix-in lg: Project/ExternLib/box-weighted-graph))
(require (prefix-in tb: Project/SharedADTs/tableADT))

(provide new-rwm
     
         get-loco
         get-node
         get-track
         get-switch
         get-signal
         get-graph
         
         get-list-locos
         get-list-switchs
         get-list-signals
         get-list-nodes
         get-list-tracks

         switch?

         tracks-belonging-to-node

         detection-hardware-id->logic-id
         detection-logic-id->hardware-id
         switch-logic-id->switch-hardware-id
         
         )


(define rwm-copy '())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Railway-model ADT
;;
;; This ADT will combine every component that compose a railway. Keeps also tables of switchs, loco's and signals, for easy acces.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-storage)
  (vector (tb:make-table) ;locos
          (tb:make-table) ;switchs
          (tb:make-table) ; signals
          '()))

(define (new-rwm filename)
  (define rwm (make-storage))
  (initiate-rwm rwm filename)
  (set! rwm-copy rwm)
  rwm)


(define (get-locos rwm) (vector-ref rwm 0))

(define (get-list-locos rwm)
  (tb:table-all-values (get-locos rwm)))

(define (get-switchs rwm) (vector-ref rwm 1))

(define (get-list-switchs rwm)
  (tb:table-all-values (get-switchs rwm)))

(define (get-list-signals rwm)
  (tb:table-all-values (get-signals rwm)))

(define (get-signals rwm) (vector-ref rwm 2))

(define (get-graph rwm) (vector-ref rwm 3))

(define (set-graph! rwm g) (vector-set! rwm 3 g))

;;getters and setters for one single object

(define (get-switch rwm id)
  (tb:table-get-element (get-switchs rwm) id))

(define (get-loco rwm id)
  (tb:table-get-element (get-locos rwm)  id))

(define (get-signal rwm id)
  (tb:table-get-element (get-signals rwm)  id))

(define (get-node rwm id)
  (let((g (get-graph rwm)))
        (lg:node-box g (lg:node-index g id))))


(define (get-list-nodes rwm)
  (define g (get-graph rwm))
  (define order (- (lg:order g) 1) )
  (let loop ((result '())
             (index order))
    (if (< index 0)
        (loop (cons (lg:node-box g index) result)
              (- index 1))
        result)))

(define (get-list-tracks rwm)
  (define g (get-graph rwm))
  (define order (lg:order g))
  (define used '())
  (lg:for-each-node g
                    (lambda (n-ind b)
                        (lg:for-each-edge
                         g
                         n-ind
                         (lambda (n2-ind t w)
                           (unless (member t used (lambda ( t t2)
                                                    (rs:same-track? t t2)))
                             (set! used (cons t used)))))))
  used)



(define (get-track rwm node1-id node2-id)
  (let*((g (get-graph rwm))
        (node2-i (lg:node-index g node2-id))
        (edge #f))
    (lg:for-each-edge g
                      (lg:node-index g node1-id)
                      (lambda(index box weight)
                        (when (= index node2-i)
                          (set! edge box))))
    edge))

(define (tracks-belonging-to-node rwm node-id)
  (lg:get-neighbours (get-graph rwm) node-id))

(define (add-loco rwm id loco)
  (tb:table-add-element (get-locos rwm) id loco))

(define (add-switch rwm id switch)
  (tb:table-add-element (get-switchs rwm) id switch))

(define (add-signal rwm id s)
  (tb:table-add-element (get-signals rwm) id s))



;test if the two tracks belong to the same switch
; returns false or the switch
(define (switch? rwm track1 track2)
  (if (and track1
           track2
           (rs:switch-type? track1)
           (rs:switch-type? track2))
      (let((switch-id (rs:node-in-common track1 track2)))
        (and switch-id (get-switch rwm switch-id)))
      #f))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;This part is used only for connection with hardware;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define modulo-1 1)
(define modulo-2 2)
(define modulo-size 8)

;maps a hardware modulo address to a logic symbol id
(define (detection-hardware-id->logic-id nr-modulo det-nr)
  (if (= nr-modulo modulo-1)
      (string->symbol (number->string det-nr))
       (string->symbol (number->string (+ det-nr modulo-size)))))

;maps from a logic symbol id to a pair (modulo-nr . detection-nr)
(define (detection-logic-id->hardware-id id)
  (let ((id-nr ( string->number (symbol->string id))))
  (if (> id-nr modulo-size)
      (cons modulo-2 (- id-nr modulo-size)) 
       (cons modulo-1 id-nr ))))

(define (switch-logic-id->switch-hardware-id id)
  (define id-nr (string->number (symbol->string id)))
  (define l (filter (lambda(lst)
                      (= (car lst) id-nr))
                    switch-adress-list))
  (cadr (car l)))

(define switch-adress-list
  (list
   (list 1 "00")
   (list 2 "02")
   (list 3 "02")
   (list 4 "03")
   (list 5 "04")
   (list 6 "04")
   (list 7 "06")
   (list 8 #f)
   (list 9 "00")
   (list 10 "01")
   (list 11 "02")
   (list 12 "03")
   (list 13 #f)
   (list 14 #f)
   (list 15 #f)
   (list 16 "03")
   (list 17 #f)
   (list 18 #f)
   (list 19 #f)
   (list 20 "07")
   (list 21 #f)
   (list 22 #f)
   (list 23 "06")
   (list 24 #f)
   (list 25 "00")
   (list 26 "01")
   (list 27 "02")
   (list 28 "03")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Hulp procedures;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ls '())
(define ns '())
(define ss '())
(define ts '())
(define ds '())
(define amount-nodes 0)


(define (populate-lists filename)
  
  (define lines (file->lines filename))
  (define string-list (let loop ((p (- (length lines)1)) (result '()))
                        (if (>= p 0)
                            (loop (- p 1)
                                  (append (string-split (list-ref lines p))
                                          result))
                            result)))

  (define leng (length string-list))
  (let loop ((position 0))
    (when (< position leng)
      
      (let ((type  (string->symbol (list-ref string-list position))))
        (case type
          [(L) (let* ([lid (string->symbol (list-ref string-list (+ position 1)))]  
                      [n1 (string->symbol (list-ref string-list (+ position 2)))]
                      [n2 (string->symbol (list-ref string-list (+ position 3)))]
                      [res (loco:new-loco lid n1 n2)])
           
                 (set! ls (cons res ls))
                 (loop (+ position 4)))]
          [(N) (let* ([id (string->symbol (list-ref string-list (+ position 1)))] 
                      [x (string->number (list-ref string-list (+ position 2)))]
                      [y (string->number (list-ref string-list (+ position 3)))]
                      [res (spot:new id x y)])
           
                 
                 (set! amount-nodes (+ amount-nodes 1))
                 (set! ns (cons res ns))
                 (loop (+ position 4)))]
          [(S) (let* ([nm (string->symbol (list-ref string-list (+ position 1)))]  
                      [n0 (string->symbol (list-ref string-list (+ position 2)))]
                      [n1 (string->symbol (list-ref string-list (+ position 3)))]
                      [n2 (string->symbol (list-ref string-list (+ position 4)))]
                      [res (switch:new-switch nm n0 n1 n2)])
           
                 (set! ss (cons res ss))
                 (loop (+ position 5)))]
          [(T) (let* ([n1 (string->symbol (list-ref string-list (+ position 1)))] 
                      [n2 (string->symbol (list-ref string-list (+ position 2)))]
                      [res (rs:new-track n1 n2)])
              
                 (set! ts (cons res ts))
                 (loop (+ position 3)))]
          [(D) (let* ([id (string->symbol (list-ref string-list (+ position 1)))]    
                      [n1 (string->symbol (list-ref string-list (+ position 2)))]
                      [n2 (string->symbol (list-ref string-list (+ position 3)))]
                      [res (rs:new-detection id n1 n2)]
                      [signal (sig:new-signal id)])
            
                 (set! ds (cons res ds))
                 (loop (+ position 4)))])))))



;;procedure to calculate the distance between two nodes
;;the distance repsresents the weight of a track
(define (calculate-weight n1 n2)
  (let ((x1 (n1  'get-x-location))
        (x2 (n2 'get-x-location))
        (y1 (n1  'get-y-location))
        (y2 (n2  'get-y-location)))
    (sqrt (+ (expt (- x1 x2) 2)
             (expt (- y1 y2) 2)))))

(define (initiate-rwm rwm filename)
  (populate-lists filename)
  (define graph (lg:new amount-nodes #f))
  

  (for-each (lambda(obj)
              (add-loco rwm (obj 'get-id) obj))
            
            ls)
  
  (define node-i 0)
  (for-each (lambda (obj)
              (lg:node-box! graph node-i obj (obj 'get-id))
              (set! node-i (+ node-i 1)))
            ns)


  (for-each (lambda(obj)
              (let*((node1-i (lg:node-index graph (obj 'get-id-n1)))
                    (node2-i (lg:node-index graph (obj 'get-id-n2)))
                    (node1 (lg:node-box graph node1-i))
                    (node2 (lg:node-box graph node2-i))
                    (weight (calculate-weight node1 node2)))
                ((obj 'set-length!) weight)
                (lg:add-edge! graph node1-i node2-i obj weight)))
            ts)
  

  (for-each (lambda(obj)
              (let*((node-index1 (lg:node-index graph (obj 'get-id-n1)))
                    (node-index2 (lg:node-index graph (obj 'get-id-n2)))
                    (node1 (lg:node-box graph node-index1))
                    (node2 (lg:node-box graph node-index2))
                    (weight (calculate-weight node1 node2)))
                ((obj 'set-length!) weight)
                (add-signal rwm (obj 'get-id) (sig:new-signal (obj 'get-id)))
                (lg:add-edge! graph node-index1 node-index2 obj weight)))
            ds)
  
        
  (for-each (lambda(obj)
              (let*((obj-id (obj 'get-id-mn))
                    (nm-i (lg:node-index graph obj-id))
                    (n0-i (lg:node-index graph (obj 'get-id-n0)))
                    (n1-i (lg:node-index graph (obj 'get-id-n1)))
                    (n2-i (lg:node-index graph (obj 'get-id-n2)))
                    (node-nm (lg:node-box graph nm-i))
                    (node-n0 (lg:node-box graph n0-i))
                    (node-n1 (lg:node-box graph n1-i))
                    (node-n2 (lg:node-box graph n2-i))
                    (track0 (rs:new-switch-track (obj 'get-id-n0) obj-id))
                    (track1 (rs:new-switch-track obj-id (obj 'get-id-n1)))
                    (track2 (rs:new-switch-track obj-id (obj 'get-id-n2)))
                    (weight0 (calculate-weight node-n0 node-nm))
                    (weight1 (calculate-weight node-nm node-n1))
                    (weight2 (calculate-weight node-nm node-n2)))
                
                (lg:add-edge! graph nm-i n1-i track1 weight1)
                (lg:add-edge! graph n0-i nm-i track0 weight0)
                (lg:add-edge! graph nm-i n2-i track2 weight2)
                
                ((track0 'set-length!) weight0)
                ((track1 'set-length!) weight1)
                ((track2 'set-length!) weight2)
                
                ((obj 'set-track-0!) track0)
                ((obj 'set-track-1!) track1)
                ((obj 'set-track-2!) track2)
                (add-switch rwm obj-id obj)))
        ss)
  
    (set-graph! rwm graph)
  
  (for-each (lambda(obj)
              (let ((d (get-track rwm (obj 'get-id-n1) (obj 'get-id-n2))))
                ((d 'reserve!) (obj 'get-id))))
            ls)
  )