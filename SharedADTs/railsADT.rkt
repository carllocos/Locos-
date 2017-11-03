#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Rails ADT
;;
;; is a general concept for every type of track that we find in a railwaymodel.
;; The different types:
;;   detection track: track that corresponds with a detection block. For this one we will also create a signal ADT
;;   switch track: track that belongs to a switch ADT.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide new-track
         new-detection
         new-switch-track
         rail-type?
         detection-type?
         switch-type?
         track-type?
         same-track?
         node-in-common
         )


(define track-type 'track-edge)
(define detection-type 'detection-edge)
(define switch-type 'switch-edge)

(define (same-track? t1 t2)
  (let ((n1 (t1 'get-id-n1)) (n2 (t1 'get-id-n2)))
    (or (and (eq? n1 (t2 'get-id-n1)) (eq? n2 (t2 'get-id-n2)))
        (and (eq? n2 (t2 'get-id-n1)) (eq? n1 (t2 'get-id-n2))))))

(define (node-in-common track1 track2)
  (let((n1 (track1 'get-id-n1))
       (n2 (track1 'get-id-n2)))
    (cond((eq? n1 (track2 'get-id-n1)) n1)
         ((eq? n1 (track2 'get-id-n2)) n1)
         (else n2))))

(define (rail-type? object)
  (when (procedure? object)
    (let ((type (object 'get-type)))
      (or (eq? type track-type)
          (eq? type detection-type)
          (eq? type switch-type)))))

(define (detection-type? object)
  (when object
    (let ((type (object 'get-type)))
      (eq? type detection-type))))

(define (track-type? object)
  (when object
    (let ((type (object 'get-type)))
      (eq? type track-type))))

(define (switch-type? object)
  (when object
    (let ((type (object 'get-type)))
      (eq? type switch-type))))

(define (display-track t)
  (newline)
  (display "track: ")
  (display (t 'get-id-n1))
  (display " - ")
  (display (t 'get-id-n2))
  (display "  type:")
  (display (t 'get-type))
  (display "  free?")
  (display (t 'free?))
  (display "  reserved-by?")
  (displayln (t 'reserver)))



(define (new-rail id-n1 id-n2 type)
  (define length 0)
  (define reserved? #f)
  (define reserver-id #f)
  (define (reserve! new-reserver)
    (set! reserved? #t)
    (set! reserver-id new-reserver)
    )
  (define (free!)
    (set! reserved? #f)
    (set! reserver-id #f))

  (define (reserved-by? loco-id)
    (eq? loco-id reserver-id))
  
  (define (set-length! l)
    (set! length l))

  (define (dispatch msg)
    (cond((eq? msg 'get-id-n1) id-n1)
         ((eq? msg 'get-id-n2) id-n2)
         ((eq? msg 'reserve!) reserve!)
         ((eq? msg 'free?) (not reserved?))
         ((eq? msg 'free!) (free!))
         ((eq? msg 'get-type) type)
         ((eq? msg 'get-length) length)
         ((eq? msg 'set-length!) set-length!)
         ((eq? msg 'reserved-by?) reserved-by?) 
         (else (error "unknown msg -- " type msg id-n1 id-n2))))
  dispatch)

(define (new-track id-n1 id-n2)
  (new-rail id-n1 id-n2 track-type))

(define (new-switch-track id-n1 id-n2)
  (new-rail id-n1 id-n2 switch-type))

(define (new-detection id id-n1 id-n2)
  (define rail (new-rail id-n1 id-n2 detection-type))
  (define backtrack-use #f)
  (define (backtrack-use! loco-id)
    (set! backtrack-use loco-id))
  (define (free-backtrack-use!)
    (set! backtrack-use #f))
  (define (dispatch msg)
    (cond((eq? msg 'get-id) id)
         (else (rail msg))))
  dispatch)