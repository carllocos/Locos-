#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switch ADT
;;
;; This ADT represent a switch.
;; Is composed by four station ID's.
;; and keeps also a reference to the tracks that compose the switch
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (prefix-in rs: Project/SharedADTs/railsADT))
(provide new-switch
         )



(define (display-switch s)
  (displayln (string-append "switch nm:"
                            (symbol->string (s 'get-id-mn))
                            "  n0:"
                            (symbol->string (s 'get-id-n0))
                            "  n1:"
                            (symbol->string (s 'get-id-n1))
                            "  n2:"
                            (symbol->string (s 'get-id-n2)))))


(define (new-switch id-mn id-n0 id-n1 id-n2)
  (define position-switch 1)
  (define track0 '())
  (define track1 '())
  (define track2 '())
  (define reserved-by #f)

  (define (set-track-0! t)
    (set! track0 t))
  (define (set-track-1! t)
    (set! track1 t))
  (define (set-track-2! t)
    (set! track2 t))
  
  (define (set-position! pos)
    (set! position-switch pos))

  ;test if the track is part of this switch adn returns the track number
  (define (which-track track)
    (cond((rs:same-track? track track0) 0)
         ((rs:same-track? track track1) 1)
         ((rs:same-track? track track2) 2)
         (else #f)))

  (define (reserve! id)
    (set! reserved-by id))

  (define (free!)
    (set! reserved-by #f))
  
  (lambda(msg)
    (cond((eq? msg 'get-id-mn) id-mn)
         ((eq? msg 'get-id-n0) id-n0)
         ((eq? msg 'get-id-n1) id-n1)
         ((eq? msg 'get-id-n2) id-n2)
         ((eq? msg 'get-track-0) track0)
         ((eq? msg 'get-track-1) track1)
         ((eq? msg 'get-track-2) track2)
         ((eq? msg 'set-track-0!) set-track-0!)
         ((eq? msg 'set-track-1!) set-track-1!)
         ((eq? msg 'set-track-2!) set-track-2!)
         ((eq? msg 'get-position) position-switch)
         ((eq? msg 'set-position!) set-position!)
         ((eq? msg 'which-track) which-track)
         ((eq? msg 'reserve!) reserve!)
         ((eq? msg 'free?) (not reserved-by))
         ((eq? msg 'reserved-by?) reserved-by)
         ((eq? msg 'free!) (free!))
         (else (error "message-switch-unknown: " msg)))))