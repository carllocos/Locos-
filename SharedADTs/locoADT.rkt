#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loco ADT
;;
;; This ADT represents a locomotive and is produced based on the two Stations ID's.
;; The ID's represent the track where the loco is standing.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide new-loco
         loco-type?)


(define loco-type 'loco)

(define (loco-type? obj)
  (and (procedure? obj)
       (eq? loco-type (obj 'get-type))))
            

(define (display-loco loco)
  (newline)
  (display "Loco ")
  (display (loco 'get-id))
  (display " oriented from ")
  (display (loco 'get-id-n1))
  (display " - ")
  (display (loco 'get-id-n2))
  (display "  with speed ")
  (display (loco 'get-speed))
  (display " distance made ")
  (display (loco 'get-distance))
  (newline))
  


(define (new-loco id id-n1 id-n2)
  (define distance 0)
  (define speed 0)
  (define (set-id-n1! new)
    (set! id-n1 new))
  (define (set-id-n2! new)
    (set! id-n2 new))
  (define (set-speed! new)
    (set! speed new))
  (define (set-distance! new)
    (set! distance new))
  (lambda(msg)
    (cond((eq? msg 'get-id) id)
         ((eq? msg 'get-id-n1) id-n1)
         ((eq? msg 'get-id-n2) id-n2)
         ((eq? msg 'set-id-n1!) set-id-n1!)
         ((eq? msg 'set-id-n2!) set-id-n2!)
         ((eq? msg 'get-distance) distance)
         ((eq? msg 'set-distance!) set-distance!)
         ((eq? msg 'get-speed) speed)
         ((eq? msg 'set-speed!)set-speed!)
         ((eq? msg 'get-type) loco-type)
         (else (error "unknown message--loco: " msg)))))