#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Station ADT
;;
;; This ADT represent a spot in the end of a track
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide new
         )


(define (new id-name x y)
  (define (set-x! new) (set! x new))
  (define (set-y! new) (set! y new))
  (lambda(msg)
    (cond((eq? msg 'get-x-location) x)
         ((eq? msg 'get-y-location) y)
         ((eq? msg 'get-id) id-name)
         (else (error "unknown msg-- spot ADT : " msg)))))

(define (display-node n)
  (displayln (string-append "node id:" (symbol->string (n 'get-id))))
  (displayln (string-append " x pos:"
                             (number->string (n 'get-x-location) )
                             "  y-pos:"
                             (number->string (n 'get-y-location)))))