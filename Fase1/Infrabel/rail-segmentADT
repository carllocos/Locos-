#lang racket


(provide new-track
         new-detection)

(define (new-track id-n1 id-n2)
  (define used #f)
  (define (used!) (set! used #t))
  (define (free!) (set! used #f))
  (lambda(msg)
    (cond((eq? msg 'get-n1-id) id-n1)
         ((eq? msg 'get-n2-id) id-n2)
         ((eq? msg 'used?) used)
         ((eq? msg 'used!) (used!))
         ((eq? msg 'free!) (free!))
         (else (error "unknown msg -- track")))))


(define (new-detection id id-n1 id-n2)
  (define track (new-track id-n1 id-n2))
  (lambda(msg)
    (cond((eq? msg 'get-id) id)
         ((eq? msg 'get-track) track)
         (else (error "unknown msg -- detection")))))