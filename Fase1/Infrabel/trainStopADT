#lang racket


(provide new-node)


(define (new-node id-name x y)
  (define (set-x! new) (set! x new))
  (define (set-y! new) (set! y new))
  (lambda(msg)
    (cond((eq? msg 'get-x-location) x)
         ((eq? msg 'get-y-location) y)
         ((eq? msg 'get-id) id-name)
         (else (error "unknown msg-- train-stop")))))