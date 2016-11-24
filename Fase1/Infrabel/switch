#lang racket
(provide new-switch)

(define (new-switch id-mn id-n0 id-n1 id-n2)
  (define position-switch 1)

  (define (increase)
    (when (= position-switch 1)
      (set! position-switch 2)))

  (define (decrease)
    (when (= position-switch 2)
      (set! position-switch 1)))

  (lambda(msg)
    (cond((eq? msg 'get-id-mn) id-mn)
         ((eq? msg 'get-id-n0) id-n0)
         ((eq? msg 'get-id-n1) id-n1)
         ((eq? msg 'get-id-n2) id-n2)
         ((eq? msg 'get-position) position-switch)
         ((eq? msg 'increase-position) (increase))
         ((eq? msg 'decrease-position) (decrease))
         (else (error "message-switch-unknown")))))