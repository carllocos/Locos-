#lang racket


;(require scheme/mpair)
(provide new-signal)

(define (new-signal id)
  (define status 'green)
  (define (orange!) (set! status 'orange))
  (define (green!) (set! status 'green))
  (define (red!) (set! status 'red))
  (lambda(msg)
    (cond((eq? msg 'red?)(eq? 'red status))
         ((eq? msg 'green?)(eq? 'green status))
         ((eq? msg 'orange?)(eq? 'orange status))
         ((eq? msg 'orange!)(orange!))
         ((eq? msg 'green!)(green!))
         ((eq? msg 'red!)(red!))
         ((eq? msg 'get-id) id)
         (else (error "unknown msg-signal")))))