#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signal ADT
;;
;; This ADT represent the light signal next to a detection block.
;; It's red whenever the associated detection-block is been used by a train
;; It's yellow when the next detection block in the railwaymodel is been used
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide new-signal
         signal-type?
         )

(define signal-type 'signal)

(define (signal-type? obj)
  (and (procedure? obj)
       (eq? (obj 'get-type) signal-type)))

(define (new-signal id)
  (define status 'green)
  
  (define (orange!)
    (set! status 'orange))
  
  (define (green!)
    (set! status 'green))
  
  (define (red!)
    (set! status 'red))
  (define (set-status! s)
    (set! status s))
  (lambda(msg)
    (cond((eq? msg 'red?)(eq? 'red status))
         ((eq? msg 'green?)(eq? 'green status))
         ((eq? msg 'orange?)(eq? 'orange status))
         ((eq? msg 'orange!)(orange!))
         ((eq? msg 'green!)(green!))
         ((eq? msg 'red!)(red!))
         ((eq? msg 'get-id) id)
         ((eq? msg 'get-type) signal-type)
         ((eq? msg 'get-status) status)
         ((eq? msg 'set-status!) set-status!)
         (else (error "unknown msg-signal: " msg)))))



(define (display-signal s)
  (displayln (string-append
              "signal id:"
              (symbol->string (s 'get-id))
              "  status:"
              (symbol->string (s 'get-status)))))