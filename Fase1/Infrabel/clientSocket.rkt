#lang racket
;(require racket/tcp)
(require "ServerSocket.rkt")
(provide connect-to-server)



(define VOID '())
(define server VOID)

(define input-buffer VOID)
(define output-buffer VOID)

(define (write data buffer)
  ((server 'receive-data) data))

(define (flush-output buffer)
  (set! output-buffer VOID))

(define (read input-buf)
  input-buffer)

(define (tcp-connect host port)
  (set! server host))

(define (read-data)
  (read input-buffer))

(define (send-data data)
  (write data output-buffer)
  (flush-output output-buffer))

(define (receive-data data)
  (set! input-buffer data))

(define (connect-to-server host port)
  (let ((in VOID) (out VOID))
    (tcp-connect host port)
    (set! output-buffer in)
    (set! input-buffer out)))

;;; nmbs -> infrabel-communication:
;;;   1.stuur data naar infrabel
;;;   2.geeft data dat infrabel gestuurd heeft
;;; infrabel-communication -> nmbs-communication
;;;   1.nmbs wilt data sturen naar jou ontvangt die

;;; nmbs-communication ->infrabel-communication
;;;   1. infrabel wilt-data sturen ontvang die
;;; infrabel -> nmbs-communication
;;;   1. stuur data naar nmbs
;;;   2. geef data dat nmbs gestuurd heeft


(define (infrabel-communication host port)
  (connect-to-server host port)
  (define (dispatch cmd)
    (cond((eq? cmd 'send-data) send-data)
         ((eq? cmd 'read-data) (read-data))
         ((eq? cmd 'receive-data) receive-data) 
         (else (error "uknown cmd- Infrabel communication"))))
  ((host 'new-client) dispatch)
  dispatch)



(define nmbs-c (nmbs-communication 800))
(define infrabel-c (infrabel-communication nmbs-c 800))


((infrabel-c 'send-data) 'hey-Whats-up!)
(nmbs-c 'read-data)

((nmbs-c 'send-data) 'good!)
(infrabel-c 'read-data)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;(let-values ([(x y) (tcp-connect "127.0.0.1" 8001)])
 ; (set! input-buffer x)
  ;(set! output-buffer y))

;(display "writing")
;(write "hey" output-buffer )
;(flush-output out)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;