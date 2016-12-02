#lang racket
;(require racket/tcp)
(provide nmbs-communication)

(define VOID '())

(define client VOID)

(define input-buffer VOID)
(define output-buffer VOID)

(define (write data buffer)
  ((client 'receive-data) data))

(define (flush-output buffer)
  (set! output-buffer VOID))

(define (read input-buf)
  input-buffer)

(define (tcp-listen port max-listeners reuse? address)
  VOID)

(define (read-data)
  (read input-buffer))

(define (send-data data)
  (write data output-buffer)
  (flush-output output-buffer))

(define (receive-data data)
  (set! input-buffer data))

(define (new-client c)
  (set! client c))
(define (make-listener host port)
  (let ((in VOID) (out VOID))
    (tcp-listen port 4 #f VOID)
    (set! output-buffer in)
    (set! input-buffer out)))

(define (nmbs-communication port)
  (make-listener VOID port)
  (define (dispatch cmd)
    (cond((eq? cmd 'send-data) send-data)
         ((eq? cmd 'read-data) (read-data))
         ((eq? cmd 'receive-data) receive-data)
         ((eq? cmd 'new-client) new-client)
         (else (error "uknown cmd- nmbs communication"))))
  dispatch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define lstr (tcp-listen 8001 4 #f "127.0.0.1"))
;(define IN '())
;(define OUT '())
;(let-values ([(x y) (tcp-accept lstr)])
 ; (set! IN x)
  ;(set! OUT y))
;(define ctr 0)
;(define (loop)
;  (when (< ctr 50000)
    
 ;   (display (read IN))
  ;  (set! ctr (+ ctr))
   ; (loop)))

;(loop)
;(when (tcp-accept-ready? lstr)
 ; (display "ready"))

;(tcp-close lstr)
;(tcp-addresses lstr)