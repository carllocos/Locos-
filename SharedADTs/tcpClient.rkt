#lang racket
(require (prefix-in tcp: racket/tcp))
(require (prefix-in ser: Project/SharedADTs/tcpServer))

(provide newTCPConnection)

(define (newTCPConnection)
  
  (define listener '())
  (define in '())
  (define out '())

  (define (init-connection)
    (define-values (intp outp) (tcp:tcp-connect "localhost" ser:port-nr))
    (set! in intp)
    (set! out outp)
    )

  (define (receive-package)

    (define packet (read in))
    packet
    )
    
  (define (send-package package)
    (write package out) 
    (flush-output out))
    

  (define (close)
  
    (close-input-port in)
    (close-output-port out))
  
  (define (dispatch cmd)
    (cond((eq? cmd 'send-package) send-package)
         ((eq? cmd 'init-connection) (init-connection))
         ((eq? cmd 'close) (close))
         ((eq? cmd 'receive-package) (receive-package))
         (else (error "unknown cmd- server" cmd))))
  dispatch)
