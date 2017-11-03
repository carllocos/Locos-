#lang racket


(require (prefix-in tcp: racket/tcp))

(provide newTCPListener
         port-nr)

(define port-nr 300)

;client represents infrabel. Whenever the server gets a message he will inform infrabel
(define (newTCPListener client)
  
  (define listener '())
  (define in '())
  (define out '())
  (define closed? #f)
  
  (define (init-listining)
    (displayln "server: server is listening...")
    (set! listener (tcp:tcp-listen port-nr 4 #t))
    (define-values (input output) (tcp:tcp-accept listener))
    (set! in input)
    (set! out output)
    (displayln "server: received one connection request.")
    )

  (define (send-package package)
    (unless closed?
      (write package out)
      (flush-output out)
      ))
    
  (define (close)
    (close-input-port in)
    (close-output-port out)
    (tcp-close listener)
    (set! closed? #t))

  (define (listen-loop)
    (unless closed?
      (define t (read in))
      (if (eof-object? t)
          (begin (close)
                 (client 'close))
          (begin ((client 'handle-received-package) t)
                 (listen-loop)))
      )
    )
  
  (define (dispatch cmd)
    (cond((eq? cmd 'send-package) send-package)
         ((eq? cmd 'init-listining) (init-listining))
         ((eq? cmd 'listen-loop) listen-loop)
         ((eq? cmd 'close) (close))
         (else (error "unknown cmd- server" cmd))))
  dispatch)
