#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Infrabel ADT
;;
;; Represents Infrabel, represents the interface for NMBS. Every communication between NMBS and Infra happens through
;; a TCP connection. It can send/receive data in form op Package ADT.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (prefix-in con: Project/Infrabel/Controller))
(require (prefix-in tcp: Project/SharedADTs/tcpServer))
(require (prefix-in rwm: Project/SharedADTs/railwaymodelADT))
(require (prefix-in p: Project/SharedADTs/packageADT))

(define rwm-file-simulation "../NMBS/hardware_model_simulation.txt")
(define rwm-file-hardware "../NMBS/hardware_model.txt")

(define (new-INFRABEL)
  (define clientSocket '())
  (define controller '())
  (define rwm '())

  (define listen-thread '())

 
  
  (define (handle-received-package package)
    (let*((msg (p:message package))
          (content (p:content package)))
      (if (p:empty-content? content)
          (dispatch msg)
          ((dispatch msg) content))))


  ;produces a list of packets the Railwaymodel ADT
  (define (prepare-rwm-content rwm)
    (let ((ls (rwm:get-list-locos rwm))
          (ss (rwm:get-list-switchs rwm))
          (sigs (rwm:get-list-signals rwm)))
      (append (map (lambda(l)
                     (p:loco-package (l 'get-id) (l 'get-id-n1) (l 'get-id-n2) (l'get-distance)))
                   ls)
              (map (lambda(s)
                     (p:switch-package (s 'get-id-mn) ( s 'get-position)))
                   ss)
              
              (map (lambda(s)
                     (p:signal-package (s 'get-id) ( s 'get-status)))
                   sigs))))
    
  
  (define (send-update-rwm)
    (let((package (p:new 'update-rwm (prepare-rwm-content rwm))))
          (( clientSocket 'send-package) package)))

  ;handles a new request for a traject
  (define (handle-traject traject-package)
    (let((loco-id (p:traject-loco traject-package))
          (tracks (p:traject-tracks traject-package)))
    ((controller 'new-traject) loco-id tracks)))

  ;initiates the rwm based on simulation? boolean.
  ;The boolean will determine if we have to use a udp or a connection to the simulator
  (define (set-up-rwm simulation?)
    (define file '())
    (if simulation?
        (begin (set! file rwm-file-simulation)
               (set! rwm (rwm:new-rwm rwm-file-simulation)))
          (begin (set! file rwm-file-hardware)
                 (set! rwm (rwm:new-rwm rwm-file-hardware))))    
   
    (set! controller (con:new-controller rwm simulation?))
   
    )
  
  (define (start-server)
    (set! clientSocket (tcp:newTCPListener dispatch))
    (clientSocket 'init-listining)
    (set! listen-thread (thread (clientSocket 'listen-loop))))

  (define (close-infrabel)
    (kill-thread listen-thread)
    (controller 'close)
    )

    
  (define (dispatch msg)
    (cond((eq? msg 'handle-received-package) handle-received-package)
         ((eq? msg 'handle-traject) handle-traject)
         ((eq? msg 'set-up) set-up-rwm)
         ((eq? msg 'close) (close-infrabel))
         ((eq? msg 'send-update-rwm) (send-update-rwm))
         (else (error "unknown msg--infrabel: " msg))))
  
  (start-server)

  dispatch)

(define infra (new-INFRABEL))
