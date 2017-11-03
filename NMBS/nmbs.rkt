#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NMBS
;;
;; Construction of NMBS requires a boolean that. Set on true means that we use the simulator, false a udp connection
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (prefix-in tcp: Project/SharedADTs/tcpClient))
(require (prefix-in package: Project/SharedADTs/packageADT))
(require (prefix-in as: Project/NMBS/assistant))
(require (prefix-in g: Project/GUI/gui))

(define rwm-file-hardware "../NMBS/hardware_model.txt")

(define rwm-file-simulation "../NMBS/hardware_model_simulation.txt")

(define (nmbs simulation?)
  (define file (if simulation?
                   rwm-file-simulation
                   rwm-file-hardware))
  
  (define clientSocket '())
  (define assistant (as:new-assistant file))
  (define gui '())
  (define update-thread '())
 
  (define (connect-to-server)
    (set! clientSocket (tcp:newTCPConnection))
    (clientSocket 'init-connection)
    ((clientSocket 'send-package) (package:new 'set-up simulation?))
    (start-update)
    )

  (define (start-gui)
    (set! gui (g:new-gui (assistant 'get-rwm) dispatch))
    (gui 'draw-all)
    (gui 'show-instructions)
    )

 

  (define (update-loop)
    (let ((package (package:new 'send-update-rwm package:EMPTY_CONTENT)))
      ((clientSocket 'send-package) package)
      (let*((response-package (clientSocket 'receive-package))
            (content (package:content response-package)))
        (unless (package:empty-content? content)
          ((assistant 'update-rwm) content)
          (gui 'draw-all))
        (sleep 3)))
    (update-loop))

  (define (start-update)
    (set! update-thread (thread update-loop)))
        

  (define (close)
    (kill-thread update-thread))
    
  
  (define (handle-received-package package)
    (let*((msg (package:message package))
          (content (package:content package)))
      (if (package:empty-content? content)
          (dispatch msg)
          ((dispatch msg) content))))

  (define (new-traject lst)
    (let* ((loco-id (car lst))
           (end (cadr lst))
           (tracks-package ((assistant 'new-traject) end loco-id))
           (package (package:new 'handle-traject (package:traject-package loco-id tracks-package))))
      (when tracks-package
        ((clientSocket 'send-package) package))))
    

  (define (handle-gui-command lst)
    (let*((command-lst (map (lambda( st)
                              (string->symbol st))
                            lst))
          (command (car command-lst))
          (args (cdr command-lst))
          (f (dispatch command)))
      (when f
          (f args))))
  
  (define (dispatch cmd)
    (cond((eq? cmd 'handle-gui-command) handle-gui-command)
         ((eq? cmd 'new-traject) new-traject)
         ((eq? cmd 'close) (close))
         (else #f)))
 (start-gui)
  ;comment the next line to cancel the connection with the server
 (connect-to-server)
  dispatch)


(define n (nmbs #t))
 