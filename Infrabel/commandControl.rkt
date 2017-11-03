#lang racket
(require (prefix-in udp: Project/Infrabel/UDP))
(require (prefix-in sim: Project/Simulator/interface))
(require (prefix-in rwm: Project/SharedADTs/railwaymodelADT))


(provide start-loco
         stop-loco
         
         start-connection
         stop-connection
         
         change-switch!
         get-detection-loco
         
         )

(define simulator? #t)

(define (start-connection Simulator?)
  (set! simulator? Simulator?)
  (if simulator?
      (sim:start-simulator)
      (udp:connect-with-Z21)))

(define (stop-connection)
  (if simulator?
      (sim:stop-simulator)
      (udp:stop-connection)))

(define (start-loco loco-id speed)
  (if simulator?
      (sim:set-loco-speed! loco-id speed)
      (udp:change-loco-speed loco-id (> speed 0) speed)))  ;the boolean on true corresponds with driving forward

(define (stop-loco loco-id)
  (if simulator?
      (sim:set-loco-speed! loco-id 0)
      (udp:change-loco-speed loco-id #t 0)))

(define (get-detection-loco loco-id)
  (if simulator?
      (sim:get-loco-detection-block loco-id)
      (let ((p (udp:detection-block-used loco-id)))
        (rwm:detection-hardware-id->logic-id (car p) (cdr p)))))
        

(define (change-switch! switch-id pos)
  (if simulator?
      (sim:set-switch-position! switch-id pos)
      (udp:change-switch-status switch-id pos)))
