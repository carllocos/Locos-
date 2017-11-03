#lang racket

(require (prefix-in cc: Project/Infrabel/commandControl))
(require (prefix-in d: Project/Infrabel/TodoADT))
(require (prefix-in tj: Project/Infrabel/trajectADT))
(require (prefix-in rs: Project/SharedADTs/railsADT))

(provide start-loco
         stop-loco
         displace-loco
         use-simulation!
         
         
         stop-loco-logic
         stop-loco-hardware
         )

(define simulation? #t)

(define Max-speed-simulation 1)
(define start-speed-simulation 0.90)
(define turbo 7)

(define Max-speed-hardware 125)
(define start-speed-hardware 70)

(define stop-speed 0)

(define (use-simulation! val)
  (set! simulation? val))

;;called when we reach a new-track/new-detection/destination
(define (derive-msg traject current-track next-track)
  (if (rs:detection-type? next-track)
      (if (tj:traject-almost-done? traject)
          d:destination-msg
          d:detection-msg)
      d:track-msg))

(define (next-node n1 n2 next-track)
  (if (eq? n2 (next-track 'get-id-n1))
      (next-track 'get-id-n2)
      (next-track 'get-id-n1)))


;same code as simulator but slightly addapted
(define (displace-loco loco traject)
  (let*([loop-virtual-time 1.0]
         [forward? (>= (loco 'get-speed) 0)]
         [dx (* (abs (loco 'get-speed)) loop-virtual-time)]
         [next-track? #f])

    ; displacing to next track
    (let while ()

      (define tlen ((tj:current-track traject) 'get-length))
      (unless (< dx (if forward?
                        (- tlen (loco 'get-distance))
                        (loco 'get-distance)))
        (set! next-track? #t)
   
        (define n1 (loco 'get-id-n1))
        (define n2 (loco 'get-id-n2))
        (define n3 (if forward?
                       (next-node n1 n2 (tj:next-track traject))
                       (next-node n2 n1 (tj:next-track traject))))
     
        (set! dx (- dx (if forward?
                           (- tlen (loco 'get-distance))
                           (loco 'get-distance))))
        
        ((loco 'set-id-n1!) (if forward? n2 n3))
        ((loco 'set-id-n2!) (if forward? n3 n1))
        ((loco 'set-distance!)(if forward? 0 ((tj:next-track traject) 'get-length)))
        (while)))
    
    ; displacement within track
    (define d1 (loco 'get-distance))
    (define d2 (if forward? (+ d1 dx) (- d1 dx)))
    (define n-t (tj:next-track traject))
    (define current (tj:current-track traject))

    ((loco 'set-distance!)d2)
    (if next-track?
        (derive-msg traject current (tj:next-track traject))
        d:rolling-msg)))

;returns correct speed based on orientation of the loco
(define (derive-speed loco traject)
  (let*((current-track (tj:current-track traject))
        (next-track (tj:next-track traject))
        (n-id (rs:node-in-common current-track next-track))
        (loco-n1 (eq? n-id (loco 'get-id-n1)))
        (speed (if simulation? start-speed-simulation start-speed-hardware)))
    (if loco-n1
        (* -1 speed)
        speed)))


;stops logic and hardware/simulation loco
(define (stop-loco loco) 
  (unless (= stop-speed (loco 'get-speed))
    (cc:stop-loco (loco 'get-id))
    ((loco 'set-speed!) stop-speed)))

;stops logic loco
(define (stop-loco-logic loco)
  ((loco 'set-speed!) stop-speed))

;stops hardware/simulation loco
(define (stop-loco-hardware loco)
  (cc:stop-loco (loco 'get-id)))
    
;starts logic and hardware/simulation loco
(define (start-loco loco traject)
    (let ((speed (derive-speed loco traject))
          (current-speed (loco 'get-speed)))
      ((loco 'set-speed!) (if simulation?
                              (* turbo speed)
                              speed))
      (cc:start-loco (loco 'get-id) speed)))