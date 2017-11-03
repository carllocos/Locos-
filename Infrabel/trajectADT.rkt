#lang racket

(require (prefix-in rs: Project/SharedADTs/railsADT))
(require (prefix-in rwm: Project/SharedADTs/railwaymodelADT))
(require (prefix-in p: Project/SharedADTs/packageADT))


(provide new-traject
         traject-loco
         
         current-track
         next-track
         previous-track         
         next-detection
         previous-detection
         prev-prev-detection

         advance!
         next-detection!
         previous-detection!
         
         member-current-pos        
         list-tracks-until-condition

         traject-done?
         traject-almost-done?
         traject-contains?
         change-direction-loco?

         )




(define head-size 5)

(define traject-label 'traject)

(define (traject? any)
  (and (vector? any)
       (eq? (vector-ref any 0) traject-label)))

(define (new-traject loco-id list-tracks rwm)
  (define list-tracks-objects (map (lambda (t)
                                     (rwm:get-track rwm  (p:track-n1 t) (p:track-n2 t)))
                                   list-tracks))
  (define tj (list->vector (append (list traject-label loco-id #f #f head-size) list-tracks-objects)))
  (next-detection! tj)
  tj)


(define (get-position traject)
  (vector-ref traject 4))

(define (advance! traject)
  (vector-set! traject 4 (incr-index (get-position traject))))


(define (traject-loco traject)
  (vector-ref traject 1))

(define (previous-detection traject)
  (vector-ref traject 2))

(define (previous-detection! traject detection)

  (define prev (previous-detection traject))
  (vector-set! traject 2 detection))


(define (prev-prev-detection traject)
  (let* ((ctr 0)
         (current (current-track traject))
         (current-id (if (rs:detection-type? current)
                         (current 'get-id)
                         #f)))
    (member-index traject
                  decr-index
                  (get-position traject)
                  (lambda(track)
                    (if (and (when (and (rs:detection-type? track)
                                        (not (eq? (track 'get-id) current-id)))
                               (set! ctr (+ ctr 1))
                               #t)
                             (= ctr 2))
                        #t
                        #f)))))

(define (next-detection traject)
  (vector-ref traject 3))

(define (next-detection! traject)
  (define detection (calculate-next-detection traject))
  (define next (next-detection traject))
  (if detection
      (vector-set! traject 3 detection)
      (vector-set! traject 3 next)))

(define (current-track traject)
  (define pos (get-position traject))
  (if (correct-index? pos traject)           
      (vector-ref traject pos)
      #f))

(define (next-track traject)
  (define pos (incr-index (get-position traject)))
  (if (correct-index? pos traject)
      (vector-ref traject pos)
      #f))

(define (previous-track traject)
  (define pos (decr-index (get-position traject)))
  (if (correct-index? pos traject)
      (vector-ref traject pos)
      #f))

(define (get-track traject pos)
  (if (correct-index? pos traject)
      (vector-ref traject pos)
      #f))

(define (traject-done? traject)
  (= (+ (get-position traject) 1) (vector-length traject)))

(define(traject-almost-done? traject)
  (= ( + (get-position traject) 2) (vector-length traject)))

(define (traject-contains? trajec-L2 detection-L1)
  (member-current-pos trajec-L2 (lambda(track)
                                       (and (rs:detection-type? track)
                                            (eq? (track 'get-id) (detection-L1 'get-id))))))

;checks if the loco has to change direction based on the current and next track
(define (change-direction-loco? loco traject)
  (let ((n-t (next-track traject))
        (p-t (previous-track traject)))
    (and p-t
         (rs:same-track? n-t p-t))))

;this returns a list of all tracks until the condition is reached
(define (list-tracks-until-condition tj cond)
  (define index (get-position tj))
  (let loop ((result '()) (i index))
    (let (( t (get-track tj i)))
      (if t
          (if (cond t)
              (reverse result)
              (loop (cons t result) (incr-index i)))
          (error "list-tracks-until-condition error the index is out out bound maybe wrong condition the condition ")))))
                    
  
;hulprprocedures


(define (calculate-next-detection traject)
  (next-detection-starting-from traject
                                (incr-index (get-position traject))))

;looks for track that satisfies condition
(define (member-current-pos traject condition)
  (member-index traject
                         (lambda(index)
                           (incr-index index))
                         (get-position traject)
                         condition))

;returns next detection starting from start-index
(define (next-detection-starting-from traject start-index)
  (member-index traject
                         (lambda(index)
                           (incr-index index))
                         start-index
                         rs:detection-type?))


;looks for track that satisfies condition
;incre/decr: procedure tha takes current index and transforms it into the next pos index
(define (member-index traject icre/decr start-index condition)
  (let loop ((index start-index))
    (if (correct-index? index traject)
        (if (condition (get-track traject index))
            (get-track traject index)
            (loop (icre/decr index)))
        #f)))



(define(incr-index index)
  (+ index 1))

(define(decr-index index)
  (- index 1))

(define (correct-index? index traject)
  (and (< index (vector-length traject))
       (>= index head-size)))