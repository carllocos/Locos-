#lang racket

(provide new-traject)

;Trajectis een opeenvolging van haltes tracks detections dat de trein moet passeren om tot bestemming te bekomen
;Men bewaart enkel de id's van elk object in de traject op!
;;bv '(Gent track7 Brussel Track13 Bestemming)
(define (new-traject l-stops)
  (define traject (list->vector l-stops))
  (define next-stop-index 1) ;;Index 0 is de halte vanwaar de loco start
  (define (traject-done?)
    (= next-stop-index (vector-length traject)))
  (define (next-stop)
    (if (traject-done?) #f (vector-ref traject next-stop-index)))
 
  (define (advance!)
    (set! next-stop-index (+ 1 next-stop-index)))
  
  (lambda(msg)
    (cond((eq? msg 'next-stop) (next-stop))
         ((eq? msg 'traject-done?) (traject-done?))
         ((eq? msg 'advance!) (advance!))
         (else (error "unknow msg-- traject")))))