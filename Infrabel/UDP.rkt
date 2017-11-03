#lang racket
(require (prefix-in sock: Project/Z21/z21-scheme/FullAPI/Z21Socket))
(require (prefix-in md: Project/Z21/z21-scheme/FullAPI/Z21MessageDriving))
(require (prefix-in ms: Project/Z21/z21-scheme/FullAPI/Z21MessageSwitches))
(require (prefix-in ml: Project/Z21/z21-scheme/FullAPI/Z21MessageLocation))
(require "../Z21/z21-scheme/FullAPI/Z21MessageSwitches.rkt")

(provide connect-with-Z21
         stop-connection
         
         change-loco-speed
         change-switch-status
         
         detection-block-used
         )

(define z '())

;this variable is where the messages handler will put the data received from Z21 
(define synergy '())

(define (reset-syn)
  (set! synergy '()))

(define (connect-with-Z21)
  (set! z (sock:setup))
  (sock:listen z handle-msg))

(define (stop-connection)
  (set! z #f))


(define (change-loco-speed id forward? speed)
  (let ((set-loco-speed-msg (md:make-set-loco-drive-msg "03" "00" (derive-speed-range speed) forward? speed)))
    (sock:send z set-loco-speed-msg)
    (sleep 1)))

(define (change-switch-status id pos)
  (let ((msg (ms:make-set-switch-msg id "00" #t pos)))
    (sock:send z msg)
    (sleep 1)
    (if (null? synergy)
        (change-switch-status id pos)
        (if (= pos (cdr synergy))
            (begin #t (reset-syn))
            (begin #f (reset-syn))))))

;returns a pair of (module, address)
(define (detection-block-used loco-id)
  (let ((msg (ml:make-rmbus-get-data-msg "00")))
        
    (sock:send z msg)
    (sleep 1)
    (if (null? synergy)
        (detection-block-used loco-id)
        (and synergy
             (cons (ml:get-module synergy) (car (ml:get-occupancies synergy)))))))


(define zero-speed 'zero)
(define low-speed-range 'low)
(define medium-speed-range 'med)
(define high-speed-range 'high)

(define (zero-speed? s)
  (= s 0))

(define (low-speed? sp)
  (and (>  sp 0)
       (<= sp md:low-speed-range)))

(define (medium-speed? sp)
  (<  sp md:med-speed-range))

(define (high-speed? sp)
  (>  sp md:med-speed-range))


(define (derive-speed-range speed)
  (let ((sp (abs speed)))
    (cond ((zero-speed? sp) md:low-speed-range)
          ((low-speed? sp) md:low-speed-range)
          ((high-speed? sp) md:high-speed-range)
          ((medium-speed? sp) md:med-speed-range))))


(define (handle-msg msg)
  (cond
    ((is-switch-info-msg? msg) (handle-switch-info msg))
    ((ml:is-rmbus-datachanged-msg? msg) (handle-location-changed-msg msg))))



(define (handle-switch-info msg)
  (set! synergy (cons (get-switch-info-address msg) (get-switch-info-position msg))))


(define (handle-location-changed-msg msg)
  (let ((data (ml:get-rmbus-data msg)))
    (if (empty? data)
        (set! synergy '())
        (let((m (loop-modulo data)))
          (set! synergy m)))))


(define (loop-modulo data)
  (if (empty? data)
      #f
      (if (not (null? (ml:get-occupancies (car data))))
          (car data)
          (loop-modulo (cdr data)))))
