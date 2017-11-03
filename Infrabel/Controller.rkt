#lang racket

(require (prefix-in tc: Project/Infrabel/Traject-Controller))
(require (prefix-in lc: Project/Infrabel/Loco-Controller))
(require (prefix-in sc: Project/Infrabel/Security-Controller))
(require (prefix-in rwm: Project/SharedADTs/railwaymodelADT))
(require (prefix-in tb: Project/SharedADTs/tableADT))
(require (prefix-in d: Project/Infrabel/TodoADT))

(require (prefix-in q: Project/ExternLib/queue-linked))
(require (prefix-in tj: Project/Infrabel/trajectADT))
(require (prefix-in cc: Project/Infrabel/commandControl))

(provide new-controller)
       
;;queue of locos that are involved in a traject
(define locos-active-queue (q:new))

;this table has an entry for each loco. Each entry holds a todo for a loco
(define todo-table (tb:make-table))
(define (get-todo loco-id)
  (tb:table-get-element todo-table loco-id))

(define (set-up-todos locos-lst)
  (for-each (lambda(l)
              (update-todo (l 'get-id) #f))
            locos-lst))
  
(define (update-todo loco-id new-todo)
  (tb:table-add-element todo-table loco-id new-todo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;We use a different thread for loop-update
(define LOOP_WAIT_TIME 0.1)
(define LOOP_THREAD '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define (new-controller rwm simulation?)

  
  (define (set-up)
    (cc:start-connection simulation?)
    (lc:use-simulation! simulation?)
    (tc:use-simulation! simulation?)
    (sc:initiate-signals rwm)
    (set-up-todos (rwm:get-list-locos rwm))
    
    (set! LOOP_THREAD (thread update-loop)))

  (define (new-traject loco-id list-tracks)
    (let*((traject (tj:new-traject loco-id list-tracks rwm))
          (todo (d:new-todo d:check-safety-msg (d:make-bag traject))))
      
      (unless (get-todo loco-id)      
        (update-todo loco-id todo)
        (q:enqueue! locos-active-queue (rwm:get-loco rwm loco-id)))))  

  (define (update-loop)
    (unless (q:empty? locos-active-queue)
      (let* ((loco (q:serve! locos-active-queue))
             (loco-id (loco 'get-id))
             (todo (get-todo loco-id))
             (msg  (d:todo-msg todo))
             (bag (d:todo-bag todo))
             (traject (d:bag-traject bag)))

        ;case when safety has to be checked. Result is that loco starts rolling or has to wait
        (cond ((d:check-safety-msg? msg)(let((response (handle-safety (sc:check-safety dispatch rwm traject) rwm traject loco bag)))
                                          (set! msg response)))

              ;case when a loco is moving. Result is that loco reaches a new track, new detection, destination or is still in the same track
              ((d:loco-rolling-msg? msg) (let ((response (handle-moving-loco rwm loco traject bag)))
                                           (set! msg response)))
              
              ;case when a loco enters a regular track or switch track
              ((d:track-reached-msg? msg)(let ((response (handle-track-reached rwm traject loco bag)))
                                           (set! msg  response)))

              ;case when a loco enters a detection track
              ((d:detection-reached-msg? msg)(let ((response (handle-detection-reached rwm traject loco bag)))
                                               (set! msg response)))
              
              ;case when a loco has to wait for safety reasons
              ((d:wait-msg? msg) (let ((response (handle-waiting-loco msg bag)))
                                   (set! msg response)))
              
              ;case is when a loco finished his traject
              ((d:destination-reached-msg? msg) (let ((response (handle-destination-reached msg rwm traject loco bag)))
                                                  (set! msg response))))
        (if (d:traject-done-msg? msg)
            (update-todo loco-id #f)
          (begin (update-todo loco-id (d:new-todo msg bag))
                 (q:enqueue! locos-active-queue loco)))))
    
    (sleep LOOP_WAIT_TIME)
    (update-loop))
        
  
  (define (stop-connection)
    (kill-thread LOOP_THREAD)
    (cc:stop-connection))
  
  (define (dispatch cmd)
    (cond ((eq? cmd 'new-traject) new-traject)
          ((eq? cmd 'close) (stop-connection)) 
          (else (error "unknown cmd-- Controller: " cmd))))
  (set-up)
  dispatch)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HULP PROCEDURES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The procedures below will handle every type of todo message ex.: moving a loco, checking and update safety
;; Every procedure will also produce a new todo msg.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (handle-moving-loco rwm loco traject bag)
  (let(( response (lc:displace-loco loco traject)))
    (if (d:time-to-check-delay? bag)
        (let ((det-id (sc:loco-logic-delay? traject)))
          (if det-id
            (begin (lc:stop-loco-hardware loco)
                   (tc:correct-delay! rwm loco traject det-id sc:update-signals)
                   (if (tj:traject-done? traject)
                       d:destination-msg
                       d:check-safety-msg)
                   )
            response))
        response)))

(define (handle-delay-hardware msg rwm traject loco bag)
  (if (sc:loco-hardware-delay? traject)
      msg
      (begin (sc:update-signals rwm traject)
             d:check-safety-msg)))

(define (handle-waiting-loco msg bag)
  (if (d:loco-done-waiting? bag)
      (begin (d:reset-wait-time! bag)
             d:check-safety-msg)
      (begin (d:increase-wait-time! bag)
             msg)))

(define (handle-track-reached rwm traject loco bag)
  (tc:reached-new-track rwm traject) 
  (sc:update-signals rwm traject)
  d:rolling-msg)

(define (handle-detection-reached rwm traject loco bag)
  (tc:reached-new-detection rwm traject)
  d:check-safety-msg)

(define (handle-destination-reached msg rwm traject loco bag)
            (if (sc:loco-hardware-delay? traject)
                msg
                (begin (lc:stop-loco loco)
                       (unless (tj:traject-done? traject)
                         (tc:reached-new-track rwm traject))
                       (sc:update-signals rwm traject)
                       d:traject-done-msg)))

(define (handle-safety msg rwm traject loco bag)
  (cond ((d:safe-msg? msg) (sc:update-signals rwm traject)
                           (tc:reserve-until-detection! rwm traject)
                           (when (tj:change-direction-loco? loco traject)
                             (lc:stop-loco loco)
                             (sleep 3))
                           (lc:start-loco loco traject)
                           d:rolling-msg)
        
        ((d:wait-msg? msg) (lc:stop-loco loco)
                           (sc:update-signals rwm traject)
                           msg)
        
        ((d:delay-hardware-msg? msg) (lc:stop-loco-logic loco)          
                                     msg)))