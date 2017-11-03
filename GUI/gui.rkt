#lang racket
(require racket/gui/base)
(require (prefix-in rwm: Project/SharedADTs/railwaymodelADT))
(require (prefix-in rs: Project/SharedADTs/railsADT))

(provide new-gui)

(define WIDTH 1500)
(define HEIGHT 700)


(define myfont (make-font #:size 15
                          #:family 'decorative
                          #:weight 'bold))

  
(define pen-width 2)
(define style-transparent 'transparent)
(define style-solid  'solid)
(define style-dot 'dot)

(define no-pen (make-object pen% "BLACK" pen-width style-transparent))
(define green-pen (make-object pen% "GREEN" pen-width style-solid))
(define orange-pen (make-object pen% "ORANGE" pen-width style-solid))
(define red-pen (make-object pen% "RED" pen-width style-solid))
(define red-pen-dot (make-object pen% "RED" (* pen-width 3) style-solid))
(define black-pen (make-object pen% "BLACK" pen-width style-solid))




(define (repaint canvas)
  (send canvas on-paint))

(define dy 1.8)
(define y-min 100)
(define dx 1.8)
(define dx-min 50)

(define scale-loco-y 20)

(define (scale-x x)
  (let ((val (/ x dx)))
    (cond ((> val WIDTH) WIDTH)
          ((< val 0) dx-min)
          (else val))))

(define (scale-y y)
  (let ((val (/ y dy)))
    (cond ((> val HEIGHT) HEIGHT)
          ((< val 0) 0)
          (else val))))



(define (draw-node dc n)
  
  (send dc set-text-foreground "blue")
  (send dc draw-text (symbol->string (n 'get-id)) (scale-x (n 'get-x-location)) (scale-y (n 'get-y-location))))


(define (draw-track dc rwm track)
  (let ((n1 (rwm:get-node rwm (track 'get-id-n1)))
        (n2 (rwm:get-node rwm (track 'get-id-n2)))
        (pen #f))
    (cond ((rs:detection-type? track) (let ((sig (rwm:get-signal rwm (track 'get-id))))
                                        (cond ((sig 'green?) (set! pen green-pen))
                                              ((sig 'orange?) (set! pen orange-pen))
                                              (else (set! pen red-pen)))))
          (else (set! pen black-pen)))

    (send dc set-pen pen)
    (send dc set-smoothing 'smoothed)
    (send dc
          draw-line
          (scale-x (n1 'get-x-location))
          (scale-y (n1 'get-y-location))
          (scale-x (n2 'get-x-location))
          (scale-y (n2 'get-y-location)))
    
    (draw-node dc n1)
    (draw-node dc n2)
    
    )
 )


(define arrow-dx 20)
(define arrow-dy 20)

(define (points-head-arrow xf<xt? yf<yt? yf=ft? Xm Ym)
  (if xf<xt?
      (if  yf<yt?
          (values (- Xm arrow-dx) Ym Xm (- Ym arrow-dy))
          (if yf=ft?
              (values (- Xm arrow-dx) (- Ym arrow-dy) (- Xm arrow-dx) (+ Ym arrow-dy))
              (values (- Xm arrow-dx) Ym Xm (+ Ym arrow-dy))))
      (if  yf<yt?
           (values Xm (- Ym arrow-dy) (+ Xm arrow-dx) Ym)
           (if yf=ft?
               (values (+ Xm arrow-dx) (- Ym arrow-dy) (+ Xm arrow-dx) (+ Ym arrow-dy))
               (values Xm (- Ym arrow-dy) (- Xm arrow-dx) Ym)))))

;this procedure just draws an arrow to indicate the position of a switch
(define (draw-switch dc rwm switch)
  (define t (if (= (switch 'get-position) 1)
                (switch 'get-track-1)
                (switch 'get-track-2)))
  (define n-from (rwm:get-node rwm (if (eq? (switch 'get-id-mn) (t 'get-id-n1))
                                       (t 'get-id-n1)
                                       (t 'get-id-n2))))
  (define n-to (rwm:get-node rwm (if (eq? (switch 'get-id-mn) (t 'get-id-n1))
                                     (t 'get-id-n2)
                                     (t 'get-id-n1))))
  (define X-from (n-from 'get-x-location))
  (define Y-from (n-from 'get-y-location))
  (define X-to (n-to 'get-x-location))
  (define Y-to (n-to 'get-y-location))
  (define Xm (if (> X-to X-from)
                 (+ X-from (/ (abs ( - X-from X-to)) 2))
                 (+ X-to (/ (abs ( - X-from X-to)) 2))))
  (define Ym (if (> Y-to Y-from)
                 (+ Y-from (/ (abs ( - Y-from Y-to)) 2))
                 (+ Y-to (/ (abs ( - Y-from Y-to)) 2))))
  
  (define-values (x1 y1 x2 y2) (points-head-arrow (< X-from X-to) (< Y-from Y-to) (= Y-from Y-to) Xm Ym))

  
  (send dc set-smoothing 'smoothed)
  (send dc set-pen red-pen)
    (send dc
          draw-line
          (scale-x Xm)
          (scale-y Ym)
          (scale-x x1)
          (scale-y y1))
    
    (send dc
          draw-line
          (scale-x Xm)
          (scale-y Ym)
          (scale-x x2)
          (scale-y y2))

  (send dc set-pen red-pen-dot)
  (send dc draw-point (scale-x X-from) (scale-y Y-from)))


(define (draw-switchs dc rwm ls)
  (for-each (lambda(s)
              (draw-switch dc rwm s))
            ls))


(define (draw-tracks dc rwm tracks)
  (for-each (lambda (t)
                (draw-track dc rwm t))
              tracks))

(define (draw-loco dc rwm loco)
  (let ((n1 (rwm:get-node rwm (loco 'get-id-n1)))
        (n2 (rwm:get-node rwm (loco 'get-id-n2))))
    
    (send dc set-pen red-pen)
    (send dc set-smoothing 'smoothed)

    (send dc set-font (make-font #:size 14 #:family 'roman
                                 #:weight 'bold))
    
    (send dc set-text-foreground "red")
    (send dc draw-text (symbol->string (loco 'get-id)) (scale-x (n1 'get-x-location)) (+ scale-loco-y (scale-y (n1 'get-y-location))))
    (send dc draw-text (symbol->string (loco 'get-id)) (scale-x (n2 'get-x-location)) (+ scale-loco-y  (scale-y (n2 'get-y-location)))))
  )

(define (draw-locos dc rwm ls)
  (for-each (lambda (l)
              (draw-loco dc rwm l))
            ls))



(define (new-gui rwm nmbs)

  (define frame (new frame%
                     [label "GUI NMBS"]
                     [width WIDTH]
                     [height HEIGHT]
                     [stretchable-width #t]
                     [stretchable-height #t]
                     [style (list 'no-resize-border)]))


  ;canvas will react on mouse events. This will trigger the dialog-box
  (define mycanvas%
    (class canvas%
      (super-new)
      (define/override (on-event mouse-e)
        (when (send mouse-e button-down?)
          (dispatch 'show-command-box)))))

  (define canvas (new mycanvas%
                      [parent frame]))

  (define dc (send canvas get-dc))

  
  (define (close-dialog-box button control-event)
    (send dialog show #f)
    (draw-all))


  (define (process-request button control-event)
    (let ((command (string-split (send command-field get-value))))
      (send dialog show #f)
      ((nmbs 'handle-gui-command) command)))

  ;components to make a dialog box
  (define dialog (instantiate dialog% ("Enter command")))
  (define command-field (new text-field% [parent dialog] [label "Command"]))
  (define panel (new horizontal-panel% [parent dialog]
                     [alignment '(center center)]
                     [min-width 400]	 
                     [min-height 50]
                     [stretchable-width #f]	 
                     [stretchable-height #f]))
  (define cancel-button (new button% [parent panel] [label "Cancel"] [callback close-dialog-box]))
  (define ok-button (new button% [parent panel] [label "Ok"] [callback process-request]))

 ;components to make the instructions in the dialog box
  (define inst-txt (string-append "This is an automatic dialoge box that appears every time you run nmbs\n\n"
                                  "The graph is the railway-model.\n\n"
                                  "The detections blocks are the colored tracks.\n\n"
                                  "A signal associated to a detection block is the color of the detection:\n"
                                  " green - not being used\n"
                                  " orange - next detection is being used\n"
                                  " red - being used\n\n"
                                  "Remaining tracks are black\n\n"
                                  "The middle of a switch is indicated with a red dot. And the direction of the switch\n\n"
                                  "with the red arrow.\n\n"
                                  "A loco is between the track with the red numbers (loco id).\n\n"
                                  "Right- or left click on canvas opens command box\n"
                                  "commands:\n"
                                  " new-traject loco-id destination\n"                                  
                             ))

   (define (show-instructions-box b e)
    (send dialog-inst show #t))

  (define (close-inst-box b e)
    (send dialog-inst show #f))

  (define dialog-inst (instantiate dialog% ("Instructions")))
  (define inst-field (new text-field%
                          [parent dialog-inst]
                          [label ""]
                          [min-width 500] 
                          [min-height 500]))
  (send inst-field set-value inst-txt)
 
  (when (system-position-ok-before-cancel?)
      (send panel change-children reverse))

  (define (show-command-box)
    (send command-field set-value "new-traject 3 Da")
    (send dialog show #t))

  (define (start)
    (send frame show #t)
    (send dc set-background "white")
    (send canvas on-paint)
    (send canvas on-paint)    
    (sleep/yield 1))

  (define (draw-all)
    (let ((ls (rwm:get-list-locos rwm))
          (ts (rwm:get-list-tracks rwm))
          (ss (rwm:get-list-switchs rwm)))
      (send dc erase)
      (draw-tracks dc rwm ts)
      (draw-switchs dc rwm ss)
      (draw-locos dc rwm ls)))
  
  (define (dispatch msg)
    (cond ((eq? msg 'show-command-box) (show-command-box))
          ((eq? msg 'show-instructions) (show-instructions-box #f #f))
          ((eq? msg 'draw-all) (draw-all))
          (else (error "unknown command for GUI msg:" msg))))
  (start)
  dispatch)
