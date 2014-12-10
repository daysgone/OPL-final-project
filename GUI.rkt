#lang racket
(require racket/gui/base)
(include "objects.rkt")
(open-firmata)

;;-------------------------------------------
;;
;; Top Level GUI section
;;
;;-------------------------------------------

(define date (seconds->date (current-seconds)))
(define hour (number->string (date-hour date)))
(define minute (number->string (date-minute date)))
(define month (number->string (date-month date)))
(define day (number->string (date-day date)))
(define year (number->string (date-year date)))
(define time-string (string-append hour ":" minute))
(define date-string (string-append month "/" day "/" year))


(define num-horiz-panels 2)
(define num-vert-panels 4)
(define main-frame-height 300)
(define main-frame-width 600)
(define button-width (/ main-frame-width 3))
(define vert-panel-width (/ main-frame-width 2))
(define horiz-panel-height (/ main-frame-height 2))
(define vert-panel-border (/ main-frame-width 100))
(define horiz-panel-border (/ main-frame-height 100))

(define main-frame (new frame% [label "Automation"]
                               [width main-frame-width]
                               [height main-frame-height]
                               [stretchable-height #f]
                               [stretchable-width #f]
                               ))
;;Creates a horizontal panel
(define (make-horiz-panel child-of height align)
        (new horizontal-panel%
                          [parent child-of]
                          [min-height height]
                          [alignment align]))

;;Creates a vertical panel
(define (make-vert-panel child-of width align)
        (new vertical-panel%
                          [parent child-of]
                          [min-width width]
                          [alignment align]))

;;Creates a vertical panel with a border
(define (make-vert-border-panel child-of width align)
        (new vertical-panel% [parent child-of]
                                 [enabled #t]
                                 [style '(border)]
                                 [border vert-panel-border]
                                 [min-width width]
                                 [alignment align]))

;;Creates a horizontal panel with a border
(define (make-horiz-border-panel child-of height align)
        (new horizontal-panel%
                          [parent scenes-panel]
                          [style '(border)]
                          [border horiz-panel-border]
                          [min-height height]
                          [alignment align]))

;;Creates a light button       
(define (make-button obj child-of)
  (new button% [parent child-of]
             [label (ask obj 'name)]
             [min-width button-width]
             [callback (lambda (button event)
                         (ask obj 'switch-state))]))

;;Switch device on/off (the opposite of its current state)
(define (switch obj)
        (ask obj 'switch-state))

;;Creates a button that handles multiple devices
(define (make-multi-button name child-of on-obj-lst off-obj-lst)
  (new button% [parent child-of]
               [label name]
               [min-width button-width]
               [callback (lambda (button event)
                         (for-each (lambda (obj) (ask obj 'set-state! #t)) on-obj-lst)
                         (for-each (lambda (obj) (ask obj 'set-state! #f)) off-obj-lst))]))

;;Creates a message
(define (make-msg child-of says)
        (new message%
             [parent child-of]
             [label says]))
                     
(define (set-time sec)
        (set! date (seconds->date sec #t))
        (set! hour (number->string (if (<= (date-hour date) 12) (date-hour date)
                                       (- (date-hour date) 12))))
        (set! minute (if (> (date-minute date) 10) (number->string (date-minute date))
                        (string-append "0" (number->string (date-minute date)))))
        (set! month (number->string (date-month date)))
        (set! day (number->string (date-day date)))
        (set! year (number->string (date-year date)))
        (set! time-string (string-append hour ":" minute))
        (set! date-string (string-append month "/" day "/" year)))



;;Call this in the REPL to show the GUI window
(define (start-gui) (send main-frame show #t)  
                    (send timer start 10))  

;;Call this in teh REPL to close the window
(define (close-gui) (send main-frame show #f)) 
  

;;--------------------------------------------
;;
;; Lights section
;;
;;--------------------------------------------

(define hallway (make-led-obj "Hallway" 7))
(define bedroom (make-led-obj "Bedroom" 8 ))
(define kitchen (make-led-obj "Kitchen" 9 ))
(define living  (make-led-obj "Living Room" 10))
(define all "All")
(define Off 0)
(define On 1)
(define all-lights (list kitchen bedroom hallway living))

;;Defines the top panel with all of the lighting panels
(define lights-panel (make-horiz-panel main-frame horiz-panel-height '(left top)))

(define (timer-off obj hour min)
        (let ((time (ask obj 'time? 'off)))
        (cond [(and (eqv? hour (car time)) (eqv? (min (cdr time)))) (ask obj 'set-off)])))

(define (timer-on obj hour min)
        (let ((time (ask obj 'time? 'off)))
        (cond [(and (eqv? hour (car time)) (eqv? (min (cdr time)))) (ask obj 'set-on)])))

(define (turn-on-timed lst)
        (for-each (timer-on hour min) lst))
(define (turn-off-timed lst)
        (for-each (timer-off hour min) lst))

(define button-panel (make-vert-border-panel lights-panel vert-panel-width '(left top)))

;;-------------------------------------------------------------
;; Sets up status log for the lights (the center panel on top)
;;-------------------------------------------------------------
(define msg-panel (make-vert-border-panel lights-panel vert-panel-width '(center top)))
  
(define top-msg-panel (make-horiz-panel msg-panel 30 '(left center)))
(define second-msg-panel (make-horiz-panel msg-panel 30 '(left center)))
(define third-msg-panel (make-horiz-panel msg-panel 30 '(left center)))
(define bottom-msg-panel (make-horiz-panel msg-panel 30 '(left center)))

(define top-msg (make-msg top-msg-panel "Kitchen: On"))
(define sec-msg (make-msg second-msg-panel "Bedroom: On"))
(define third-msg (make-msg third-msg-panel "Hallway: On"))
(define bottom-msg (make-msg bottom-msg-panel "Living Room: On"))

(define (update-status-display)
        (send top-msg set-label (string-append "Kitchen: " (if (ask kitchen 'state?) "On" 
                                                               "Off")))
        (send sec-msg set-label (string-append "Bedroom: " (if (ask bedroom 'state?) "On" 
                                                               "Off")))
        (send third-msg set-label (string-append "Hallway: " (if (ask hallway 'state?) "On" 
                                                               "Off")))
        (send bottom-msg set-label (string-append "Living Room: " (if (ask living 'state?) "On" 
                                                               "Off"))))


;;--------------------------------------------------------
;; Sets up the panel with the buttons for individual lights
;;--------------------------------------------------------
 
(define kitchen-light (make-button kitchen button-panel))
(define bedroom-light (make-button bedroom button-panel))
(define hallway-light (make-button hallway button-panel))
(define living-light  (make-button living button-panel))


;;------------------------------------------------------------
;; Sets up scene selection panel (the top right panel)
;;------------------------------------------------------------

(define scenes-panel (make-vert-border-panel lights-panel vert-panel-width '(center top)))

(define label-panel (make-horiz-border-panel scenes-panel 20 '(center top)))  
(define scene-label (make-msg label-panel "Select Scene: "))

(define movie-time (make-multi-button "Movie Time" scenes-panel (list hallway kitchen) (list bedroom living)))
(define night-light (make-multi-button "Night Light" scenes-panel (list hallway bedroom living) (list kitchen)))
(define all-lights-button (make-multi-button "All Lights" scenes-panel all-lights '()))



;;---------------------------------------------
;;
;; Temp section
;;
;;---------------------------------------------
(define temp-panel (make-horiz-panel main-frame horiz-panel-height '(left top)))
(define reading-panel (make-vert-border-panel temp-panel vert-panel-width '(center top)))
(define temp (make-msg reading-panel "0"))
(define furnace-panel (make-vert-border-panel temp-panel vert-panel-width '(center top)))


(define furnace-setting (make-msg furnace-panel "Furnace is off"))

(define ac-setting (make-msg furnace-panel "A/C is on"))

;;----------------------------------------------------
;;
;; Time Panel
;;
;;----------------------------------------------------

(define hvac (make-HVAC-obj 'heater))
(define temp-time-panel (make-vert-border-panel temp-panel vert-panel-width '(center top)))

(define time-label (make-msg temp-time-panel "Time: "))


;;Display Time
(define actual-time (make-msg temp-time-panel time-string))
  
;;Display Date
(define actual-date (make-msg temp-time-panel date-string))
  
;;Timer to update the clock, date, and send commands at certain times. 
(define timer (new timer%
                   [notify-callback (lambda () 
                                      (set-time (current-seconds))
                                      (send temp set-label (number->string (ask hvac 'cur-temp)))
                                      (send actual-date set-label date-string)  
                                      (send actual-time set-label time-string)
                                      (update-status-display)
                                      (ask hvac 'run)
                                      (send furnace-setting set-label (string-append "Furnace is " (cond [(equal? 2 (ask hvac 'state?)) "On"]
                                                                                                     [else "Off"])))
                                      (send ac-setting set-label (string-append "A/C is " (if (equal? 4 (ask hvac 'state?)) "On"
                                                                                                     "Off"))))]
                   [interval #f]))

 