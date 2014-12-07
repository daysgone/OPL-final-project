#lang racket
(require racket/gui/base)
(include "objects.rkt")
;;(open-firmata)

;;-------------------------------------------
;;
;; Top Level GUI section
;;
;;-------------------------------------------

(define date (seconds->date (current-seconds)))
(define hour (number->string (date-hour date)))
(define minute (number->string (date-minute date)))
(define time-string (string-append hour ":" minute))

(define run #f)

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

(define (start-gui) (send main-frame show #t)
                    (send timer start 10))  ;;Call this in the REPL to show the GUI window
(define (close-gui) (send main-frame show #f)
                    (set! run #f))  ;;Call this in teh REPL to close the window
;;--------------------------------------------
;;
;; Lights section
;;
;;--------------------------------------------

(define hallway "Hallway")
(define bedroom "Bedroom")
(define kitchen "Kitchen")
(define living "Living")
(define all "All")
(define Off 0)
(define On 1)
(define all-lights '(kitchen bedroom hallway))
(define light-state (make-hash))

;;Defines the top panel with all of the lighting panels
(define lights-panel (new horizontal-panel% 
                          [parent main-frame]
                          [min-height horiz-panel-height]
                          [alignment '(left top)]
                          [stretchable-height #f]
                          [stretchable-width #f]))
;;--------------------------------------------------------
;; Sets up the panel with the buttons for individual lights
;;--------------------------------------------------------

(define button-panel (new vertical-panel% [parent lights-panel]
                                 [enabled #t]
                                 [style '(border)]
                                 [border vert-panel-border]
                                 [min-width vert-panel-width]
                                 [alignment '(left top)]))

(define (make-button light-name)
  (new button% [parent button-panel]
             [label light-name]
             [min-width button-width]
             [callback (lambda (button event)
                         (switch light-name msg))]))

(define kitchen-light (make-button kitchen))
(define bedroom-light (make-button bedroom))
(define hallway-light (make-button hallway))
(define living-light  (make-button living))

(define (turn-on light mesg)
               (lambda (light) (send mesg set-label "On")))

(define (turn-off light mesg)
               ( (hash-set! light-state light Off)
                 (send mesg set-label (list light "Off"))))
(define (switch light mesg)
               (if (hash-ref light-state light On) (turn-on light mesg)
                   (turn-off light mesg)))
;;-------------------------------------------------------------
;; Sets up status log for the lights (the center panel on top)
;;-------------------------------------------------------------
(define msg-panel (new vertical-panel% [parent lights-panel]
                              [enabled #t]
                              [style '(border)]
                              [border vert-panel-border]
                              [min-width vert-panel-width]
                              [alignment '(center top)]))

(define msg (new message% [parent msg-panel]
                          [label "No events thus far..."]))
;;------------------------------------------------------------
;; Sets up scene selection panel (the top right panel)
;;------------------------------------------------------------

(define scenes-panel (new vertical-panel%
                          [parent lights-panel]
                          [style '(border)]
                          [border vert-panel-border]
                          [min-width vert-panel-width]
                          [alignment '(center top)]))

(define label-panel (new horizontal-panel%
                          [parent scenes-panel]
                          [style '(border)]
                          [border horiz-panel-border]
                          [min-height (/ horiz-panel-height 6)]
                          [alignment '(center top)]))

(define scenes-button-panel (new horizontal-panel%
                          [parent scenes-panel]
                          [min-height (/ horiz-panel-height 6)]
                          [alignment '(left top)]))

(define scene-label (new message%
                          [parent label-panel]
                          [label "Select Scene:"]))

(define (make-scene-button light-name)
  (new button% [parent scenes-panel]
             [label light-name]
             [min-width button-width]
             [callback (lambda (button event)
                         (switch light-name msg))]))

(define all-lights-button (make-scene-button all))


;;---------------------------------------------
;;
;; Temp section
;;
;;---------------------------------------------
(define temp-panel (new horizontal-panel% 
                        [parent main-frame]
                        [min-height horiz-panel-height]
                        [alignment '(left top)]
                        [stretchable-height #f]
                        [stretchable-width #f]
                        ))
(define reading-panel (new vertical-panel%
                           [parent temp-panel]
                           [style '(border)]
                           [border vert-panel-border]
                           [min-width vert-panel-width]
                           [alignment '(center top)]
                           ))


;;;(define temp-dividor-panel(new vertical-panel%
;;;                          [parent temp-panel]
;;;                          [alignment '(center top)]))

(define temp (new message% [parent reading-panel]
                           [label "no reading"]))

(define furnace-panel (new vertical-panel%
                           [parent temp-panel]
                           [style '(border)]
                           [border vert-panel-border]
                           [min-width vert-panel-width]
                           [alignment '(center top)]))

(define furnace-setting (new message% 
                             [parent furnace-panel]
                             [label "Furnace is off"]))
;;----------------------------------------------------
;;
;; Time Panel
;;
;;----------------------------------------------------


(define temp-time-panel (new vertical-panel%
                           [parent temp-panel]
                           [style '(border)]
                           [border vert-panel-border]
                           [min-width vert-panel-width]
                           [alignment '(center top)]
                           ))

(define time-label (new message%
                          [parent temp-time-panel]
                          [label "Time: "]))

;;Display Time
(define actual-time (new message%
                          [parent temp-time-panel]
                          [label time-string]))

;;Display Date
(define actual-date (new message%
                          [parent temp-time-panel]
                          [label (string-append (number->string (date-month date)) "/" (number->string (date-day date)) "/" (number->string (date-year date)))]))

(define timer (new timer%
                   [notify-callback (lambda () 
                                    (set! date (seconds->date (current-seconds)))
                                    (set! hour (number->string (date-hour date)))
                                    (set! minute (number->string (date-minute date)))
                                    (set! time-string (string-append hour ":" minute))
                                    (send actual-date set-label (string-append (number->string (date-month date)) "/" (number->string (date-day date)) "/" (number->string (date-year date))))  
                                    (send actual-time set-label time-string))]
                   [interval #f]))




;;(define (GoTo)
;;  (set-pin-mode! 9 OUTPUT_MODE)
;;  (set-arduino-pin! 9)
;;  (sleep 1)
;;  (clear-arduino-pin! 9) 
;;  (sleep 1)
;;  (GoTo)
;;)

;;(GoTo)