#lang racket
(require racket/gui/base)
(require firmata)
;;(open-firmata)

;;-------------------------------------------
;;
;; Top Level GUI section
;;
;;-------------------------------------------

(define main-frame-height 300)
(define main-frame-width 300)
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
(send main-frame show #t)
;;--------------------------------------------
;;
;; Lights section
;;
;;--------------------------------------------

(define hallway "Hallway")
(define bedroom "Bedroom")
(define kitchen "Kitchen")
(define all "All")
(define Off 0)
(define On 1)
(define all-lights '(kitchen bedroom hallway))
(define light-state (make-hash))

(define lights-panel (new horizontal-panel% 
                          [parent main-frame]
                          [min-height horiz-panel-height]
                          [alignment '(left top)]
                          [stretchable-height #f]
                          [stretchable-width #f]))
                          
(define button-panel (new vertical-panel% [parent lights-panel]
                                 [enabled #t]
                                 [style '(border)]
                                 [border vert-panel-border]
                                 [min-width vert-panel-width]
                                 [alignment '(left top)]))
(define lights-dividor-panel(new vertical-panel%
                          [parent lights-panel]
                          [alignment '(center top)]))

(define msg-panel (new vertical-panel% [parent lights-panel]
                              [enabled #t]
                              [style '(border)]
                              [border vert-panel-border]
                              [min-width vert-panel-width]
                              [alignment '(center top)]))

(define msg (new message% [parent msg-panel]
                          [label "No events thus far..."]))

(define (make-button light-name)
  (new button% [parent button-panel]
             [label light-name]
             [min-width button-width]
             [callback (lambda (button event)
                         (switch light-name msg))]))

(define kitchen-light (make-button kitchen))
(define bedroom-light (make-button bedroom))
(define hallway-light (make-button hallway))
(define all-lights-button (make-button all))

(define (turn-on light mesg)
               (lambda (light) (send mesg set-label "On")))

(define (turn-off light mesg)
               ( (hash-set! light-state light Off)
                 (send mesg set-label (list light "Off"))))
(define (switch light mesg)
               (if (hash-ref light-state light On) (turn-on light mesg)
                   (turn-off light mesg)))
                         

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







(define (GoTo)
  (set-pin-mode! 9 OUTPUT_MODE)
  (set-arduino-pin! 9)
  (sleep 1)
  (clear-arduino-pin! 9) 
  (sleep 1)
  (GoTo)
)

(GoTo)