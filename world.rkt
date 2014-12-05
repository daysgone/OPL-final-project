#lang racket
(include "objects.rkt")
(open-firmata);; open serial port conection using USB to arduino

;;create objects
(define LED1 (make-led-obj 'led1 10))
(define LED2 (make-led-obj 'led2 13))

(define furnace-led   (make-led-obj 'furnace 0))
(define AC-led   (make-led-obj 'ac 11  ))

;;define "rooms" with lists
(define HVAC (list furnace-led AC-led))
(define lights(list LED1 LED2))



(define (setup)
  ;;need to setup pins for analong/digital before it will actually work
  (map (λ (i) (set-pin-mode! (ask i 'pin) OUTPUT_MODE)) lights)
  (report-digital-port! 0 1) ;allows u to as current state of pin
  )


;;;; testing only below-------------------------------------------------
(define (testing)
  ;;testing toggling of all lights in group
  (printf "default values :\n")
  (display-light-state)
  (sleep 2)
  
  (toggle-state)
  (display-light-state)
  (sleep 2)
  
  (change-light-state #f)
  (display-light-state)
  (sleep 2)
  
  ;;set a light on manually
  (printf "setting LED1 on :\n")
  (ask LED1 'set-high )
  (display-light-state)
  (sleep 2)
  
  (printf "setting LED1 off :\n")
  (ask LED1 'set-low )
  (display-light-state)
  (sleep 2)
  )

;;do something with objects
;(ask AC-led 'name)
;(ask AC-led 'set-timer! 10) ;set timer for 10 sec need to use actual time
;(ask AC-led 'time-left); returns time left on timer

(define (display-light-state)
  (for-each (λ (x);if u use map it returns values which are not needed for this
            (printf "\t~a : ~a\n" 
                    (ask x 'name)
                    (ask x 'state?))) lights)
)

(define (toggle-state)
  (printf "Flipping each value :\n")
  (for-each (λ (x) (ask x 'switch-state)) lights))

(define (change-light-state value)
  (printf "setting each value to ~a :\n" value)
  (for-each (λ (x);if u use map it returns values which are not needed for this
              (ask x 'set-state! value)
              ) lights)
)

(define (keep-led-on group t) ;turn all led on for t amount of time
  (map (λ (x) (ask x 'set-high)) group)
         
  (sleep t)   
  (clear-pins group)
  (sleep clock-speed)
)



