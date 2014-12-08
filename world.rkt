#lang racket
(include "objects.rkt")
(open-firmata);; open serial port conection using USB to arduino


;;create objects
(define LED1 (make-led-obj 'led1 7))
(define LED2 (make-led-obj 'led2 8))
(define LED3 (make-led-obj 'led3 9))
(define LED4 (make-led-obj 'led4 10))

;HVAC device
(define HVAC (make-HVAC-obj 'hvac)) ;need to set pins for sub objects!!!!
  
;(define Furnace-LED   (make-led-obj 'furnace 11));red on rgb led
;(define Fan-LED   (make-led-obj 'fan 12  )); green on rgb
;(define AC-LED  (make-led-obj 'ac 13  )); blue on rgb
;(define thermostat (make-analog-obj 'therm ))

;(define HVAC (list Furnace-LED AC-LED Fan-LED)); device

;;define groups of lights
(define lights (list LED1 LED2 LED3 LED4))

(set! env (list lights)) ;;allows clearing of all pins on all groups

(define (setup)
  (reset-system) ;clear all pins
  (set! debug #f) ; use debug messages
  
  ;;need to setup pin mode before it will actually work
  ;;leds automatically set to OUTPUT_MODE
  ;(map (λ (i) (set-pin-mode! (ask (ask i 'slaves?) 'pin) OUTPUT_MODE)) HVAC)
  
  (report-digital-port! 0 1) ;allows u to as current state of pin
  )


;;;; testing only below-------------------------------------------------
(define (testing)
  ;;testing toggling of all lights in group
  (printf "default values :\n")
  (display-light-state lights)
  (sleep 1)
  
  (toggle-state lights); with debug mode on will display change states
  (sleep 1)
  
  ;;turn all lights off
  (change-light-state #f lights)
  (display-light-state lights)
  (sleep clock-speed)
  
  ;;set a light on manually
  (printf "setting LED1 on :\n")
  (sleep clock-speed)
  (ask LED1 'set-on )
  (sleep clock-speed)
  (display-light-state lights)
  
  
  (printf "setting LED1 off :\n")
  (sleep clock-speed)
  (ask LED1 'set-off )
  (sleep clock-speed)
  (display-light-state lights)
  
  )

;(ask AC-led 'name)
;(ask AC-led 'set-timer! 10) ;set timer for 10 sec need to use actual time
;(ask AC-led 'time-left); returns time left on timer
;;functions to work with groups of lights------------------------



             

;;initialze

(setup)

;(testing)

