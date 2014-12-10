#lang racket
(include "objects.rkt")
(open-firmata);; open serial port conection using USB to arduino


;;create objects
(define LED1 (make-led-obj 'led1 7))
(define LED2 (make-led-obj 'led2 8))
(define LED3 (make-led-obj 'led3 9 PWM_MODE))

(define LED4 (make-led-obj 'led4 10))
(define thermostat (make-simple-analog-sensor-obj 'therm 0 INPUT_MODE))
;HVAC device
;(define HVAC (make-HVAC-obj 'hvac)) ;need to set pins for sub objects!!!!

;;define groups of lights
(define lights (list LED1 LED2 LED3 LED4))

;should add HVAC to env but need to make new method inside to set children
(set! env (list lights )) ;;allows clearing of all pins on all groups

(define (setup)
  ;(reset-system) ;clear all pins
  (set! debug #f) ; use debug messages
  (report-digital-port! 0 1) ;allows u to as current state of pin
  (report-analog-pin! 0 1)
  )


;;;; testing only below-------------------------------------------------
(define (testing)
  (set! debug #t)
  ;;testing toggling of all lights in group
  (printf "default values :\n")
  (display-light-state lights)
  (sleep clock-speed)
  
  (toggle-state lights); with debug mode on will display change states
  (sleep (* clock-speed 2))
  
  ;;turn all lights off
  (change-light-state #f lights)
  (sleep (* clock-speed 2))
  
  ;;set a light on manually
  (printf "setting LED1 on :\n")
  (sleep clock-speed)
  (ask LED1 'set-on! )
  (sleep clock-speed)
  (display-light-state lights)
  
  ;;toggle all led
  (toggle-state lights); with debug mode on will display change states
  (sleep clock-speed)
  
 
  (change-light-state #f lights);;shut all lights off
  (sleep clock-speed)
  
  
  )
   

;;initialze

(setup)

;(testing)

