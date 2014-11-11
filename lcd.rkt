#lang racket
(require "firmata.rkt")
;(require (planet xtofs/firmata:1:0/firmata)) ;; includes the firmata library from racket (includes arduino interface fcns)
(open-firmata) ;; open a serial port connection to the USB connected arduino

(set-pin-mode! 13 OUTPUT_MODE) ;; set a digital output pin to be 13
;; in this case pin 13 happens to be connected directly to my board so no extra stuff needed

;; create a quick infinite loop to check that this code works
(define (loop)
  (set-arduino-pin! 13) ;; this will set pin 13 to HIGH status (LED = on)
  (sleep 1) ;; pause for a moment
  ;(clear-arduino-pin! 13) ;; this will set pin 13 to LOW status (LED = off)
  ;(sleep 1)
  (loop))

(loop)