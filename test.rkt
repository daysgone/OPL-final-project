#lang racket

(require "firmata.rkt")
;(require (planet xtofs/firmata:1:0/firmata))
(define port-name "/dev/tty.usbmodem1a21"); need to adjust depending on what usb port plugged into
(open-firmata);; open serial port conection using USB to arduino

(define led0 13) 
(define led1 10) 
(define lights (list led0 led1))
(define temp-sens 0 )
(define analog-sensor (list temp-sens))
(define clock-speed .2) ;; 0.005 is the tested max propogation speed.

(define (my-set-pin-mode! pin mode)
  (set-pin-mode! pin mode)
  (sleep clock-speed))

(define (clear-pins)
  (set-low! led0 0)
  (set-low! led1 0)
  )
(define (set-high! pin)
  (set-arduino-pin! pin)
)
(define (set-low! pin t)
  (clear-arduino-pin! pin)
  ;(sleep (* clock-speed t))
)

(define (setup)
  (map (λ (i) (clear-arduino-pin! i)) lights)
  (sleep clock-speed)
  (map (λ (i) (my-set-pin-mode! i OUTPUT_MODE)) lights)
  (report-digital-port! 0 1)
  (map (λ (i) (set-pin-mode! i INPUT_MODE)) analog-sensor)
  (report-analog-pin! 0 1)
  )

(define (get-volt-temp-sens pin)
  (* (read-analog-pin pin) 0.00488758553));converts 0-1023 that read-analog gets to values in range 0-5.0

(define (degreesC)
  (* (+ (get-volt-temp-sens 0) .5) 10)) ;.5 is voltage scaling from sensor datasheet 10mv per 1degC
(define (degreesF)
  (+ (* (degreesC) (/ 9.0 5.0)) 32))

(define (keep-led-on pin-list t) ;turn all led on for t amount of time
  (map (λ (x) (set-high! x)) lights)
         
  (sleep t)   
  (clear-pins)
  (sleep clock-speed)
  ;(keep-led-on pin-list t) ; turn on to loop
)

(define (led-on-temp pin-list t)
  (let ([temp t]
        [temp-on (lambda (x) (- x 1))]
        [temp-off (lambda (x) (+ x 1))])
    
    (printf "~a ~a ~n" "analog value" (read-analog-pin 0))
    ;(printf "~a ~a ~n" "current voltage" (get-volt-temp-sens 0))
    (printf "~a ~a ~a ~a ~a ~n"  "current temp:" (degreesC) 'C (degreesF) 'F )
    ;need to figure out how to not trigger light on and off when it gets to the correct temp
    (cond [(<= (degreesC) (- t 0.1));turn on furnace when x degrees below temp set
           (map (λ (x) (set-high! x)) pin-list)
          ]
          [(>= (degreesC) (+ t 0.2))
           (clear-pins) ; after it shuts off it cant turn on untill temp drops 2 degrees below set temp
           ;(set! temp (- t 2))
          ]
          [else (display "running")
                (map (λ (x) (set-high! x)) pin-list)]);shoudl be in range to keep furnace on
  (sleep 10); need to figure out way to only poll sensor every x seconds to help it not turn on and off 
  (led-on-temp pin-list t)
        
  
  ))

(define (start)
  (setup)
  ;(keep-led-on lights 5)
  ;(led-on-temp lights 18.6)
  )
(start)


   
;(close-firmata)