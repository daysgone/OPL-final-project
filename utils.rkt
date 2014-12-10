(require "firmata.rkt")
#|-----------------------------------------
            Global vars

-------------------------------------------|#
(define clock-speed 1) ;; 0.005 is the tested max propogation speed.
(define debug #f)
(define env '()) ; create empty env
(define valid-modes (list OUTPUT_MODE ANALOG_MODE PWM_MODE ))

#|-----------------------------------------
           utility functions

-------------------------------------------|#

;;what type of object is it????
(define (is-a object property)
  (let ((method (get-method object property)))
    (if (method? method)
        (ask object property)
        false)))


#|-----------------------------------------
          general pin based functions


(define (set-on! pin)
  (set-arduino-pin! pin)
)
(define (set-off! pin)
  (clear-arduino-pin! pin)  
)
-------------------------------------------|#
#|-----------------------------------------
          general pin based functions

-------------------------------------------|#
(define (clear-pins group)
  (for-each (λ (x);if u use map it returns values which are not needed for this
              (ask x 'set-off!)
              ) group)
  )

;environment is all the object groups created
(define (reset-system)
  (if (not(null? env));something in env
      (for-each (lambda (x) (clear-pins x)) env)
      (error "nothing in env")))


#|-----------------------------------------
             light group based functions

-------------------------------------------|#
(define (display-light-state group)
  (for-each (λ (x);if u use map it returns values which are not needed for this
            (printf "\t~a : ~a\n" 
                    (ask x 'name)
                    (ask x 'state?))) group)
)

;switch state of every member of group---------------
(define (toggle-state group)
  (unless (not debug) (printf "Flipping each value :\n"));debug
  (for-each (λ (x) (ask x 'switch-state)) group)
  (sleep clock-speed)
)

; change a whole group at once -------------------
(define (change-light-state value group)
  (unless (not debug) (printf "setting each value to ~a :\n" value));debug
  (for-each (λ (x) (ask x 'set-state! value)) group) ;mapping not used since it returns a value
  (sleep clock-speed)
)

;turn all members of group on for t amount of time----------------
(define (keep-on group t)
  (map (λ (x) (ask x 'set-on)) group)
         
  (sleep t)   
  (clear-pins group);shut off now
  (sleep clock-speed); brief pause before it can do something else
)


#|-----------------------------------------
          HVAC util functions

|#
;analog sensor has a range of inaccuracy of 2 degree C from data sheet on temp sensor used
(define (get-volt-temp-sens pin)
  (* (read-analog-pin pin) 0.00488758553));converts 0-1023 that read-analog gets to values in range 0-5.0

(define (degreesC)
  (* (+ (get-volt-temp-sens 0) .5) 10)) ;.5 is voltage scaling from sensor datasheet 10mv per 1degC

(define (degreesF)
  (+ (* (degreesC) (/ 9.0 5.0)) 32))

(define (F->C temp)
  (* (- temp 32) (/ 5.0 9.0))) 
 
(define (led-on-temp); pin-list t)
  #|(let ([temp t]
        [temp-on (lambda (x) (- x 1))]
        [temp-off (lambda (x) (+ x 1))])
  |#
    (printf "~a ~a ~n" "analog value" (read-analog-pin 0))
    ;(printf "~a ~a ~n" "current voltage" (get-volt-temp-sens 0))
    (printf "~a ~a ~a ~a ~a ~n"  "current temp:" (degreesC) 'C (degreesF) 'F )
  )
#|
    (cond [(<= (degreesC) (- t 0.1));turn on furnace when x degrees below temp set
           (map (λ (x) (set-on! x)) light)
          ]
          [(>= (degreesC) (+ t 0.2))
           (map (λ (x) (set-off! x)) light) ; after it shuts off it cant turn on untill temp drops 2 degrees below set temp
           ;(set! temp (- t 2))
          ]
          [else (display "running")
                (map (λ (x) (set-on! x))light)]);should be in range to keep furnace on
  )
  ;(led-on-temp pin-list t)
        
  


;;unused functions---------------------
(define (is-a object property)
  (let ((method (get-method object property)))
    (if (method? method)
        (ask object property)
        false)))

(define (fade-off pin)
    (for ([i (my-range 255 0 5)])
          ((analog-write! pin i) (sleep clock-speed))
    )
)  
|#
