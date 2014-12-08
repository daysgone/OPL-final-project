(require "firmata.rkt")
(include "utils.rkt")



;;; Simple object system with inheritance
(define (ask object message . args)  
  (let ((method (get-method object message)))
    (if (method? method)
        (apply method (cons object args))
        (error "No method" message (cadr method)))))

(define (get-method object message)
  (object message))

(define (no-method name)
  (list 'no-method name))

(define (method? x)
  (not (no-method? x)))

(define (no-method? x)
  (if (pair? x)
      (eq? (car x) 'no-method)
      #f))

;;base object
(define (make-obj name)
  (lambda (message) 
    (cond [(eq? message 'name) (lambda (self) name)]
          [else (no-method name)])))

#|-----------------------------------------
              SIMPLE DEVICE 
     -used for furnace , AC , fan
 !!! mode must be INPUT_MODE OUTPUT_MODE ANALOG_MODE PWM_MODE
-------------------------------------------|#
(define (make-simple-device-obj name pin-def mode)
  (let* ([pin pin-def]
         [mode (set-pin-mode! pin mode)];sets mode on creation
         [obj (make-obj name)])
    
    (lambda (message) 
      (cond [(eq? message 'pin);returns int 
             (lambda (self) pin)];physical pin location on ardino
            [(eq? message 'mode);returns defined ints (INPUT_MODE/OUTPUT_MODE/ANALOG_MODE/PWM_MODE etc... from firmata.rkt)
             (lambda (self) mode)]
            [(eq? message 'set-mode!) 
             (lambda (self new-mode) (set-pin-mode! pin new-mode))]
            [(eq? message 'state?);returns bool, #t is on
             (lambda (self) (is-arduino-pin-set? pin))]
            [(eq? message 'set-on) ;turn light on
             (lambda (self) (set-arduino-pin! pin))]
            [(eq? message 'set-off) ;turn light off
             (lambda (self)(clear-arduino-pin! pin))]
            [(eq? message 'set-state!) ;;change state of led to what is passed in as arg
             (lambda (self state) (if (boolean? state)
                                      (begin (if (equal? state #t)
                                                 (set-on! pin);this will set internal state and update arduino pin state
                                                 (set-off! pin)
                                                 )
                                             (unless (not debug) 
                                               (printf "\t~a : \t ~a \n" (ask self 'name) (ask self 'state?))
                                               )
                                             )
                                      (error "please use a boolean value")
                                      ))] ; something other then a bolean used
            
            ;;toggle led state
            [(eq? message 'switch-state);toggles current state
             (lambda (self) (ask self 'set-state! (not (ask self 'state?))))]
            
            [else (get-method obj message)])))
)

#|-----------------------------------------
                     LED 
     !-> defaults to manual temp control unless at least 1 on/off time is used
  TODO - to use intensity led must be connected to a PWD pin
-------------------------------------------|#
(define (make-led-obj name pin)
  (let* ([schedual #f]
        [mode OUTPUT_MODE]
        [time-on (cons 0 0)]
        [time-off (cons 0 0)]
        [intensity 0] ;range 0-255
        [device-obj (make-simple-device-obj name pin mode)])
   
    (lambda (message) 
      (cond [(eq? message 'time) (lambda (self state) ;returns cons cell
                                        (cond [(eq? state 'on) time-on]
                                              [(eq? state 'off) time-off]
                                              [else (error "please use 'on or 'off for state")]
                                        ))]
            ;usage (ask self 'set-time! (cons 12 00) 'on)
            [(eq? message 'set-time!) (lambda (self time state) ;time passed as a cons cell???
                                        (cond [(eq? state 'on) (set! time-on time)]
                                              [(eq? state 'off)(set! time-off time)]
                                              [else (error "please use 'on or 'off for state")]
                                        ))]
            
            ;;NOT YET IMPLEMENTED!!!!
            [(eq? message 'intensity);returns int
             (lambda (self) intensity)];need to figure of pwm pins!!!!!
            [(eq? message 'set-intensity!) (lambda (self level) (set! intensity level))]
            
            [else (get-method device-obj message)])
      )))


#|-----------------------------------------
     HVAC - includes "furnace" and "ac" objects????
     !-> defaults to manual temp control unless at least 1 on/off time is used
-------------------------------------------|#
(define (make-HVAC-obj name) 
  (let* ([schedual #f];manual temp change only(overides current schedual until next schedualed time triggers)
         
         [time-morn (cons (cons 0 0) -1)]; time/temp ((12.00). 65) temp in F
         [time-afternoon (cons (cons 0 0) -1)]
         [time-evening (cons (cons 0 0) -1)]
         [time-night (cons (cons 0 0) -1)]
         [time-schedual (list time-morn time-afternoon time-evening time-night)]
         
         ;sets high/low for HVAC to kick on defaults to 2 degree dif
         [temp-range 2]
         [temp-on (lambda (x) (- x temp-range))]; prob dont need lambda???
         [temp-off (lambda (x) (+ x temp-range))]
         
         [furnace (make-simple-device-obj 'furnace 11 OUTPUT_MODE)];shouldnt be hardcoded pin
         [fan (make-simple-device-obj 'fan 12 OUTPUT_MODE)];shouldnt be hardcoded pin
         [ac (make-simple-device-obj 'ac 13 OUTPUT_MODE)];shouldnt be hardcoded pin
         
         ;[thermostat (make-analog-obj 'therm);
         
         [slaves (list furnace fan ac)] ;complex device can have simple devices that are controled by it
         [inputs '()] ; can allow
         
         [device-obj (make-simple-device-obj name 0 0)]);not actually connected to pins 
    
    (lambda (message) 
      (cond [(eq? message 'get-slave) (λ (self sub-obj) sub-obj)] 
            [(eq? message 'set-slave!) (λ (self slave state)
            ;(filter-map (λ (i) (if (eq? slave (ask i 'name))#t #f))slaves))];returns a list not what is wanted
                (map (λ (i) (if (eq? slave (ask i 'name));look for item in list with correct name
                                (begin (ask i 'set-state! state)
                                       #t)
                                #f ))slaves))]
                         
            [(eq? message 'set-fan!) (λ (self state)
                                       (ask (ask self 'get-slave fan) 'set-state! state))]
            ;manual control only use 'set-temp-schedual! for automation
            [(eq? message 'set-temp!) (lambda (self) self)]
            [(eq? message 'set-temp-schedual!) (lambda (self ) self)]
            
            [(eq? message 'slaves) (lambda (self) slaves)]
            [else (get-method device-obj message)])
      )))
