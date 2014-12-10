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
  (λ (message) 
    (cond [(eq? message 'name) (λ (self) name)]
          [else (no-method name)])))

#|-----------------------------------------
              SIMPLE DEVICE 
     -used for furnace , AC , fan
 !!! mode must be INPUT_MODE OUTPUT_MODE ANALOG_MODE PWM_MODE
     -could make pin and mode optional???
-------------------------------------------|#
(define (make-simple-device-obj name pin-def mode)
  (let* ([pin pin-def]
         ;[mode (set-pin-mode! pin mode)];sets mode on creation
         [obj (make-obj name)]
         )
    
    ;;set things on creation
    (set-pin-mode! pin mode);sets mode 
    
    (λ (message) 
      (cond [(eq? message 'pin);returns int 
             (λ (self) pin)];physical pin location on ardino
            ;[(eq? message 'mode);returns defined ints (INPUT_MODE/OUTPUT_MODE/ANALOG_MODE/PWM_MODE etc... from firmata.rkt)
            ; (λ (self) mode)]
            [(eq? message 'set-mode!) (λ (self new-mode) (set-pin-mode! pin new-mode))];;prob shouldnt be used
            
            [(eq? message 'state?);returns bool, #t is on
             (λ (self) (is-arduino-pin-set? pin))]
            
            [(eq? message 'set-on!) ;turn light on
             (λ (self . value)  (if(equal? mode PWM_MODE)
                  (analog-write! pin (car value))
                  (set-arduino-pin! pin)))]
            
            [(eq? message 'set-off!) ;turn light off
             (λ (self)
               (if(equal? mode PWM_MODE)
                  (analog-write! pin 0)
                  (clear-arduino-pin! pin)))]
            
            [(eq? message 'set-state!) ;;change state of led to what is passed in as arg
             (λ (self state . value) (cond [(and (boolean? state) (null? value) (equal? state #t) )
                                            (ask self 'set-on!)]
                                           [(and (boolean? state) (equal? state #t) )
                                            (ask self 'set-on! (car value))]
                                           [(boolean? state)
                                            (ask self 'set-off!)]
                                           [else (error "please use a boolean value")])
               )]
            
            ;;toggle led state
            [(eq? message 'switch-state);toggles current state
             (λ (self) (ask self 'set-state! (not (ask self 'state?))))]
            
            [else (get-method obj message)])))
)

#|-----------------------------------------
                     LED 
     !-> defaults to manual temp control unless at least 1 on/off time is used
  TODO - to use intensity led must be connected to a PWD pin
-------------------------------------------|#
(define (make-led-obj name pin . mode-def)
  (let* ([schedual #f]
         [mode (if (null? mode-def);did we pass optional param
                   OUTPUT_MODE ;default for led
                   (let ([new-mode (findf (λ (i) (equal? (car mode-def) i)) (list OUTPUT_MODE ANALOG_MODE PWM_MODE ))])
                     (unless (not debug)
                       (display new-mode))
                     (if new-mode
                         (begin (set-pin-mode! pin new-mode)
                                new-mode)
                         OUTPUT_MODE)
                         
                   ))]
         [time-on (cons 0 0)]
         [time-off (cons 0 0)]
         [intensity 0] ;range 0-255 only can be used if mode PWM
         [device-obj (make-simple-device-obj name pin mode )]
         )
    
    (λ (message) 
      (cond [(eq? message 'schedual?) (λ (self) schedual)]
            ;if schedual is #f then time methods will not function
            [(eq? message 'set-schedual!) (λ (self state) 
                                            (if (boolean? state)
                                                (begin (if state
                                                           (set! schedual #t)
                                                           (set! schedual #f) )
                                                       (unless (not debug) 
                                                         (printf "\t~a schedual: \t ~a \n" (ask self 'name) (ask self 'schedual?))))
                                             (error "please use a boolean value")
                                            ))]
            [(eq? message 'time) (λ (self state) ;returns cons cell
                                   (if schedual
                                       (cond [(eq? state 'on) time-on]
                                             [(eq? state 'off) time-off]
                                             [else (error "please use 'on or 'off for state")]
                                       )
                                       (error "please set-schedual #t before using")))]
            
            ;usage (ask self 'set-time! (cons 12 00) 'on)
            [(eq? message 'set-time!) (λ (self time state) ;time passed as a cons cell???
                                        (if schedual
                                            (begin(cond [(eq? state 'on) (set! time-on time)]
                                                        [(eq? state 'off)(set! time-off time)]
                                                        [else (error "please use 'on or 'off for state")])
                                                  (unless (not debug) 
                                                    (printf "\t~a ~a @ ~a \n" (ask self 'name) state (ask self 'time state))))
                                            (error "please set-schedual #t before using")))]
            
            ;;NOT YET IMPLEMENTED!!!!
            [(eq? message 'intensity);returns int
             (λ (self) intensity)];need to figure of pwm pins!!!!!
            [(eq? message 'set-intensity!) (λ (self level) (set! intensity level))]
            
            [else (get-method device-obj message)])
      )))
#|-----------------------------------------
               analog sensor 
    -input only
-------------------------------------------|#
(define (make-simple-analog-sensor-obj name pin-def mode)
  (let* ([pin pin-def]
         [mode (set-pin-mode! pin mode)];sets mode on creation
         [obj (make-obj name)])
    
    (λ (message) 
      (cond [(eq? message 'pin);returns int 
             (λ (self) pin)];physical pin location on ardino
            [(eq? message 'mode);returns defined ints (INPUT_MODE/OUTPUT_MODE/ANALOG_MODE/PWM_MODE etc... from firmata.rkt)
             (λ (self) mode)]
            ;;[(eq? message 'set-mode!) (λ (self new-mode) (set-pin-mode! pin new-mode))]
            [(eq? message 'value?);returns 0-1023
             (λ (self) (read-analog-pin pin))]
            
            
            [else (get-method obj message)])))
)
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
         [temp-on (λ (x) (- x temp-range))]; prob dont need λ???
         [temp-off (λ (x) (+ x temp-range))]
         
         #|----------
                           sub objects of device 
                                                 ---------------|#
         [furnace (make-simple-device-obj 'furnace 11 OUTPUT_MODE)];hardcoded pin
         [fan (make-simple-device-obj 'fan 12 OUTPUT_MODE)];hardcoded pin
         [ac (make-simple-device-obj 'ac 13 OUTPUT_MODE)];hardcoded pin
         
         [thermostat (make-simple-analog-sensor-obj 'therm 0 INPUT_MODE)];
         
         [children (list furnace fan ac)] ;complex device can have simple devices that are controled by it
         [inputs '()] ; can allow
         
         
         [obj (make-obj name)])
    
    (λ (message) 
      (cond ;[(eq? message 'set-off!) ((for-each (λ () (ask  'set-off) children)))]
            [(eq? message 'get-child) (λ (self sub-obj) sub-obj)] 
            [(eq? message 'set-child-state!) (λ (self child state)
                (for-each (λ (i) (if (eq? child (ask i 'name));look for item in list with correct name
                                (begin (ask i 'set-state! state)
                                       #t)
                                #f ))children))]
            [(eq? message 'ask-child) (λ (self child message . args)
                                        (let ([c (findf (λ (i) (equal? child (ask i 'name))) children)]);#f if not found, only returns first child with name
                                          (cond [(equal? c #f) (error "no child found with name:" child " acceptable children: " (ask self 'children-name))];no child found
                                                [(null? args);no extra args
                                                 (display c)];(ask c message)]
                                                [(equal? (length args) 1)
                                                 (ask c message (car args))]
                                                [else (error "code not setup for more then 1 extra arg")] ;might have more data in args thats needed!!!!
                                          )
                                          ))]
                                                
            [(eq? message 'set-furnace!) (λ (self state)
                                       (ask (ask self 'get-child furnace) 'set-state! state))]
            [(eq? message 'set-fan!) (λ (self state)
                                       (ask (ask self 'get-child fan) 'set-state! state))]
            [(eq? message 'set-fan!) (λ (self state)
                                       (ask (ask self 'get-child ac) 'set-state! state))]
            
            ;manual control only use 'set-temp-schedual! for automation
            ;if schedual is #f then time-time-off-day variables will be used
            [(eq? message 'set-temp-schedual!) (λ (self ) self)]
            [(eq? message 'set-temp!) (λ (self) self)]
            
            [(eq? message 'children) (λ (self) children)];list sub objects
            [(eq? message 'children-name) (λ (self) (map (λ (i) (ask i 'name)) children))]
            
            [else (get-method obj message)])
      )))
