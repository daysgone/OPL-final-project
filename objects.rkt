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
         [intensity -1]
         )
    
    ;;set things on creation
    (set-pin-mode! pin mode);sets mode 
    
    (λ (message) 
      (cond [(eq? message 'get-intensity) (λ (self) intensity)]
            [(eq? message 'pin);returns int 
             (λ (self) pin)];physical pin location on ardino
            ;[(eq? message 'mode);returns defined ints (INPUT_MODE/OUTPUT_MODE/ANALOG_MODE/PWM_MODE etc... from firmata.rkt)
            ; (λ (self) mode)]
            [(eq? message 'set-mode!) (λ (self new-mode) (set-pin-mode! pin new-mode))];;prob shouldnt be used
            
            [(eq? message 'state?);returns bool, #t is on
             (λ (self) (if(equal? mode PWM_MODE)
                          (if (> intensity 0) #t #f); has no state
                          (is-arduino-pin-set? pin)))]
            
            [(eq? message 'set-on!) ;turn light on
             (λ (self . value)
               (if(equal? mode PWM_MODE)
                  (let([v (if (null? value) 255 (car value))])
                    (analog-write! pin v)
                    (set! intensity v));internal intensity since we can ask current status
                  (set-arduino-pin! pin)))]
            
            [(eq? message 'set-off!) ;turn light off
             (λ (self)
               (if(equal? mode PWM_MODE)
                  (begin
                    (analog-write! pin 0)
                    (set! intensity 0));internal intensity since we can ask current status
                  (clear-arduino-pin! pin)))]
            
            [(eq? message 'set-state!) ;;change state of led to what is passed in as arg
             (λ (self state . value) (cond [(and (boolean? state) (null? value) (equal? state #t) )
                                            (ask self 'set-on!)]
                                           [(and (boolean? state) (equal? state #t) )
                                            (ask self 'set-on! (car value))
                                            (set! intensity (car value))]
                                           [(boolean? state)
                                            (ask self 'set-off!)]
                                           [else (error "please use a boolean value")])
               )]
            
            ;;toggle led state
            [(eq? message 'switch-state);toggles current state
             (λ (self) (if(equal? mode OUTPUT_MODE)
                          (ask self 'set-state! (not (ask self 'state?)))
                          (ask self 'set-state! #t (abs (- 255 intensity)))))]
            
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
            [(eq? message 'value);returns 0-1023
             (λ (self) (read-analog-pin pin))]
            
            
            [else (get-method obj message)])))
  )
#|-----------------------------------------
     HVAC - includes "furnace" and "ac" objects????
     !-> defaults to manual temp control unless at least 1 on/off time is used
-------------------------------------------|#
(define (make-HVAC-obj name) 
  (let* ([temp 65]
         [temp-range .5];sets high/low for HVAC to kick on defaults to .5 degree dif
         ;[temp-on (- temp temp-range)]
         ;[temp-off (+ temp temp-range)]
         [schedual #f];manual temp change only(overides current schedual until next schedualed time triggers)
         
         ;;unused at this time
         [time-morn (cons (cons 0 0) -1)]; time/temp ((12.00). 65) temp in F
         [time-afternoon (cons (cons 0 0) -1)]
         [time-evening (cons (cons 0 0) -1)]
         [time-night (cons (cons 0 0) -1)]
         [time-schedual (list time-morn time-afternoon time-evening time-night)]
         #|----------
                           sub objects of device 
                                                 ---------------|#
         [furnace (make-simple-device-obj 'furnace 11 OUTPUT_MODE)];hardcoded pin
         [fan (make-simple-device-obj 'fan 12 OUTPUT_MODE)];hardcoded pin
         [ac (make-simple-device-obj 'ac 13 OUTPUT_MODE)];hardcoded pin
         
         [thermostat (make-simple-analog-sensor-obj 'therm 0 INPUT_MODE)];
         ;complex device can have simple device sub objects 
         [children (list furnace fan ac)]
         [sensors (list thermostat)];cannot be set off
         
         [all-children (flatten (list children sensors))]
         
         
         [obj (make-obj name)])
    
    (λ (message) 
      (cond [(eq? message 'ask-child) (λ (self child message . args);work with just 1 child
                                        (let ([c (findf (λ (i) (equal? child (ask i 'name))) all-children)]);#f if not found, only returns first child with name
                                          (cond [(equal? c #f) (error "no sub-object found with name:" child )];no child found
                                                [(null? args);no extra args
                                                 (ask c message)]
                                                [(equal? (length args) 1)
                                                 (ask c message (car args))]
                                                [else (error "code not setup for more then 1 extra arg")] ;might have more data in args thats needed!!!!
                                                )
                                          ))]
            ;;shows all sub objects state, really only the fan and either then ac/furnace  
            [(eq? message 'state?)(λ (self)
                                    (for-each (λ (i) (ask i 'state?)) children))]
            ;controls of whole system is off 
            [(eq? message 'set-off!)(λ (self)
                                      (for-each (λ (i) (ask i 'set-state! #f)) children))]
            
            ;;control whats on
            [(eq? message 'set-state!)(λ (self state)
                                        (cond [(and (boolean? state) state)
                                               ;furnace only
                                               (begin (ask self 'set-child! 'furnace #t)
                                                      (ask self 'set-child! 'ac #f))]
                                              [(boolean? state)
                                               ;ac only
                                               (begin (ask self 'set-child! 'furnace #f)
                                                      (ask self 'set-child! 'ac #t))]
                                              ;set both off
                                              [(equal? 'off state)
                                               ;shuts system off
                                               (ask self 'set-off!)]
                                              [else (error "#t for furnace, #f for ac , 'off for system off")]))]
            
            
            ;set individual sub-objects
            [(eq? message 'set-child!) (λ (self child state)
                                         (ask self 'ask-child child 'set-state! state))]
            
            
            ;;externally F
            [(eq? message 'cur-temp) (λ (self) (degreesF)) ]
            [(eq? message 'temp) (λ (self) temp) ]
            
            ;;internally C
            [(eq? message 'run) (λ (self)
                                  (printf "Current temp  ~a C\t ~a F\n" (round (degreesC)) (round (degreesF)))
                                  (cond [(<= (degreesC) (- (F->C temp) temp-range));turn on furnace when x degrees below temp set
                                         (ask self 'set-furnace! #t)]
                                        [(>= (degreesC) (+ (F->C temp) temp-range))
                                         (ask self 'set-furnace! #f)]
                                        [else ("in no change temp range\n")]
                                        
                                        ))]
            
            
            ;manual control only use 'set-temp-schedual! for automation
            ;if schedual is #f then time-time-off-day variables will be used
            [(eq? message 'set-temp-schedual!) (λ (self ) self)]
            [(eq? message 'set-temp!) (λ (self value) (set! temp value))]
            
            [(eq? message 'children) (λ (self) children)];list sub objects
            [(eq? message 'children-name) (λ (self) (map (λ (i) (ask i 'name)) children))]
            
            [else (get-method obj message)])
      )))
