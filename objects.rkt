;#lang racket
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

;;simple device
(define (make-simple-device-obj name pin-def)
  (let ([pin pin-def] ; defaults to some pin not on board
        [obj (make-obj name)])
    
    (lambda (message) 
      (cond [(eq? message 'pin) (lambda (self) pin)];physical pin location on ardino
            [(eq? message 'state?) (lambda (self) (is-arduino-pin-set? pin))];on/off?
            [(eq? message 'set-on) ;turn light on
             (lambda (self) (set-arduino-pin! pin))]
            [(eq? message 'set-off) ;turn light off
             (lambda (self)(clear-arduino-pin! pin))]
            
            ;;change state of led to what is passed in as arg
            [(eq? message 'set-state!) 
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
            [(eq? message 'switch-state) (lambda (self) (ask self 'set-state! (not(ask self 'state?))))]
                                                  
          [else (get-method obj message)])))
)


(define (make-led-obj name pin-def)
  (let ([timer -1] ; if neg then stays in current state till manually changed
        [time-on -1]
        [time-off -1]
        [intensity 0] ;range 0-255
        
        [device-obj (make-simple-device-obj name pin-def)])
    
    (lambda (message) 
      (cond [(eq? message 'intensity?) (lambda (self) intensity)]
            [(eq? message 'set-intensity!) (lambda (self level) (set! intensity level))]
            
            ;;may have timer outside of light????
            [(eq? message 'time-left?) (lambda (self) timer)] 
            [(eq? message 'set-timer!) (lambda (self time) (set! timer time))]
            
            [else (get-method simple-device-obj message)])
      )))

(define (make-HVAC-obj name pin-def)
  (let ([always-on #t];no schedualing
        [temp-morn -1]
        [time-morn 0]
        [temp-afternoon -1]
        [time-afternoon 0]
        [temp t]
        [temp-on (lambda (x) (- x 1))]
        [temp-off (lambda (x) (+ x 1))]
        [children '()] ;complex device can have simple devices as children
        [device-obj (make-simple-device-obj name pin-def)])
    
    (lambda (message) 
      (cond [(eq? message 'set-fan!) (lambda (self) self)]
            
            [else (get-method simple-device-obj message)])
      )))
