;#lang racket
(require "firmata.rkt")
(include "utils.rkt")

(define clock-speed .2) ;; 0.005 is the tested max propogation speed.

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
(define (make-device-obj name  )
  (lambda (message) 
    (cond [(eq? message 'name) (lambda (self) name)]
          [else (no-method name)])))


(define (make-led-obj name pin-def)
  (let ([cur-state #f] ; defaults to off
        [intensity 255] ;range 0-255
        [pin pin-def] ; defaults to some pin not on board
        ;timer is a simple count down would like to use actual time as well for turning off and on
        [timer -1] ; if neg then stays in current state till manually changed
        [time-on -1]
        [time-off -1]
        
        [device-obj (make-device-obj name)])
    
    (lambda (message) 
      (cond [(eq? message 'pin) (lambda (self) pin)]
            [(eq? message 'state?) (lambda (self) cur-state)]
            [(eq? message 'set-high) 
             (lambda (self) 
               (begin (set! cur-state #t)
                      (set-arduino-pin! pin)))]
            [(eq? message 'set-low) 
             (lambda (self) 
               (begin (set! cur-state #f)
                      (clear-arduino-pin! pin)))]
            ;;change state of led to what is passed in as arg
            [(eq? message 'set-state!) 
             (lambda (self state) (set! cur-state (test-bool state) ))]
            ;;toggle led state
            [(eq? message 'switch-state) (lambda (self) (set! cur-state (not cur-state)))]
            [(eq? message 'intensity) (lambda (self) intensity)]
            [(eq? message 'set-intensity!) (lambda (self level) (set! intensity level))]
            
            [(eq? message 'time-left) (lambda (self) timer)] 
            [(eq? message 'set-timer!) (lambda (self time) (set! timer time))]
            [else (get-method device-obj message)])
      )))


;; updated firmata functions with more functionality
(define (my-set-pin-mode! pin mode)
  (set-pin-mode! pin mode)
  (sleep clock-speed))

(define (set-high! pin)
  (set-arduino-pin! pin)
)
(define (set-low! pin t)
  (clear-arduino-pin! pin)
  
)
(define (clear-pins group)
  (for-each (λ (x);if u use map it returns values which are not needed for this
              (ask x 'set-low)
              ) lights)
  )
