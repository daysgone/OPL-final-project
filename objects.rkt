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

;;base object, places, and objects and all derived objects
(define (make-named-object name . location)
  (lambda (message) 
    (cond [(eq? message 'name) (lambda (self) name)]
          [else (no-method name)])))

;physical objects, devices
(define (make-device name location)
  (let ((named-obj (make-named-object name location)))
    (lambda (message)
      (cond ((eq? message 'place)    (lambda (self) location))
            ((eq? message 'install) ;should never be used by user
             (lambda (self)
               (ask location 'add-thing self))) 
            ((eq? message 'set-place)
             (lambda (self new-place)
               (set! location new-place)
               'place-set))
            (else (get-method named-obj message))))))

(define (make&install-device name place)
  (let ((device-obj (make-device name place)))
    (ask device-obj 'install)
    device-obj))

;place/room
(define (make-place name)
  (let ([things '()]
        [named-obj (make-named-object name)])
    (lambda (message)
      (cond ((eq? message 'things) (lambda (self) things))
            ;; Following two methods should never be called by the user...
            ;;  they are system-internal methods. See CHANGE-PLACE instead.
            ((eq? message 'add-thing)
             (lambda (self new-thing)
               (cond ((memq new-thing things)
                      (display-message (list (ask new-thing 'name)
                                             "is already at" name)))
                     (else (set! things (cons new-thing things))))
               ))
            ((eq? message 'del-thing)
             (lambda (self thing)
               (cond ((not (memq thing things))
                      (display-message (list (ask thing 'name)
                                             "is not at" name))
                      false)
                     (else (set! things (delq thing things))    ;; DELQ defined
                           true))))                             ;; below

            (else (get-method named-obj message))))))

(define (make-led name place pin)
  (let ([timer 0]
        [pin 0]
        [state #f]
        [device-obj (make-device name place)])
  (lambda (message) 
    (cond [(eq? message 'led?) (lambda (self) true)]
          [(eq? message 'pin) (lambda (self) pin)]
          [(eq? message 'time-left) (lambda (self) timer)]
          [(eq? message 'set-timer) (lambda (self time) (set! timer time))]
          [else (get-method device-obj message)])
         )))

(define (make&install-led name place)
  (let ((led (make-led name place 13)))
    (ask led 'install) led))

