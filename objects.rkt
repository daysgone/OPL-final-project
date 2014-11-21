#lang racket
(require "firmata.rkt")

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

(define (make-named-object name)
  (lambda (message) 
    (cond [(eq? message 'name) (lambda (self) name)]
          )
         ))
          
(define (make-place name)
  (let ((things       '())
        (named-obj (make-named-object name)))
    (lambda (message)
      (cond [(eq? message 'add-thing) 
              (lambda (self) things)]
             [(eq? message 'del-thing) 
              (lambda (self thing) #t)]
             [else (get-method named-obj message)]
             ))))
      
(define (make-led name)
  (let ([timer 0]
        [pin 0]
        [state #f])
  (lambda (message) 
    (cond [(eq? message 'name) (lambda (self) name)]
          [else (get-method named-obj message)])
         )))