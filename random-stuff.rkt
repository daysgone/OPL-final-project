#lang racket

;test range with streams
(define (my-range a b step)
  (stream->list (in-range a b step)))

;(for ([i (in-range 0 255 5)]) (printf "~a " (- 250 i)))


;;email test
(require racket/list openssl/mzssl net/imap net/head)
(define (email-test)
  (define imap-server "imap-mail.outlook.com")
  (define imap-port-no 993)
  (define username "xmtbikerx@hotmail.com")
  (define pw "")
  (define mailbox-name "home-automation")
  
  
  
  (define (test-connect)
    (let ([c (ssl-make-client-context)])
      (let-values ([(in out) (ssl-connect imap-server imap-port-no c)])
        (imap-connect* in out username pw mailbox-name))))
  
  (define-values [imap messages recent] (test-connect))
  
  (define (get-interesting-headers ns)
    (for/list ([x (imap-get-messages imap ns '(header))])
      (filter-map
       (λ (x)
         (define s
           (string->symbol (string-downcase (bytes->string/utf-8 (car x)))))
         (and (memq s '(from to date subject body))
              (cons s (bytes->string/utf-8 (cdr x)))))
       (extract-all-fields (car x))
       )))
    
  (get-interesting-headers (for/list ([i messages]) (add1 i)))
  )

;;;time test
(require racket/date)
(define (time-test)
  ;; Given a day, month, and year, return the weekday
  (define (day-month-year->weekday day month year)
    (define local-secs (find-seconds 0
                                     0
                                     0
                                     day
                                     month
                                     year
                                     #t))
    (define the-date (seconds->date local-secs))
    (vector-ref #("sunday" "monday" "tuesday" "wednesday" "thursday"
                           "friday" "saturday")
                (date-week-day the-date)))
  
  
  (define d (date 0    ;sc
                  2    ;mn
                  3    ;hr
                  20   ;day
                  8    ;month
                  2012 ;year
                  0    ;weekday  <<<
                  0    ;year-day <<<
                  #f   ;dst?
                  0    ;time-zone-offset
                  ))
  
  (displayln (seconds->date (date->seconds d)))
  (date->string d)
  (date->string (current-date)))



(require racket/gui)
; DrRacket, version 5.2.1
; 情報科学類ソフトウェアサイエンス主専攻実験
; M.NAKAJIMA
; 2012/06/29
 
; Create a window and show message.
(define frame (new frame% [label "Timer"]))
(define msg (new message% [parent frame]
                 [label "Press Start button to start the timer."]))
 
; begin is the time when "Start" button is pressed (in milliseconds)
; diff is (- (current-inexact-milliseconds) begin)
(define begin1 0.0)
(define diff 0.0)
 
; Timer callback
(define timer (new timer%
                   [notify-callback (lambda ()
                                               (set! diff (- (current-inexact-milliseconds) begin1))
                                               (send msg set-label (number->string diff)))]
                   [interval #f]))
 
; Start button
(new button% [parent frame] [label "Start"]
     (callback (lambda (button event)
                 (set! begin1 (- (current-inexact-milliseconds) diff))
                 (send timer start 10))))
 
; Stop button
(new button% [parent frame]
     [label "Stop"]
     (callback (lambda (button event)
                 (send timer stop))))
 
; Reset button
(new button% [parent frame]
     [label "Clear"]
     (callback (lambda (button event)
                 (set! diff 0)
                 (set! begin1 (current-inexact-milliseconds))
                 (send msg set-label (number->string diff))
                 )))
 
; show window
(send frame show #t)