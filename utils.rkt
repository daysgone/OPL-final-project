;password for website opluml14

#|-----------------------------------------
            Global vars

-------------------------------------------|#
(define clock-speed 1) ;; 0.005 is the tested max propogation speed.
(define debug #f)
(define env '()) ; create empty env


#|-----------------------------------------
           utility functions

-------------------------------------------|#
(define (my-range a b step)
  (stream->list (in-range a b step))) ;overcomplicated use of strem ;P



#|-----------------------------------------
            pin based functions

-------------------------------------------|#
(define (set-on! pin)
  (set-arduino-pin! pin)
)
(define (set-off! pin)
  (clear-arduino-pin! pin)  
)

(define (clear-pins group)
  (for-each (λ (x);if u use map it returns values which are not needed for this
              (ask x 'set-off)
              ) group)
  )
(define (reset-system)
  (for-each (lambda (x) (clear-pins x)) env))


#|-----------------------------------------
            group based functions

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

#|
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
