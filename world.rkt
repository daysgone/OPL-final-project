#lang racket
(include "objects.rkt")

(define conf-room     (make-place 'conf-room))
(define gen-device    (make&install-device 'device conf-room))
(define furnace-led   (make&install-led 'furnace conf-room))
(define AC-led   (make&install-led 'ac conf-room))

(define lights (list furnace-led AC-led))

(define (reset)
  (map (λ (i) (clear-arduino-pin! i)) lights))
  