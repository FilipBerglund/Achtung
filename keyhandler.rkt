#lang racket/gui
;Written by: KRISTOFFER BRANDER
;The purpose of this file is to store key presses. With this you
;can ask (in files that require this one) whether or not a key
;is down. These functions are used in curve% (curve.rkt) and special-canvas%
;(special-canvas.rkt). The special-canvas% will keep which keys are pressed
;updated.

;2018-05-09: New.
;2018-05-24: Added comments.

(provide key-down! key-up! key-down?)

;Stores characters with booleans.
(define keyboard (make-hasheq))

;Stores a char with a bool in keyboard.
(define (key-down! char) (hash-set! keyboard char #t))
(define (key-up! char) (hash-set! keyboard char #f))

;Checks if char is presses. IN: virtual key code OUT: bool
(define (key-down? char) (hash-ref keyboard char #f))