#lang racket/gui
(provide key-down! key-up! key-down?)

(define keyboard (make-hasheq))
(define (key-down! char) (hash-set! keyboard char #t))
(define (key-up! char) (hash-set! keyboard char #f))
(define (key-down? char) (hash-ref keyboard char #f))