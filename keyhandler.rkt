#lang racket/gui
(provide (all-defined-out))

(define keyboard (make-hasheq))
(define (key-down! char) (hash-set! keyboard char #t))
(define (key-up! char) (hash-set! keyboard char #f))
(define (key-down? char) (hash-ref keyboard char #f))