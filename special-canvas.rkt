#lang racket/gui
(require "keyhandler.rkt")
(provide special-canvas%)
(define special-canvas%
  (class canvas%
    (define/override (on-char event)
      (define key (send event get-key-code))
      (define release (send event get-key-release-code))
      (when (eq? release 'press)
        (key-down! key))
      (when (eq? key 'release)
        (key-up! release)))
    (super-new)))