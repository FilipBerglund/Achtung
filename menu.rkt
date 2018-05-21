#lang racket
(require "keyhandler.rkt")
(define menu%
  (class object%
    (init
     [bitmap (make-bitmap 710 1050 #f 0.5)]
     [bitmap-dc (new bitmap-dc% [bitmap curve-bitmap])]
     [menu-x-position 0]
     [menu-y-position 0]
     [active #t]
     [x-pos 250]
     [y-pos 300])
    (define/public (update-state)
      (cond ((key-down? 'escape)
             (set! active #t))
            ((key-down? 'up)
             (unless (equal? menu-state 0)
               (set! menu-state (add1 menu-state))))
            ((key-down? 'down)
             (unless (equal? menu-state 5)
               (set! menu-state (add1 menu-state))))
            ((key-down? 'left)
             (menu-y-pos
            ((key-down? 'enter)
             (activate menu-position))
            