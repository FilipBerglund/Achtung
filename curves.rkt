#lang racket/gui
(provide (all-defined-out))
(require "curve.rkt")
(require "powerup.rkt")
(require "Abstractions.rkt")
(define speed-powerup (new speed-powerup%
                           [color red]))
(define size-powerup (new size-powerup%
                          [color green]))
(define clear-powerup (new clear-powerup%
                           [color blue]))
(define collision-powerup (new collision-powerup%
                               [color yellow]))

(define curves%
  (class object%
    (init-field
     [number-of-players 4]
     [players (list )]
     [powerups (list speed-powerup size-powerup clear-powerup collision-powerup)])   ;Number of players, between 2 and 5.

    (define/public (make-curves)
      (set! players (helper number-of-players)))
    (define (helper number-of-players)
      (if (equal? number-of-players 0)
          '()
          (cons (new curve%
                     [curve_color (list-ref color-list (- number-of-players 1))] ;Every curve gets it's own color.
                     [left  (list-ref input-keys (* 2 (- number-of-players 1)))] ;Every curve gets it's own controls.
                     [right (list-ref input-keys (+ (* 2 (- number-of-players 1)) 1))])
                (helper (- number-of-players 1)))))

    (define/public (check-collisions);Checks every possible ordered pair of curves. So it leads to (number-of-curves)^2 function calls..
      (map (lambda (x) (map (lambda (y) (send x collision? y)) players)) players))
    
    (define/public (check-powerups)
      (map (lambda (x) (map (lambda (y) (send x collision? y)) powerups)) players))
    
    (define/public (update-positions)
      (map (lambda (x) (send x update-pos)) players))
    
    (define/public (update-velocities)
      (map (lambda (x) (send x update-vel)) players))
    
    (define/public (draw-curves dc)
      (map (lambda (x) (send x draw-curve dc)) players))
    
    (define/public (calculate-score)
      (map (lambda (x) (send x dead?)) players))
    
    (define/public (new-round)
      (map (lambda (x) (send x new-round)) (append powerups players)))
    
    (define/public (draw-powerups dc)
      (map (lambda (x) (send x update dc)) powerups))
    (define (calc-score)
      (map (lambda (x) (when (send x died?)
                         (map (lambda (y)
                                (unless (send y dead?)
                                  (send y addscore)))
                              players)))
           players))
    (define/public (display-score dc)
      (calc-score)
      (map (lambda (x)
             (send dc set-brush red 'transparent)
             (send dc draw-text (number->string (send x get-score))
                   (send x get-x-pos)
                   (- (send x get-y-pos) 30))) players))
    (super-new)))