#lang racket/gui
(provide (all-defined-out))
(require "curve.rkt")
(require "Abstractions.rkt")


;(define red (make-object color% 255 0 0))
(define curves%
  (class object%
    (init-field
     [number-of-players 4]
     [players (list )])   ;Number of players, between 2 and 5.

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

    (define/public (check-collitions);Checks every possible ordered pair of curves. So it leads to (number-of-curves)^2 function calls..
      (map (lambda (x) (map (lambda (y) (send x collition? y)) players)) players))
    
    (define/public (update-positions)
      (map (lambda (x) (send x update-pos)) players))
    
    (define/public (update-velocities)
      (map (lambda (x) (send x update-vel)) players))
    
    (define/public (draw-curves dc)
      (map (lambda (x) (send x draw-curve dc)) players))
    (define/public (calculate-score)
      (map (lambda (x) (send x dead?)) players))
    (define/public (new-round)
      (map (lambda (x) (send x new-round)) players))
    (super-new)))


;(map (lambda (x) (when (send x died?)
;                    (map (lambda (y) (unless (send y dead?)
;                                         (send y addscore)) players))
;       players)