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
      (set! players (creator number-of-players)))
    (define (creator number-of-players)
      (if (equal? number-of-players 0)
          '()
          (cons (new curve%
                     [name (list-ref name-list (- number-of-players 1))]
                     [curve_color (list-ref color-list (- number-of-players 1))] ;Every curve gets it's own color.
                     [left  (list-ref input-keys (* 2 (- number-of-players 1)))] ;Every curve gets it's own controls.
                     [right (list-ref input-keys (+ (* 2 (- number-of-players 1)) 1))])
                (creator (- number-of-players 1)))))

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

    ;Displays the current score and sorts it with the person with the higest score at the top.
    (define/public (display-score dc)
      (calc-score)
      (let ((position 1))
        (set! players (sort players #:key (lambda (x) (send x get-score)) >))
        (map (lambda (x)
               (send dc set-text-foreground (send x get-color))
               (send dc draw-text (string-join (list (send x get-name)
                                                     (number->string (send x get-score)) "pt"))
                     850 (* 40 position))
               (set! position (add1 position))) players)))
    (define (end-game?)
      (ormap (lambda (x) (<= (* (- number-of-players 1) 10) (send x get-score))) players))
                     
    
    (define/public (end-round? dc clock)
      (let ((dead-players 0))
        (send dc set-text-foreground white)
        (map (lambda (x) (when (send x dead?) (set! dead-players (add1 dead-players)))
               (cond ((and (end-game?) (equal? (add1 dead-players) number-of-players))
                      (send dc draw-text (string-join (list "GAME OVER""\n"
                                                            (send (car players) get-name)
                                                            "WINS!")) 350 300))
                     ((equal? (add1 dead-players) number-of-players)
                      (send dc draw-text "ROUND OVER" 350 300)
                      (send clock stop))))
             players)))
                                                                        
    (super-new)))