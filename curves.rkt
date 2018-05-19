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
(define super-powerup (new super-powerup%
                           [color orange]
                           [x-pos 200]
                           [y-pos 300]
                           [spawn-duration 500]
                           [effect-duration 500]))

(define gamestate%
  (class object%
    (init-field
     [number-of-players 4] ;Number of players, between 2 and 5.
     [players (list )]
     [powerups (list speed-powerup
                     size-powerup
                     clear-powerup
                     collision-powerup
                     super-powerup
                     )])   

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

    ;Checks every possible ordered pair of curves.
    ;So it leads to (number-of-curves)^2 function calls..
    (define/public (check-collisions)
      (map (lambda (x) (map (lambda (y) (send x collision? y (send x get-bitmap-level))) players)) players))

    ;Checks collitions with powerups. Works even when collition is off.
    (define/public (check-powerups)
      (map (lambda (x) (map (lambda (y) (send x collision? y (send x get-bitmap-level))) powerups)) players))

    (define/public (update-positions)
      (map (lambda (x) (send x update-pos)) players))
    
    (define/public (update-velocities)
      (map (lambda (x) (send x update-vel)) players))

    (define/public (draw-curves dc)
      (map (lambda (x) (send x draw-curve dc)) players))

    (define/public (reset-scores!)
      (map (lambda (x) (send x reset-score!)) players))

    ;Resets the relevant variables and removes powerups from curves.
    (define/public (new-round)
      (map (lambda (x) (send x new-round)) (append powerups players)))
    
    (define/public (draw-powerups dc)
      (map (lambda (x) (send x update dc)) powerups))

    ;Curves get a point if they are alive while another player dies.
    (define (calc-score)
      (map (lambda (x) (when (send x died?)
                         (map (lambda (y)
                                (unless (send y get-dead)
                                  (send y addscore)))
                              players)))
           players))
    
    (define/public (stop-holes)
      (map (lambda (x) (send x set-hole! #f)) players))

    ;Displays the current score on dc and sorts it with the person with the highest
    ;score at the top.
    (define/public (display-score dc)
      (calc-score)
      (let ((position 1))
        ;Sorts the players based on score.
        (set! players (sort players #:key (lambda (x) (send x get-score)) >))
        (map (lambda (x)
               (send dc set-text-foreground (send x get-color))
               (send dc set-font a-font)
               (send dc draw-text
                     (string-join (list
                                   (number->string (send x get-score))
                                   (if (equal? (send x get-score) 1)
                                       "pt"
                                       "pts")))
                     830 (* 60 position))
               (set! position (add1 position))) players)))
    
    ;The game is over when one curve get enough points.
    (define (game-over?)
      (ormap (lambda (x) (<= (* (sub1 number-of-players) 10) (send x get-score)))
             players))

    ;The round is over when all but one player is dead.
    (define (round-over?)
      (let ((dead-players 0))
        (map (lambda (x) (when (send x get-dead)
                           (set! dead-players (add1 dead-players))))
             players)
        (equal? (add1 dead-players) number-of-players)))
        
    
    ;Ends the round or the game. Displays nice things on the canvas.
    (define/public (end-round/game? dc clock)
      (send dc set-text-foreground white)
      (cond ((and (game-over?) (round-over?))
             (send dc draw-text "GAME OVER" 250 240)
             (send dc draw-text (string-join
                                 ;Players is sorted so the first is the leader.
                                 (list (send (car players) get-name)
                                       "WINS!")) 250 300)
             ;(send clock stop);To be added later but now it's not
             ;active for debugging purposes.
             )
            ((round-over?)
             (send dc draw-text "ROUND OVER" 250 280)
             (send clock stop))))                                                  
    (super-new)))