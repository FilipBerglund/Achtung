#lang racket/gui
;In this file objects.





(provide gamestate%)
(require "curve.rkt")
(require "powerup.rkt")
(require "Abstractions.rkt")

(define speed-powerup (new speed-powerup%
                           [color red]))
(define size-powerup (new size-powerup%
                          [color green]))
(define clear-powerup (new clear-powerup%
                           [color actual-blue]))
(define collision-powerup (new collision-powerup%
                               [color yellow]))
(define superpowerup (new super-powerup%
                          [color gray]
                          [x-pos 90]
                          [y-pos (- (/ frame-height 2) 60)]
                          [variance 1]
                          [spawn-duration 1000]
                          [effect-duration 1000]))

(define gamestate%
  (class object%
    (init-field
     [number-of-players 2] ;Number of players, between 2 and 5. For debugging
     ;1 player is okay but then the function end-round/game? (in this file) needs
     ;to be modified so that the game clock isn't stoped when the game ends.
     [players (list )]
     [input-keys default-keys-list]
     ;The active powerups.
     [powerups (list speed-powerup
                     size-powerup
                     clear-powerup
                     collision-powerup)])

    ;Returns #t if the curves are initiated, else #f.
    (define/public (curves-initiated?)
      (not (null? players)))
    
    (define/public (set-superpowerup-on x)
      (if (equal? x 1)
          (set! powerups (cons superpowerup powerups))
          (set! powerups (remove superpowerup powerups))))
    
    (define/public (superpowerup-on?)
      (if (equal? (memq superpowerup powerups) #f)
          #f #t))
    
    (define/public (set-number-of-players nr)
      (set! number-of-players nr))
    (define/public (get-number-of-players)
      number-of-players)
    
    ;Creates the curves and sets players to this list of curves.
    (define/public (make-curves)
      (set! players (creator number-of-players)))
    (define (creator number-of-players)
      (if (equal? number-of-players 0)
          '()
          (cons (new curve%
                     [name (list-ref name-list (- number-of-players 1))]
                     ;Every curve gets it's own color.
                     [curve_color (list-ref color-list (- number-of-players 1))]
                     ;Every curve gets it's own controls.
                     [left  (list-ref input-keys (* 2 (- number-of-players 1)))]
                     [right (list-ref input-keys (+ (* 2 (- number-of-players 1)) 1))])
                (creator (- number-of-players 1)))))

    ;Checks every possible ordered pair of curves.
    ;So it leads to (number-of-curves)^2 function calls..
    (define/public (check-collisions)
      (map (lambda (x) (map (lambda (y) (send x collision? y (send x get-bitmap-level)))
                            players)) players))

    ;Checks collitions with powerups. Works even when collition is off.
    (define/public (check-powerups)
      (map (lambda (x) (map (lambda (y) (send x collision? y (send x get-bitmap-level)))
                            powerups)) players))

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
      (map (lambda (x)
             (when (send x died?)
               (map (lambda (y)
                      (unless (send y get-dead)
                        (send y addscore)))
                    players)))
           players))

    ;Draws the current score on dc and sorts it with the person with the highest
    ;score at the top.
    (define/public (display-score dc)
      (calc-score)
      (let ((position 1))
        ;Sorts the players based on score.
        (set! players (sort players #:key (lambda (x) (send x get-score)) >))
        (map (lambda (x)
               (send dc set-text-foreground (send x get-color))
               (send dc set-font big-font)
               (send dc draw-text
                     (string-join (list
                                   (number->string (send x get-score))
                                   (if (equal? (send x get-score) 1)
                                       "pt"
                                       "pts")))
                     (- frame-width 220) (* 60 position))
               (set! position (add1 position))) players)))
    
    ;The game is over when one curve get enough points.
    (define/public (game-over?)
      (ormap (lambda (x) (<= (* (sub1 number-of-players) 10) (send x get-score)))
             players))

    ;When one or less players are alive, the round ends.  
    (define/public (round-over?)
      (let ((number-of-dead-players 0))
        (map (lambda (x)
               (when (send x get-dead)
                 (set! number-of-dead-players (add1 number-of-dead-players))))
             players)
        (>= (add1 number-of-dead-players) number-of-players)))
    
    ;Ends the round or the game. Displays nice things on the canvas.
    ;Also returns a boolean.
    (define/public (draw-end-screen dc)
      (send dc set-text-foreground white)
      (cond ((and (send this game-over?) (send this round-over?))
             (send dc draw-text "GAME OVER"
                   ;To center it on the game-part of the frame.
                   ;It looks hacky but it's actually thought through.
                   (- (/ (- frame-width 250) 2) 180)
                   (- (/ frame-height 2) 60))
             (send dc draw-text
                   (string-join
                    ;Players is sorted so the first is the leader.
                    (list (send (car players) get-name) "WINS!"))
                   (- (/ (- frame-width 250) 2) 180)
                   (/ frame-height 2)))
            ((send this round-over?)
             (send dc draw-text "ROUND OVER"
                   (- (/ (- frame-width 250) 2) 185)
                   (- (/ frame-height 2)  30)))))                                                  
    (super-new)))