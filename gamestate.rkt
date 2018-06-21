#lang racket/gui
;Written by: FILIP BERGLUND.
;Here powerups are initiated. gamestate% organizes the curves of the
;game. Gives out points to curves. Checks if the game/round is over,
;has a function to draw a scoreboard. Checks collisions between objects in the
;game. Has functions to map update velocity, position, draw etc to all curves.
;Because gamestate% is a class you can easily implement functions to be able
;to have stored games and a active game.

;2018-05-24: Restructured how game-over?, end-round? and draw-end-screen
;(previously named game-over?, end-round? and end-round/game?) interact
;with each other.

;2018-05-25: Now the powerups are initiated in gamestate, so now the whole
;"gamestate" is encapsulated in gamestate%.

(provide gamestate%)
(require "curve.rkt")
(require "powerup.rkt")
(require "settings.rkt")

(define gamestate%
  (class object%
    (init-field
     ;Number of players, between 2 and 5.
     [number-of-players 2]
     [players (list )]
     [input-keys default-keys-list]
     ;The active powerups. The superpowerup is off by default.
     [powerups (list (new speed-powerup%
                           [color red])
                     (new size-powerup%
                          [color green])
                     (new clear-powerup%
                           [color actual-blue])
                     (new collision-powerup%
                               [color yellow]))]
     [superpowerup (new super-powerup%
                          [color gray]
                          [x-pos 90]
                          [y-pos (- (/ frame-height 2) 60)]
                          [variance 1]
                          [spawn-duration 1000]
                          [effect-duration 1000])])

    ;Returns #t if the curves are initiated, else #f.
    ;OUT: bool
    (define/public (curves-initiated?)
      (not (null? players)))

    ;Turns the superpowerup on or off.
    ;If x=1 then on else off.
    ;IN: 0 or 1 OUT: void.
    (define/public (set-superpowerup-on x)
      (if (equal? x 1)
          (set! powerups (cons superpowerup powerups))
          (set! powerups (remove superpowerup powerups))))

    ;To check if superpowerup is on.
    ;OUT: bool
    (define/public (superpowerup-on?)
      (if (equal? (memq superpowerup powerups) #f)
          #f #t))

    ;IN: int OUT: void
    (define/public (set-number-of-players nr)
      (set! number-of-players nr))
    
    ;OUT: int
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
    ;So it leads to (number-of-curves)^2 function calls.
    ;IN: void OUT: void
    (define/public (check-collisions)
      (map (lambda (a-curve)
             (map (lambda (another-curve)
                    (send a-curve collision?
                          another-curve
                          (send a-curve get-bitmap-level)))
                  players))
           players))

    ;Checks collisions with powerups. Works even when collision is off.
    ;IN: void OUT: void
    (define/public (check-powerups)
      (map (lambda (a-curve)
             (map (lambda (a-powerup)
                    (send a-curve collision? a-powerup
                          (send a-curve get-bitmap-level)))
                  powerups))
           players))

    ;IN: void OUT: void
    (define/public (update-positions)
      (map (lambda (a-curve) (send a-curve update-pos)) players))

    ;IN: void OUT: void
    (define/public (update-velocities)
      (map (lambda (a-curve) (send a-curve update-vel)) players))

    ;IN: void OUT: void
    (define/public (draw-curves dc)
      (map (lambda (a-curve) (send a-curve draw-curve dc)) players))

    ;IN: void OUT: void
    (define/public (reset-scores!)
      (map (lambda (a-curve) (send a-curve reset-score!)) players))

    ;Resets the relevant variables and removes powerups from curves.
    (define/public (new-round)
      (map (lambda (a-curve) (send a-curve new-round)) (append powerups players)))

    (define/public (draw-powerups dc)
      (map (lambda (a-powerup) (send a-powerup update dc)) powerups))

    ;Curves get a point if they are alive while another player dies.
    (define (calc-score)
      (map (lambda (a-curve)
             (when (send a-curve died?)
               (map (lambda (another-curve)
                      (unless (send another-curve get-dead)
                        (send another-curve addscore)))
                    players)))
           players))

    ;Draws the current score on dc and sorts it with the person with the highest
    ;score at the top.
    ;IN: a-dc OUT: void
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
    ;IN: void OUT: bool
    (define/public (game-over?)
      (ormap (lambda (x) (<= (* (sub1 number-of-players) 10) (send x get-score)))
             players))

    ;When one or less players are alive, the round ends.
    ;IN: void OUT: bool
    (define/public (round-over?)
      (let ((number-of-dead-players 0))
        (map (lambda (a-curve)
               (when (send a-curve get-dead)
                 (set! number-of-dead-players (add1 number-of-dead-players))))
             players)
        (>= (add1 number-of-dead-players) number-of-players)))
    
    ;Draws things on dc when game-over? and round-over?
    ;are true or when round-over? is true.
    ;IN: dc OUT: void
    (define/public (draw-end-screen dc)
      (send dc set-text-foreground white)
      (cond ((and (game-over?) (round-over?))
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
            ((round-over?)
             (send dc draw-text "ROUND OVER"
                   (- (/ (- frame-width 250) 2) 185)
                   (- (/ frame-height 2)  30)))))                                                  
    (super-new)))