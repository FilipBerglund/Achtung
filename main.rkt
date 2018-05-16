#lang racket/gui
(require "special-canvas.rkt")
(require "curves.rkt")
(require "powerup.rkt")
(require "Abstractions.rkt")
(define black (make-object color% 0 0 0))


(define players (new curves%
                     [number-of-players 2]))
(send players make-curves)

;;The main window
(define *game-frame* (new frame%
                          (label "Achtung, die Kurve!")
                          (height 710)
                          (width 1050)))
(send *game-frame* show #t)

(define (drawing-proc canvas dc)
  (let ([startTime (current-inexact-milliseconds)])
    (send dc set-pen white 15 'solid)
    (send dc draw-line 10 600 10 10)
    (send dc draw-line 10 10 800 10)
    (send dc draw-line 800 10 800 600)
    (send dc draw-line 10 600 800 600)
    (send players draw-powerups dc)
    (send players update-velocities)
    (send players draw-curves dc)
    (send players update-positions)
    (send players check-collisions)
    
    (send players display-score dc)
    (send players end-round? dc game-clock)
    (displayln (- (current-inexact-milliseconds) startTime))
    (send players check-powerups)
    ))

;This is where the game is played
(define *game-window*
  (new special-canvas%
       [parent *game-frame*]
       [paint-callback drawing-proc]))
(send *game-window* set-canvas-background black)
;Updates the game
(define (*render-fn*)
  (send *game-window* refresh-now))

;Sets the framerate and calls render-fn that updates the game and draws to the screen
(define game-clock
  (new timer%
       [notify-callback *render-fn*]
       [just-once? #f]))

(define Start
  (new button%
       [parent *game-frame*]
       [label "Start"]
       [callback (lambda (button event)
                   (send *game-window* focus)
                   (send Start set-label "playing..")
                   (send game-clock start 12 #f))]))
;(define Pause
;  (new button%
;       [parent *game-frame*]
;       [label "Pause"]
;       [callback (lambda (button event)
;                   (send Pause set-label "Paused..")
;                   (send Start set-label "Restart")
;                   (send game-clock stop))]))

(define New-round
  (new button%
       [parent *game-frame*]
       [label "NEW ROUND"]
       [callback (lambda (botton event)
                   (send players new-round)
                   (send game-clock start 12 #f)
                   (send *game-window* focus))]))