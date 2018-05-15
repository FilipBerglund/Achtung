#lang racket/gui
(require "special-canvas.rkt")
(require "curves.rkt")
(require "powerup.rkt")
(define black (make-object color% 0 0 0))

(define players (new curves%
                     [number-of-players 2]))
(send players make-curves)

;;The main window
(define *game-frame* (new frame%
                          (label "Achtung, die Kurve!")
                          (height 1080)
                          (width 1920)))
(send *game-frame* show #t)

(define (drawing-proc canvas dc)
  (let ([startTime (current-inexact-milliseconds)])

    (send dc set-pen black 15 'solid)
    (send dc draw-line 0 600 0 0)
    (send dc draw-line 0 0 800 0)
    (send dc draw-line 800 0 800 600)
    (send dc draw-line 0 600 800 600)
    (send players draw-powerups dc)
    (send players update-velocities)
    (send players draw-curves dc)
    (send players update-positions)
    (send players check-collisions)
    (send players check-powerups)

    (displayln (- (current-inexact-milliseconds) startTime))))

;This is where the game is played
(define *game-window*
  (new special-canvas%
       [parent *game-frame*]
       [paint-callback drawing-proc]))

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
                   (send Start set-label "playing..")
                   (send game-clock start 12 #f))]))

(define Pause
  (new button%
       [parent *game-frame*]
       [label "Pause"]
       [callback (lambda (button event)
                   (send Pause set-label "Paused..")
                   (send Start set-label "Restart")
                   (send game-clock stop))]))

(define Reset
  (new button%
       [parent *game-frame*]
       [label "Reset"]
       [callback (lambda (botton event)
                   (send players new-round)
                   (send game-clock stop)
                   (send (send *game-window* get-dc) clear))]))