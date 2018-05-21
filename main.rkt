#lang racket/gui
(require "special-canvas.rkt")
(require "curves.rkt")
(require "powerup.rkt")
(require "Abstractions.rkt")
(require "keyhandler.rkt")
(require "menu-item.rkt")

(define haze-bitmap (make-object bitmap% 1050 610 #f 0.5))
(define haze-dc (new bitmap-dc% [bitmap haze-bitmap]))
(send haze-dc set-alpha 0.2)
(send haze-dc set-brush gray 'solid )
(send haze-dc draw-rectangle 0 0 1050 610)

;;The main window
(define game-frame (new frame%
                        (label "Achtung, die Kurve!")
                        (height 610)
                        (width 1050)))
(send game-frame show #t)

(define (draw-playingfield-frame dc)
  (send dc set-pen yellow 6 'solid)
  (send dc draw-line 10 600 10 10)
  (send dc draw-line 10 10 800 10)
  (send dc draw-line 800 10 800 600)
  (send dc draw-line 10 600 800 600))

(define gamestate1 (new gamestate%))

(define (drawing-proc canvas dc)
  (let ((show-menu (send canvas show-menu?)))
    (cond (show-menu
           (send gamestate1 display-score dc)
           (send gamestate1 draw-curves dc)
           (draw-playingfield-frame dc)
           (send dc draw-bitmap haze-bitmap 0 0 'solid)
           (send gamestate1 end-round/game? dc)
           (send game-canvas draw-menu dc)
           )
          (else
           (draw-playingfield-frame dc)
           (send gamestate1 update-velocities)
           (send gamestate1 draw-powerups dc)
           (send gamestate1 draw-curves dc)
           (send gamestate1 update-positions)
           (send gamestate1 check-collisions)
           (send gamestate1 display-score dc)
           (send gamestate1 check-powerups)
           (when (send gamestate1 end-round/game? dc)
             (send game-canvas set-show-menu! #t)
             (send game-canvas set-menu-row-to-0))
           ))))

;This is where the game is played
(define game-canvas
  (new special-canvas%
       [parent game-frame]
       [paint-callback drawing-proc]))
(send game-canvas set-canvas-background black)

;A menu item that lets you start a new game with a set number of players.
(define number-of-players
  (new menu-item%
       [parent game-canvas]
       [callback
        (lambda (arg)
            (send gamestate1 set-number-of-players arg)
            (send gamestate1 make-curves)
            (send gamestate1 new-round);For some reason it lags when this isn't here.
            (send game-canvas set-show-menu! #f))]
       [in-focus-draw-proc
        (lambda (dc y-pos)
          (cond ((< (send game-canvas get-menu-col) 2)
                 (send game-canvas set-menu-col! 2))
                ((> (send game-canvas get-menu-col) 5)
                 (send game-canvas set-menu-col! 5)))
          (send dc set-font standard-font)
          (send dc set-text-foreground red)
          (send dc draw-text
                (number->string (send game-canvas get-menu-col)) 200
                (+ (*  (send game-canvas get-menu-row) 40) 100))
          (send dc draw-text "New game! Number of players?" 240 y-pos))]
       [draw-proc
        (lambda (dc y-pos)
          (send dc set-font standard-font)
          (send dc set-text-foreground white)
          (send dc draw-text "New game! Number of players?" 240 y-pos))]))

;A menu item that lets you start a new round
(define new-round
  (new menu-item%
       [parent game-canvas]
       [callback
        (lambda (arg)
          (send gamestate1 new-round)
          (send game-canvas set-show-menu! #f))]
       [in-focus-draw-proc
        (lambda (dc y-pos)   
          (send dc set-font standard-font)
          (send dc set-text-foreground red)
          (send dc draw-text "->" 200 y-pos)
          (send dc draw-text "New round!" 240 y-pos))]
       [draw-proc
        (lambda (dc y-pos)
          (send dc set-font standard-font)
          (send dc set-text-foreground white)
          (send dc draw-text "New round!" 240 y-pos))]))

(send game-canvas set-menu-items! (list new-round number-of-players))


;Updates the game
(define (*render-fn*)
  (send game-canvas refresh-now))

;Sets the framerate and calls render-fn that updates the game and draws to the screen
(define game-clock
  (new timer%
       [notify-callback *render-fn*]
       [just-once? #f]))
(send game-clock start 12 #f)
(send game-canvas focus)