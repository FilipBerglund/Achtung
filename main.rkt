#lang racket/gui
(require "special-canvas.rkt")
(require "Abstractions.rkt")
(require "menu-item.rkt")
(require "world-init.rkt")

(define (drawing-proc canvas dc)
  (let ((show-menu (send canvas show-menu?)))
    (cond (show-menu
           (send gamestate1 display-score dc)
           (send gamestate1 draw-curves dc)
           (draw-playingfield-frame dc)
           ;Adds haze when the menu is active, looks nice.
           (send dc draw-bitmap haze-bitmap 0 0 'solid)
           ;Displays endgame screen.
           (send gamestate1 end-round/game? dc)
           (send game-canvas draw-menu dc))
          (else
           (draw-playingfield-frame dc)
           (send gamestate1 update-velocities)
           (send gamestate1 draw-powerups dc)
           (send gamestate1 draw-curves dc)
           (send gamestate1 update-positions)
           (send gamestate1 check-collisions)
           (send gamestate1 display-score dc)
           (send gamestate1 check-powerups)
           ;Changes to the menu screen and sets the menu selector to new round if
           ;the round is over but not the game. If the game is over the selector
           ;is over them menu item "new game".
           (cond ((and (send gamestate1 game-over?) (send gamestate1 round-over?))
                  (send game-canvas set-show-menu! #t)
                  (send game-canvas set-menu-row! 0))
                 ((send gamestate1 round-over?)
                  (send game-canvas set-show-menu! #t)
                  (send game-canvas set-menu-row! 1)))
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
          (send gamestate1 set-number-of-players arg))]
       [in-focus-draw-proc
        (lambda (dc y-pos x-pos)
          ;Only 2,3,4 or 5 players are allowed.
          (cond ((< (send game-canvas get-menu-col) 2)
                 (send game-canvas set-menu-col! 2))
                ((> (send game-canvas get-menu-col) 5)
                 (send game-canvas set-menu-col! 5)))
          (send dc draw-text
                (string-join
                 (list "Number of players:"
                       (number->string (send game-canvas get-menu-col))))
                (+ x-pos 25) y-pos))]
       [draw-proc
        ;If not in focus this function runs and displays the selected setting.
        (lambda (dc y-pos x-pos)
          (send dc draw-text
                (string-join
                 (list "Number of players:"
                       (number->string (send gamestate1 get-number-of-players))))
                (+ x-pos 25) y-pos))]))

;Menu item that lets you select if you want the superpowerup to be active.
(define superpowerup-toggle
  (new menu-item%
       [parent game-canvas]
       [callback
        ;On if arg is 1, off if arg is 0.
        (lambda (arg)
          (send gamestate1 set-superpowerup-on arg))]
       [in-focus-draw-proc
        (lambda (dc y-pos x-pos)
          ;You only have two options.
          (cond ((< (send game-canvas get-menu-col) 0)
                 (send game-canvas set-menu-col! 0))
                ((> (send game-canvas get-menu-col) 1)
                 (send game-canvas set-menu-col! 1)))
          (send dc draw-text
                (string-join
                 (list "Superpowerup:"
                       (if (equal? (send game-canvas get-menu-col) 0)
                           "off" "on"))) (+ x-pos 25) y-pos))]
       [draw-proc
        ;If not in focus this function runs and displays the selected setting.
        (lambda (dc y-pos x-pos)
          (send dc draw-text
                (string-join
                 (list "Superpowerup:"
                       (if (send gamestate1 superpowerup-on?)
                           "on" "off")))
                (+ x-pos 25) y-pos))]))

(define new-game
  (new menu-item%
       [parent game-canvas]
       [callback
        (lambda (arg)
          (send gamestate1 make-curves)
          (send gamestate1 new-round);For some reason it lags when this isn't here.
          (send game-canvas set-show-menu! #f))]
       [in-focus-draw-proc
        (lambda (dc y-pos x-pos)
          (send dc draw-text "Start new game!" (+ x-pos 25) y-pos))]
       [draw-proc
        (lambda (dc y-pos x-pos)
          (send dc draw-text "Start new game!" (+ x-pos 25) y-pos))]))

;A menu item that lets you start a new round
(define new-round
  (new menu-item%
       [parent game-canvas]
       [callback
        (lambda (arg)
          (send gamestate1 new-round)
          (send game-canvas set-show-menu! #f))]
       [in-focus-draw-proc
        (lambda (dc y-pos x-pos)
          (send dc draw-text "New round!" (+ x-pos 25) y-pos))]
       [draw-proc
        (lambda (dc y-pos x-pos)
          (send dc draw-text "New round!" (+ x-pos 25) y-pos))]))

(send game-canvas set-menu-items!
      (list new-game new-round number-of-players superpowerup-toggle))

;Updates the game
(define (render-fn)
  (send game-canvas refresh-now))

;Sets the framerate and calls render-fn that updates the game and draws to the screen
(define game-clock
  (new timer%
       [notify-callback render-fn]
       [just-once? #f]))

(send game-clock start 12 #f)
(send game-canvas focus)