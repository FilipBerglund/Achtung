#lang racket/gui
(require "Abstractions.rkt")
(require "powerup.rkt")
(require "gamestate.rkt")

(define gamestate1 (new gamestate%))

(provide draw-playingfield-frame
         haze-bitmap gamestate1
         game-frame)

(define haze-bitmap (make-object bitmap% frame-width frame-height #f 0.5))
(define haze-dc (new bitmap-dc% [bitmap haze-bitmap]))
(send haze-dc set-alpha 0.25)
(send haze-dc set-brush gray 'solid )
(send haze-dc draw-rectangle 0 0 frame-width frame-height)

;(define gamestate1 (new gamestate%))

;;The main window
(define game-frame
  (new frame%
       (label "Achtung, die Kurve!")
       (height frame-height)
       (width frame-width)))
(send game-frame show #t)


