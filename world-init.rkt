#lang racket/gui
(require "Abstractions.rkt")
(require "gamestate.rkt")

(provide draw-playingfield-frame
         haze-bitmap gamestate1
         game-frame)

(define haze-bitmap (make-object bitmap% 1050 610 #f 0.5))
(define haze-dc (new bitmap-dc% [bitmap haze-bitmap]))
(send haze-dc set-alpha 0.25)
(send haze-dc set-brush gray 'solid )
(send haze-dc draw-rectangle 0 0 1050 610)

(define (draw-playingfield-frame dc)
  (send dc set-pen yellow 6 'solid)
  (send dc draw-line 10 600 10 10)
  (send dc draw-line 10 10 800 10)
  (send dc draw-line 800 10 800 600)
  (send dc draw-line 10 600 800 600))

(define gamestate1 (new gamestate%))

;;The main window
(define game-frame
  (new frame%
       (label "Achtung, die Kurve!")
       (height 610)
       (width 1050)))
(send game-frame show #t)