#lang racket/gui
;Written by: FILIP BERGLUND
;In this file are all general settings for the game. Except some for the the menu
;and the powerups. Also some abstractions to make the code a bit neater and some
;appearance settings.

;2018-05-24: Added default-size and default-speed.
;2018-05-24: Changed filename from Abstractions to settings.

(provide (all-defined-out))

;Converts float to int, is used when looking at a pixel
;from a bitmap, in curve% (curve.rkt).
;IN: float OUT: int
(define (float->int x)
  (inexact->exact (round x)))

;Is used for draw-menu, a method of special-canvas% (special-canvas.rkt).
(define standard-font (make-font #:family 'default))
;This font is used for draw-end-screen and display-score,
;methods of gamestate% (gamestate.rkt).
(define big-font (make-font #:size 40
                          #:family 'roman
                          #:style 'italic
                          #:hinting 'aligned)) 

;Checks if a color% is white.
;IN: color% OUT: bool
(define (white? color)
  (not
   (or (not (equal? (send color red) 255))
       (not (equal? (send color green) 255))
       (not (equal? (send color blue) 255)))))

(define gray (make-object color% 70 70 50))
(define light-gray (make-object color% 130 130 110))
(define actual-blue (make-object color% 0 0 255))
(define red (make-object color% 253 24 0))
(define blue (make-object color% 0 158 231))
(define green (make-object color% 0 219 12))
(define yellow (make-object color% 237 237 7))
(define orange (make-object color% 250 115 7))
(define black (make-object color% 0 0 0))
(define purple (make-object color% 235 61 184))
(define white (make-object color% 255 255 255))

;Name of the curves
(define name-list
  (list "Red" "Yellow" "Orange" "Green" "Purple"))

;Color of the curves, is matched with the names above.
(define color-list
  (list red yellow orange green purple))

;The default keys to control the curves. Q W for the first curve and so on.
(define default-keys-list
  (list #\q #\w #\o #\p #\n #\m #\t #\y #\x #\c))

;Default speed and size of the curves. These are set when the curves are created
;and when powerups reset they can set the size or speed to default.
(define default-speed 3)
(define default-size 7)

;It can work with smaller height but 600 is just enough
;to fit the menu under the scoreboard when playing 5 players.
(define frame-height 600)
;It works without the superpowerup on when width is as low as 651 but
;the superpowerup works badly. Recommended lowest is 700.
(define frame-width 1050)

;How often the game updates. (How often the game-loop in main.rkt runs).
(define frametime 12) 

;Draws a frame to dc around the playing field so that you know there you're
;allowed to go.
;IN: a-dc OUT: void.
(define (draw-playingfield-frame dc)
  (send dc set-pen light-gray 6 'solid)
  (send dc draw-line 10 (- frame-height 10) 10 10)
  (send dc draw-line 10 10 (- frame-width 250) 10)
  (send dc draw-line
        (- frame-width 250) 10
        (- frame-width 250) (- frame-height 10))
  (send dc draw-line
        10                  (- frame-height 10)
        (- frame-width 250) (- frame-height 10)))

;When drawn creates a haze effect for the things behind it.
;Is used when the menu is active to add haze to the background.
(define haze-bitmap (make-object bitmap% frame-width frame-height #f 0.5))
(define haze-dc (new bitmap-dc% [bitmap haze-bitmap]))
(send haze-dc set-alpha 0.25)
(send haze-dc set-brush gray 'solid )
(send haze-dc draw-rectangle 0 0 frame-width frame-height)