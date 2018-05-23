#lang racket/gui
(provide (all-defined-out))
;;ABSTRACTIONS

;Converts float to int
(define (float->int x)
  (inexact->exact (round x)))

(define standard-font (make-font #:family 'default))
(define a-font (make-font #:size 40
                          #:family 'roman
                          #:style 'italic
                          #:hinting 'aligned)) 

;Checks if a color% is white, returns a boolean.
(define (white? color)
  (not
   (or (not (equal? (send color red) 255))
       (not (equal? (send color green) 255))
       (not (equal? (send color blue) 255)))))
(define gray (make-object color% 70 70 50))
(define actual-blue (make-object color% 0 0 255))
(define red (make-object color% 253 24 0))
(define blue (make-object color% 0 158 231))
(define green (make-object color% 0 219 12))
(define yellow (make-object color% 237 237 7))
(define orange (make-object color% 250 115 7))
(define black (make-object color% 0 0 0))
(define purple (make-object color% 235 61 184))
(define white (make-object color% 255 255 255))

(define name-list
  (list "Red" "Yellow" "Orange" "Green" "Purple"))

(define color-list
  (list red yellow orange green purple))

(define (draw-playingfield-frame dc)
  (send dc set-pen yellow 6 'solid)
  (send dc draw-line 10 (- frame-height 10) 10 10)
  (send dc draw-line 10 10 (- frame-width 250) 10)
  (send dc draw-line
        (- frame-width 250) 10
        (- frame-width 250) (- frame-height 10))
  (send dc draw-line
        10                  (- frame-height 10)
        (- frame-width 250) (- frame-height 10)))

;It can work with smaler height but 600 is just enough
;to fit the menu under the scoreboard when playing 5 players.
(define frame-height 600)
;It works without the superpowerup on when width is as low as 651 but
;the superpowerup works badly. Recomended lowest is 700.
(define frame-width 1000)