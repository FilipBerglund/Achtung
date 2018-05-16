#lang racket/gui
(provide (all-defined-out))
;;ABSTRACTIONS

;Converts float to int
(define (float->int x)
  (inexact->exact (round x)))

(define a-font (make-font #:size 29)) 

;Checks if a color is white
(define (white? color)
  (not
   (or (not (equal? (send color red) 255))
       (not (equal? (send color green) 255))
       (not (equal? (send color blue) 255)))))

(define red (make-object color% 253 24 0))
(define blue (make-object color% 0 158 231))
(define green (make-object color% 0 219 12))
(define yellow (make-object color% 219 232 4))
(define orange (make-object color% 250 115 7))
(define black (make-object color% 0 0 0))
(define purple (make-object color% 235 61 184))
(define white (make-object color% 255 255 255))
(define name-list
  (list "Red" "Yellow" "Orange" "Green" "Purple"))
(define color-list
  (list red yellow orange green purple))

(define input-keys (list #\q #\w #\p #\å #\n #\m #\x #\c #\t #\y))

(define scoreboard-positions
  (list 40 80 120 160 200))