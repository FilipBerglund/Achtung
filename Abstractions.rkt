#lang racket/gui
(provide (all-defined-out))
;;ABSTRACTIONS

;Converts float to int
(define (float->int x)
  (inexact->exact (round x)))


;Checks if a color is white
(define (white? color)
  (not
   (or (not (equal? (send color red) 255))
       (not (equal? (send color green) 255))
       (not (equal? (send color blue) 255)))))

(define red (make-object color% 255 0 0))
(define blue (make-object color% 0 0 255))
(define green (make-object color% 0 255 0))
(define yellow (make-object color% 255 255 0))
(define black (make-object color% 0 0 0))
(define color-list
  (list red blue green yellow black))

(define input-keys (list #\q #\w #\p #\å #\n #\m #\x #\c #\t #\y))

