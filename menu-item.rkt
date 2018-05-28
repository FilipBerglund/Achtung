#lang racket/gui
;Written by: FILIP BERGLUND
;menu-item% is defined here. This class is part of a menu system.
;The other part is in special-canvas% (special-canvas.rkt).
;menu-item% is responsible for the text that is drawn when it's
;drawn to canvas, and for the function that's called when the
;menu-item is selected. special-canvas% is responsible for the
;positioning and spacing of the menu, and the operations on the menu.

;2018-05-23: Added y-pos as a variable to draw-menu-item so that
;special-canvas% is responsible for everything regarding the positioning
;of the menu-item%'s.

(require "settings.rkt")
(provide menu-item%)
(define menu-item%
  (class object%
    (init-field
     ;A special-canvas%
     parent
     draw-proc
     in-focus-draw-proc
     [callback (lambda()#f)])

    ;When called fulfills menu-item%'s purpose.
    ;IN: int OUT: what ever callback does.
    (define/public (activate arg)
      (callback arg))

    ;Draws the menu-item to dc. Different drawing functions
    ;depending on whether or not it's in focus.
    ;IN: a-dc int int
    (define/public (draw-menu-item dc y-pos x-pos)
      (cond ((send parent in-focus? this)
             (send dc set-text-foreground red)
             (send dc draw-text "->" x-pos y-pos)
             (in-focus-draw-proc dc y-pos x-pos))
            (else
             (send dc set-text-foreground white)
             (draw-proc dc y-pos x-pos))))
    (super-new)))