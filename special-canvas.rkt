#lang racket/gui
;Writen by: FILIP BERGLUND, KRISTOFFER BRANDER.
;In this file is a special-canvas% that has support for a menu using
;menu-item% (menu-item.rkt). It also supports several keypresses by
;storing key-events in a hash in "keyhandler.rkt".

;From menu-item.rkt:
;"This class is part of a menu system.
;The other part is in special-canvas% (special-canvas.rkt).
;menu-item% is responsible for the text that is drawn when it's
;drawn to canvas, and for the function that's called when the
;menu-item is selected. special-canvas% is responsible for the
;positioning and spacing of the menu, and the operations on the menu."

;2018-05-23: Fixed so that the menu moves to where it should be when the
;frame-height and frame-width changes.

(require "keyhandler.rkt")
(require "settings.rkt")
(provide special-canvas%)
(define special-canvas%
  (class canvas%
    (init-field [show-menu #t]
                [menu-item-list (list )]
                [menu-row 0]
                [menu-col 0])
    
    (define/override (on-char event)
      ;With this (together with key-handler) you can press several keys at the same time.
      (define key (send event get-key-code))
      (define release (send event get-key-release-code))
      (when (eq? release 'press)
        (key-down! key))
      (when (eq? key 'release)
        (key-up! release))
      ;Escape shows the menu.
      (when (equal? key 'escape)
        (set! show-menu #t))
      ;Space toggels the menu.
      (when (equal? key #\space)
        (set! show-menu (not show-menu)))
      ;This controls the menu once it's shown. It can hover over menu-items
      ;(menu-item%) and call the one that's in focus' activate function with the
      ;current column as argument.
      (when show-menu
        (cond ((equal? key #\return)
               (send (list-ref menu-item-list menu-row) activate menu-col))
              ((equal? key 'up)
               (unless (equal? menu-row 0)
                 (set! menu-row (sub1 menu-row))))
              ((equal? key 'down)
               (unless (equal? menu-row (sub1 (length menu-item-list)))
                 (set! menu-row (add1 menu-row))))
              ((equal? key 'left)
               (set! menu-col (sub1 menu-col)))
              ((equal? key 'right)
               (set! menu-col (add1 menu-col))))))

    ;Draws menu-item-list to dc.
    ;IN: a-dc OUT: void.
    (define/public (draw-menu dc)
      (let ((y-pos (- frame-height 190))
            (x-pos (- frame-width 240)))
        (send dc set-font standard-font)
        (map (lambda (x)
               (send x draw-menu-item dc y-pos x-pos)
               (set! y-pos (+ y-pos 40)))
             menu-item-list)))

    ;A menu-item% can ask if it's in focus. menu-item% has different drawing procs
    ;depending on if it's in focus or not.
    ;IN: menu-item% OUT: bool
    (define/public (in-focus? a-menu-item)
      (eq? (list-ref menu-item-list menu-row) a-menu-item))

    ;Sets which menu-items are in the menu.
    ;IN: list of menu-item%
    (define/public (set-menu-items! lst)
      (set! menu-item-list lst))

    ;OUT: bool
    (define/public (show-menu?)
      show-menu)

    ;IN: bool
    (define/public (set-show-menu! x)
      (set! show-menu x))

    ;OUT: int
    (define/public (get-menu-row)
      menu-row)
    
    ;OUT: int
    (define/public (get-menu-col)
      menu-col)
    
    ;IN: int
    (define/public (set-menu-row! row)
      (set! menu-row row))
    
    ;IN: int
    (define/public (set-menu-col! col)
      (set! menu-col col))
    (super-new)))