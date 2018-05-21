#lang racket/gui
(require "keyhandler.rkt")
(require "Abstractions.rkt")
(provide special-canvas%)
(define special-canvas%
  (class canvas%
    (init-field [state 1]
                [show-menu #t]
                [menu-item-list (list )]
                [menu-row 0]
                [menu-col 0])
    
    (define/override (on-char event)
      
      (define key (send event get-key-code))
      (define release (send event get-key-release-code))
      (when (eq? release 'press)
        (key-down! key))
      (when (eq? key 'release)
        (key-up! release))
      
      (when (equal? key 'escape)
        (set! show-menu #t))
      
      (when (equal? key #\space)
        (set! show-menu (not show-menu)))
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
    
    (define/public (draw-menu dc)
      (let ((y-pos 420))
        (map (lambda (x)
               (send x draw-menu-item dc y-pos)
               (set! y-pos (+ y-pos 40)))
             menu-item-list)))

    (define/public (in-focus? a-menu-item)
      (eq? (list-ref menu-item-list menu-row) a-menu-item))
    (define/public (set-menu-items! lst)
      (set! menu-item-list lst))
    (define/public (show-menu?)
      show-menu)
    (define/public (set-show-menu! x)
      (set! show-menu x))
    (define/public (set-menu-row-to-0)
      (set! menu-row 0))
    (define/public (get-menu-row)
      menu-row)
    (define/public (get-menu-col)
      menu-col)
    (define/public (set-menu-row! x)
      (set! menu-row x))
    (define/public (set-menu-col! x)
      (set! menu-col x))
    (super-new)))