#lang racket/gui
(provide menu-item%)
(define menu-item%
  (class object%
    (init-field
     draw-proc
     in-focus-draw-proc
     parent;A special-canvas%
     [callback (lambda()#f)])
    
    (define/public (activate arg)
      (callback arg))
    
    (define/public (draw-menu-item dc y-pos)
      (if (send parent in-focus? this)
          (in-focus-draw-proc dc y-pos)
          (draw-proc dc y-pos)))
    
    (super-new)))