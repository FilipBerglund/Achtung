#lang racket/gui
(provide menu-item%)
(define menu-item%
  (class object%
    (init-field
     draw-proc
     [bitmap (make-object bitmap% 710 1050 #f 0.5)]
     [bitmap-dc (new bitmap-dc% [bitmap bitmap])]
     [callback (lambda()#f)])
    
    (define/public (activate arg)
      (callback arg))
    
    (define/public (draw-menu-item dc y-pos)
      (draw-proc dc y-pos))
    
    (super-new)))