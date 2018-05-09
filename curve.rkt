#lang racket/gui
(require "keyhandler.rkt")
(require "Abstractions.rkt")

(define white (make-object color% 230 230 230))
(provide curve%)
(define curve%
  (class object%
    (init-field
     left
     right
     curve_color
     [collition-off? #f]
     [curve_size 7]
     [collition-color (new color%)]
     [x-pos (random 200 600)] ;When initiated becomes the staring pos then it's the current pos.
     [y-pos (random 200 400)]
     [x-vel 0]
     [y-vel 0]
     [speed 7];Speed is not the same as velocity so this is not superfluous
     [dead #f]
     [angle (random 0 1000)]
     [score 0])


    ;The curve keeps track of where it is and has been. These are also used to draw to the canvas through the canvas dc.
    (define *curve-bitmap* (make-object bitmap% 800 600 #f 0.5))
    (define *curve-dc* (new bitmap-dc% [bitmap *curve-bitmap*]))
    (send *curve-dc* set-pen curve_color curve_size 'xor)

    ;So that other curves can check where others have been.
    (define/public (get-bitmap-dc)
      *curve-dc*)

    ;This is called everytime the curve is drawn, after a random number of frames the curve stops drawing itself for a set number of frames.
    (define hole?
      (let ((count 8)
            (length 9))
        (lambda ()
          (set! count (add1 count))
          (cond ((< count 7) #t) ;;Defines the length of the hole
                ((< count length) #f)
                (else (set! length (random 40 100));;Defines the length between the holes.
                      (set! count 0))))))
    
    ;Draws the new part of the curve to the canvas.
    (define/public (draw-curve dc)
      (unless dead
        (if (hole?)
            (begin
              (send dc set-pen white curve_size 'solid);In a hole the original curve isn't drawn but gray circles are. This makes it easier to see where you are going.
              (send dc draw-ellipse x-pos y-pos 2 2)
              (send dc draw-bitmap-section *curve-bitmap* (- x-pos 20) (- y-pos 20) (- x-pos 20) (- y-pos 20) 40 40 'solid) ;;Without this it draws the above on the curve, this undoes that.
              (set! collition-off? #t));You can't collide with other things when you are a hole.
            (begin 
              (send *curve-dc* draw-line x-pos y-pos (+ x-pos x-vel) (+ y-pos y-vel))
              (send dc set-pen curve_color curve_size 'solid)
              (send dc draw-bitmap-section *curve-bitmap* (- x-pos 20) (- y-pos 20) (- x-pos 20) (- y-pos 20) 40 40 'solid)
              (set! collition-off? #f))
            )))
    (define/public (dead?)
      dead)

    ;This allows for the user to control the curve by changing its direction through changing the angle of the velocity.
    (define/public (update-vel)
      (cond ((key-down? left) (set! angle (- angle (* 0.017 speed)))) ;For each frame a key is pressed the angle of the velocity changes.
            ((key-down? right) (set! angle (+ angle (* 0.017 speed))))) ;The speed multiplier makes the turn radius about the same regardless of the speed.
      (set! x-vel (* speed (cos angle))) ;Makes the speed constant regardless of the direction.
      (set! y-vel (* speed (sin angle))))

    ;Checks if the curve will collide with another-curve. It does this by looking at the other curvs *curve-bitmap* and if that is colored.
    (define/public (collition? another-curve)
      ;Gets the color of the pixel where this curve wants to move too. The curve_size fixes the issue of the collition getting triggered when the speed gets to low or not triggered when the speed is to high.
      (send (send another-curve get-bitmap-dc) get-pixel
            (float->int (+ x-pos (* (- curve_size 1) (cos angle))))
            (float->int (+ y-pos (* (- curve_size 1) (sin angle)))) collition-color)
      ;If it's a non white pixel it dies, unless the collition is off. Also, it can't move beyond the screen.
      (cond ((or (< 800 (+ x-pos x-vel))
                 (> curve_size (+ x-pos x-vel));Because the collitions detection is dependent on the curve_size this has to be so too.
                 (< 600 (+ y-pos y-vel))
                 (> curve_size (+ y-pos y-vel)))
             (set! dead #t))
            ((not (white? collition-color))
             (unless collition-off?
               (set! dead #t)))))

    (define/public (update-pos) ;;Updates position.
      (unless dead
        (set! x-pos (+ x-pos x-vel))
        (set! y-pos (+ y-pos y-vel))))

;    (define/public dieder
;      (let ((prev #f))
;        (lambda ()
;          (if (equal? #f prev)
;              4
;              (set! prev #t)))))
    
    (define/public (new-round)
      [set! x-pos (random 200 600)]
      [set! y-pos (random 200 400)]
      [set! angle (random 0 1000)]
      [set! dead #f]
      [send *curve-dc* erase])
    (super-new)))
