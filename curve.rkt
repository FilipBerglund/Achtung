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
     [collision-off #f]
     [hole #f]
     [curve_size 7]
     [collision-color1 (new color%)]
     [collision-color2 (new color%)]
     [collision-color3 (new color%)]
     [x-pos (random 200 600)] ;When initiated becomes the staring pos then it's the current pos.
     [y-pos (random 200 400)]
     [x-vel 0]
     [y-vel 0]
     [speed 3];Speed is not the same as velocity so this is not superfluous
     [dead #f]
     [angle (random 0 1000)]
     [score 0])


    ;The curve keeps track of where it is and has been. These are also used to draw to the canvas through the canvas dc.
    (define *curve-bitmap* (make-object bitmap% 800 600 #f 0.5))
    (define curve-dc (new bitmap-dc% [bitmap *curve-bitmap*]))
    ;(send curve-dc set-pen curve_color curve_size 'xor)

    ;So that other curves can check where others have been.
    (define/public (get-bitmap-dc)
      curve-dc)

    ;This is called everytime the curve is drawn, after a random number of frames the curve stops drawing itself for a set number of frames.
    (define hole?
      (let ((count 8)
            (length 20))
        (lambda ()
          (cond (hole (set! count 20) #t)
                ((< count 20)
                 (set! count (add1 count))
                 #t) ;;Defines the length of the hole
                ((< count length)
                 (set! count (add1 count))
                 #f)
                (else (set! length (random 80 160));;Defines the length between the holes.
                      (set! count 0))))))

    ;Draws the new part of the curve to the canvas.
    (define/public (draw-curve dc)
      (cond (dead (send dc draw-bitmap *curve-bitmap* 0 0 'solid)
                  (send dc set-pen curve_color curve_size 'solid))
            ((hole?)
             (send dc set-pen curve_color curve_size 'solid)
             (send dc draw-ellipse x-pos y-pos 2 2)
             (send dc draw-bitmap *curve-bitmap* 0 0 'solid) ;;Without this it draws the above on the curve, this undoes that.
             (set! collision-off #t));You can't collide with other things when you are a hole.
            (else
             (send curve-dc set-pen curve_color curve_size 'solid)
             (send curve-dc draw-line x-pos y-pos (+ x-pos x-vel) (+ y-pos y-vel))
             (send curve-dc set-pen black 1 'solid)
             ;             (send curve-dc draw-point
             ;                   (float->int (+ x-pos (* (+ (/ curve_size 1.7) 1) (cos (- angle (/ pi 3))))))
             ;                   (float->int (+ y-pos (* (+ (/ curve_size 1.7) 1) (sin (- angle (/ pi 3)))))))
             ;             (send curve-dc draw-point
             ;                   (float->int (+ x-pos (* (+ (/ curve_size 1.7) 1) (cos (+ angle (/ pi 3))))))
             ;                   (float->int (+ y-pos (* (+ (/ curve_size 1.7) 1) (sin (+ angle (/ pi 3)))))))
             ;             (send curve-dc draw-point
             ;                   (float->int (+ x-pos (* (+ (/ curve_size 1.7) 1) (cos angle))))
             ;                   (float->int (+ y-pos (* (+ (/ curve_size 1.7) 1) (sin angle)))))
             (send dc draw-bitmap *curve-bitmap* 0 0 'solid)
             (set! collision-off #f))))
    
    (define/public (dead?)
      dead)

    ;This allows for the user to control the curve by changing its direction through changing the angle of the velocity.
    (define/public (update-vel)
      (cond ((key-down? left) (set! angle (- angle (* 0.017 speed)))) ;For each frame a key is pressed the angle of the velocity changes.
            ((key-down? right) (set! angle (+ angle (* 0.017 speed))))) ;The speed multiplier makes the turn radius about the same regardless of the speed.
      (set! x-vel (* speed (cos angle))) ;Makes the speed constant regardless of the direction.
      (set! y-vel (* speed (sin angle))))

    ;Not pretty I know.
    (define/public (get-collision-on?)
      (not collision-off))
    
    (define/public (apply-on-hit-effect x)
      (when (send x get-collision-on?)
        (send x set-dead! #t)))
    
    ;Checks if the curve will collide with another-curve. It does this by looking at the other curvs *curve-bitmap* and if that is colored.
    (define/public (collision? another-object)
      ;Gets the color of the pixel where this curve wants to move too. The curve_size fixes the issue of the collision getting triggered when the speed gets to low or not triggered when the speed is to high.
      (unless dead
        (send (send another-object get-bitmap-dc) get-pixel
              (float->int (+ x-pos (* (+ (/ curve_size 1.7) 2) (cos angle))))
              (float->int (+ y-pos (* (+ (/ curve_size 1.7) 2) (sin angle)))) collision-color1)
        (send (send another-object get-bitmap-dc) get-pixel
              (float->int (+ x-pos (* (+ (/ curve_size 1.7) 1) (cos (+ angle (/ pi 3))))))
              (float->int (+ y-pos (* (+ (/ curve_size 1.7) 1) (sin (+ angle (/ pi 3)))))) collision-color2)
        (send (send another-object get-bitmap-dc) get-pixel
              (float->int (+ x-pos (* (+ (/ curve_size 1.7) 1) (cos (- angle (/ pi 3))))))
              (float->int (+ y-pos (* (+ (/ curve_size 1.7) 1) (sin (- angle (/ pi 3)))))) collision-color3)
        ;If it's a non white pixel it dies, unless the collision is off. Also, it can't move beyond the screen.
        (when (or (not (white? collision-color1))
                  (not (white? collision-color2))
                  (not (white? collision-color3)))
          (send another-object apply-on-hit-effect this))))

    (define/public (update-pos) ;;Updates position.
      (when (or (< 790 (+ x-pos x-vel))
                (> 20 (+ x-pos x-vel));Because the collisions detection is dependent on the curve_size this has to be so too.
                (< 590 (+ y-pos y-vel))
                (> 20 (+ y-pos y-vel)))
        (set! dead #t))
      (unless dead
        (set! x-pos (+ x-pos x-vel))
        (set! y-pos (+ y-pos y-vel))))

    (define/public (set-speed! x)
      (set! speed x))

    (define/public (set-size! x)
      (set! curve_size x))
    (define/public (died?)
      (died))
    (define died
      (let ((prev #f))
        (lambda ()
          (cond ((equal? dead prev) #f)
                ((not dead) (set! prev dead) #f)
                (else (set! prev dead) #t)))))   
    
    (define/public (get-x-vel)
      x-vel)
    (define/public (get-y-vel)
      y-vel)
    (define/public (get-x-pos)
      x-pos)
    (define/public (get-y-pos)
      y-pos)
    (define/public (set-x-pos x)
      (set! x-pos x))
    (define/public (set-y-pos y)
      (set! y-pos y))
    (define/public (get-angle)
      angle)
    (define/public (get-size)
      curve_size)
    (define/public (get-color)
      curve_color)
    (define/public (clear-dc)
      (send curve-dc erase))
    (define/public (set-dead! x)
      (set! dead x))
    (define/public (set-collision! x)
      (set! collision-off x))
    (define/public (set-hole! x)
      (set! hole x))
    (define/public (addscore)
      (set! score (add1 score)))
    (define/public (get-score)
      score)
    (define/public (new-round)
      [set! x-pos (random 200 600)]
      [set! y-pos (random 200 400)]
      [set! angle (random 0 1000)]
      [set! dead #f]
      [send curve-dc erase])
    (super-new)))
