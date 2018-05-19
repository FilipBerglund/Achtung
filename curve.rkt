#lang racket/gui
(require "keyhandler.rkt")
(require "Abstractions.rkt")
;(define white (make-object color% 230 230 230))
(provide curve%)
(define curve%
  (class object%
    (init-field
     name
     left
     right
     curve_color
     [collision-off #f]
     [hole #f]
     [hole-counter -80];Makes the curves start as a hole,
     ;this way you avoid collitions before you have time to do anything.
     [curve-size 7]
     [collision-color1 (new color%)]
     [collision-color2 (new color%)]
     [collision-color3 (new color%)]
     ;When initiated becomes the staring pos then it's the current pos.
     [x-pos (random 200 600)]
     [y-pos (random 200 400)]
     [x-vel 0]
     [y-vel 0]
     [speed 3];Speed is not the same as velocity so this is not superfluous
     [dead #f]
     [angle (random 0 1000)]
     [score 0]
     ;The curve keeps track of where it is and has been.
     ;These are also used to draw to the canvas through the canvas dc.
     [curve-bitmap (make-object bitmap% 800 600 #f 1)]
     [curve-dc (new bitmap-dc% [bitmap curve-bitmap])]
     [superpowerup-bitmap (make-object bitmap% 800 600 #f 0.01)]
     [superpowerup-dc (new bitmap-dc% [bitmap superpowerup-bitmap])]
     [current-bitmap curve-bitmap]
     [current-dc curve-dc])
    
    (send superpowerup-dc set-alpha 0.2)
    ;So that other curves can check where others have been.
    (define/public get-bitmap-dc
      (lambda x current-dc))

    ;This is called everytime the curve is drawn, after a random number of
    ;frames the curve stops drawing itself for a set number of frames.
    (define hole?
      (let ((length 20))
        (lambda ()
          (cond (hole (set! hole-counter 20) #t)
                ((< hole-counter 10)
                 (set! hole-counter (add1 hole-counter))
                 #t) ;;Defines the length of the hole
                ((< hole-counter length)
                 (set! hole-counter (add1 hole-counter))
                 #f)
                (else (set! length (random 60 180));;Defines the length
                      ;between the holes.
                      (set! hole-counter 0))))))

    ;Draws the new part of the curve to the canvas.
    (define/public (draw-curve dc)
      (cond (dead (send dc draw-bitmap curve-bitmap 0 0 'solid)
                  (send dc draw-bitmap superpowerup-bitmap 0 0 'solid)
                  (send dc set-pen curve_color curve-size 'solid)
                  (send dc draw-ellipse x-pos y-pos 2 2))
            ((hole?)
             (send dc set-pen curve_color curve-size 'solid)
             (send curve-dc set-pen curve_color curve-size 'solid);Change!
             (send superpowerup-dc set-pen curve_color curve-size 'solid);Change!
             (send dc draw-ellipse x-pos y-pos 2 2)
             (send dc draw-bitmap curve-bitmap 0 0 'solid)
             (send dc draw-bitmap superpowerup-bitmap 0 0 'solid)
             ;;Without this it draws the above on the curve, this undoes that.
             (set! collision-off #t));You can't collide with other things when you are a hole.
            (else
             (send curve-dc set-pen curve_color curve-size 'solid);Change!
             (send superpowerup-dc set-pen curve_color curve-size 'solid);Change!
             (send current-dc draw-line x-pos y-pos (+ x-pos x-vel) (+ y-pos y-vel))
             ;(send curve-dc set-pen black 1 'solid)
             ;             (send curve-dc draw-point
             ;                   (float->int (+ x-pos (* (+ (/ curve-size 1.7) 1) (cos (- angle (/ pi 3))))))
             ;                   (float->int (+ y-pos (* (+ (/ curve-size 1.7) 1) (sin (- angle (/ pi 3)))))))
             ;             (send curve-dc draw-point
             ;                   (float->int (+ x-pos (* (+ (/ curve-size 1.7) 1) (cos (+ angle (/ pi 3))))))
             ;                   (float->int (+ y-pos (* (+ (/ curve-size 1.7) 1) (sin (+ angle (/ pi 3)))))))
             ;             (send curve-dc draw-point
             ;                   (float->int (+ x-pos (* (+ (/ curve-size 1.7) 1) (cos angle))))
             ;                   (float->int (+ y-pos (* (+ (/ curve-size 1.7) 1) (sin angle)))))
             (send dc draw-bitmap curve-bitmap 0 0 'solid)
             (send dc draw-bitmap superpowerup-bitmap 0 0 'solid)
             (set! collision-off #f))))
    
    (define/public (get-dead)
      dead)

    ;This allows for the user to control the curve by changing its direction through changing the angle of the velocity.
    (define/public (update-vel)
      (cond ((key-down? left) (set! angle (- angle (* 0.02 speed)))) ;For each frame a key is pressed the angle of the velocity changes.
            ((key-down? right) (set! angle (+ angle (* 0.02 speed))))) ;The speed multiplier makes the turn radius about the same regardless of the speed.
      (set! x-vel (* speed (cos angle))) ;Makes the speed constant regardless of the direction.
      (set! y-vel (* speed (sin angle))))

    ;Not pretty I know.
    (define/public (get-collision-on?)
      (not collision-off))
    
    (define/public (apply-on-hit-effect x)
      (when (send x get-collision-on?)
        (send x set-dead! #t)))
    
    ;Checks if the curve will collide with another-curve. It does this by looking at the other curvs curve-bitmap and if that is colored.
    (define/public (collision? another-object)
      ;Gets the color of the pixel where this curve wants to move too. The curve-size fixes the issue of the collision getting triggered when the speed gets to low or not triggered when the speed is to high.
      (unless dead
        (send (send another-object get-bitmap-dc) get-pixel
              (float->int (+ x-pos (* (+ (/ curve-size 1.7) 2) (cos angle))))
              (float->int (+ y-pos (* (+ (/ curve-size 1.7) 2) (sin angle)))) collision-color1)
        (send (send another-object get-bitmap-dc) get-pixel
              (float->int (+ x-pos (* (+ (/ curve-size 1.7) 1) (cos (+ angle (/ pi 3))))))
              (float->int (+ y-pos (* (+ (/ curve-size 1.7) 1) (sin (+ angle (/ pi 3)))))) collision-color2)
        (send (send another-object get-bitmap-dc) get-pixel
              (float->int (+ x-pos (* (+ (/ curve-size 1.7) 1) (cos (- angle (/ pi 3))))))
              (float->int (+ y-pos (* (+ (/ curve-size 1.7) 1) (sin (- angle (/ pi 3)))))) collision-color3)
        ;If it's a non white pixel it dies, unless the collision is off. Also, it can't move beyond the screen.
        (when (or (not (white? collision-color1))
                  (not (white? collision-color2))
                  (not (white? collision-color3)))
          (send another-object apply-on-hit-effect this))))

    (define/public (update-pos) ;;Updates position.
      ;Because the collisions detection is dependent on the curve-size this has to be so too.
      (when (or (< 797 (+ (+ x-pos x-vel) (/ curve-size 2)))
                (> 13  (- (+ x-pos x-vel) (/ curve-size 2)))
                (< 597 (+ (+ y-pos y-vel) (/ curve-size 2)))
                (> 13  (- (+ y-pos y-vel) (/ curve-size 2))))
        (set! dead #t))
      (unless dead
        (set! x-pos (+ x-pos x-vel))
        (set! y-pos (+ y-pos y-vel))))

    (define/public (set-speed! x)
      (set! speed x))
    (define/public (set-size! x)
      (set! curve-size x))
    (define/public (died?)
      (died))
    (define died
      (let ((prev #f))
        (lambda ()
          (cond ((equal? dead prev) #f)
                ((not dead) (set! prev dead) #f)
                (else (set! prev dead) #t)))))   
    
    (define/public (get-x-vel) x-vel)
    (define/public (get-y-vel) y-vel)
    (define/public (get-x-pos) x-pos)
    (define/public (get-y-pos) y-pos)
    
    (define/public (set-x-pos x) (set! x-pos x))
    (define/public (set-y-pos y) (set! y-pos y))
    
    (define/public (get-angle) angle)
    (define/public (get-size) curve-size)
    (define/public (get-color) curve_color)
    
    (define/public (set-dead! x) (set! dead x))
    (define/public (set-collision! x) (set! collision-off x))
    (define/public (set-hole! x) (set! hole x))
    (define/public (addscore) (set! score (add1 score)))
    (define/public (get-score) score)
    (define/public (get-name) name)
    (define/public (set-current-bitmap&dc-default)
      (set! current-bitmap curve-bitmap)
      (set! current-dc curve-dc))
    (define/public (set-current-bitmap&dc-superpowerup)
      (set! current-bitmap superpowerup-bitmap)
      (set! current-dc superpowerup-dc))
    (define/public (combine-bitmaps)
      ;Doesn't get much uglier than this lol. I don't know how else to do it though.
      (send curve-dc draw-bitmap superpowerup-bitmap 0 0 'solid)
      (send curve-dc draw-bitmap superpowerup-bitmap 0 0 'solid)
      (send curve-dc draw-bitmap superpowerup-bitmap 0 0 'solid)
      (send curve-dc draw-bitmap superpowerup-bitmap 0 0 'solid)
      (send curve-dc draw-bitmap superpowerup-bitmap 0 0 'solid)
      (send curve-dc draw-bitmap superpowerup-bitmap 0 0 'solid)
      (send curve-dc draw-bitmap superpowerup-bitmap 0 0 'solid))
    (define/public (erase-superpowerup-dc)
      (send superpowerup-dc erase))
    (define/public (erase-current-dc)
      (send current-dc erase))
    (define/public (new-round)
      [set! x-pos (random 200 600)]
      [set! y-pos (random 200 400)]
      [set! angle (random 0 1000)]
      [set! dead #f]
      [send curve-dc erase]
      [send superpowerup-dc erase]
      [set! hole-counter -80])
    (super-new)))
