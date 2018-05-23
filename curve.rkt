#lang racket/gui
(require "keyhandler.rkt")
(require "Abstractions.rkt")
(provide curve%)
(define curve%
  (class object%
    (init-field
     name;name of the curve. Used to display the winner at the end of the game.
     left;The keyboard keys used to control the curve.
     right
     curve_color
     [collision-off #f]
     [hole #f]
     [hole-counter -80];Makes the curves start as a hole,
     ;this way you avoid collitions before you have time to do anything.
     [curve-size 7]
     [collision-color1 (new color%)];Variables that are used to check for collisions.
     [collision-color2 (new color%)]
     [collision-color3 (new color%)]
     ;When initiated becomes the staring pos then it's the current pos.
     [x-pos (random 200 (- frame-width 450))]
     [y-pos (random 200 (- frame-height 200))]
     [x-vel 0]
     [y-vel 0]
     [speed 3];Speed is not the same as velocity so this is not superfluous
     [dead #f]
     [angle (random 0 1000)]
     [score 0]
     ;The curve keeps track of where it is and has been.
     ;These are also used to draw to the canvas through the canvas dc.
     [curve-bitmap (make-object bitmap% (- frame-width 250) (- frame-height 10) #f 0.5)]
     [curve-dc (new bitmap-dc% [bitmap curve-bitmap])]
     [superpowerup-bitmap (make-object bitmap% (- frame-width 250) (- frame-height 10) #f 0.5)]
     [superpowerup-dc (new bitmap-dc% [bitmap superpowerup-bitmap])]
     ;The active bitmap.
     [current-bitmap curve-bitmap]
     [current-dc curve-dc]
     [current-bitmap-level 'level-1])

    ;The superpowerup is trasparent so that the player knows which one they are on.
    ;For some reason you can't do this in the initination of the object.
    (send superpowerup-dc set-alpha 0.2)
    
    ;So that other curves can check where others have been.
    ;This funtion is also defined in all subclasses of powerup%.
    ;Takes a curve% or powerup% and returns it's bitmap. In the case of curves
    ;it returns the bitmap that is on bitmap-level.
    (define/public (get-bitmap-dc bitmap-level)
      (if (equal? bitmap-level 'level-1)
          curve-dc
          superpowerup-dc))

    ;This is called everytime the curve is drawn, after a random number of
    ;frames the curve stops drawing itself for a set number of frames. Returns a
    ;boolean.
    (define hole?
      (let ((length 20))
        (lambda ()
          ;The boolean hole can force the function to return #t.
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

    ;Draws the bitmaps of the curve to dc.
    (define/public (draw-curve dc)
      (cond (dead (send dc draw-bitmap curve-bitmap 0 0 'solid)
                  (send dc draw-bitmap superpowerup-bitmap 0 0 'solid)
                  (send dc set-pen curve_color curve-size 'solid)
                  (send dc draw-ellipse x-pos y-pos 2 2))
            ((hole?)
             (send dc set-pen curve_color curve-size 'solid)
             (send dc draw-ellipse x-pos y-pos 2 2)
             (send dc draw-bitmap curve-bitmap 0 0 'solid)
             (send dc draw-bitmap superpowerup-bitmap 0 0 'solid)
             ;;Without this it draws the above on the curve, this undoes that.
             (set! collision-off #t));You can't collide with other things when you
            ;are a hole.
            (else
             ;The pen needs to change when the size powerup is activated.
             (send curve-dc set-pen curve_color curve-size 'solid);Change!
             (send superpowerup-dc set-pen curve_color curve-size 'solid);Change!
             (send current-dc draw-line x-pos y-pos (+ x-pos x-vel) (+ y-pos y-vel))
             (send dc draw-bitmap curve-bitmap 0 0 'solid)
             (send dc draw-bitmap superpowerup-bitmap 0 0 'solid)
             (set! collision-off #f))))
    
    (define/public (get-dead)
      dead)

    ;This allows for the user to control the curve by changing its direction through
    ;changing the angle of the velocity.
    (define/public (update-vel)
      (cond ((key-down? left) (set! angle (- angle (* 0.02 speed)))) ;For each frame
            ;a key is pressed the angle of the velocity changes.
            ((key-down? right) (set! angle (+ angle (* 0.02 speed))))) ;The speed
      ;multiplier makes the turn radius about the same regardless of the speed.
      (set! x-vel (* speed (cos angle))) ;Makes the speed constant regardless of the
      ;direction.
      (set! y-vel (* speed (sin angle))))

    ;Not pretty I know.
    (define/public (get-collision-on?)
      (not collision-off))

    ;This is what 
    (define/public (apply-on-hit-effect x)
      (when (send x get-collision-on?)
        (send x set-dead! #t)))
    
    ;Checks if the curve will collide with another-curve.
    ;It does this by looking at the other game objects curve-bitmap and if that is
    ;colored it runs that objects apply-on-hit-effect.
    (define/public (collision? another-object bitmap-level)
      ;You can only collide with curves on your bitmap level
      ;Gets the color of three pixels where this curve wants to move to.
      ;The curve-size fixes the issue of the collision getting triggered
      ;when the speed gets too low or not triggered when the speed is too high.
      (unless dead
        (send (send another-object get-bitmap-dc bitmap-level) get-pixel
              (float->int (+ x-pos (* (+ (/ curve-size 1.7) 2) (cos angle))))
              (float->int (+ y-pos (* (+ (/ curve-size 1.7) 2) (sin angle))))
              collision-color1)
        (send (send another-object get-bitmap-dc bitmap-level) get-pixel
              (float->int (+ x-pos (* (+ (/ curve-size 1.7) 1) (cos (+ angle (/ pi 3))))))
              (float->int (+ y-pos (* (+ (/ curve-size 1.7) 1) (sin (+ angle (/ pi 3))))))
              collision-color2)
        (send (send another-object get-bitmap-dc bitmap-level) get-pixel
              (float->int (+ x-pos (* (+ (/ curve-size 1.7) 1) (cos (- angle (/ pi 3))))))
              (float->int (+ y-pos (* (+ (/ curve-size 1.7) 1) (sin (- angle (/ pi 3))))))
              collision-color3)
        ;If it's a non white pixel it dies, unless the collision is off. Also, it can't
        ;move beyond the screen.
        (when (or (not (white? collision-color1))
                  (not (white? collision-color2))
                  (not (white? collision-color3)))
          (send another-object apply-on-hit-effect this))))

    (define/public (update-pos) ;;Updates position.
      ;Because the collisions detection is dependent on the curve-size this has to be
      ;so too. Regarless of collision status you die when you go beyond the screen boarders.
      (when (or (< (- frame-width 253) (+ (+ x-pos x-vel) (/ curve-size 2)))
                (> 13  (- (+ x-pos x-vel) (/ curve-size 2)))
                (< (- frame-height 13) (+ (+ y-pos y-vel) (/ curve-size 2)))
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
    (define/public (reset-score!)
      (set! score 0))
    (define/public (get-name) name)
    (define/public (get-bitmap-level)
      current-bitmap-level)
    (define/public (set-bitmap-level! level)
      (set! current-bitmap-level level))
    (define/public (set-current-bitmap&dc-default)
      (set! current-bitmap curve-bitmap)
      (set! current-dc curve-dc)
      (set! current-bitmap-level 'level-1))
    (define/public (set-current-bitmap&dc-superpowerup)
      (set! current-bitmap superpowerup-bitmap)
      (set! current-dc superpowerup-dc)
      (set! current-bitmap-level 'level-2))
    (define/public (combine-bitmaps)
      ;Doesn't get much uglier than this lol. I don't know how else to do it though.
      ;I can't seem to find a way to change the alpha of things that are already drawn
      ;to a bitmap. This also introduces a lag when this function is called at the end
      ;of the superpowerup powerup.
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
      [set! x-pos (random 200 (- frame-width 450))]
      [set! y-pos (random 200 (- frame-height 200))]
      [set! angle (random 0 1000)]
      [set! dead #f]
      [send curve-dc erase]
      [send superpowerup-dc erase]
      (hole?);Calling this funktion fixes the issue where the collision-powerup
      ;doesn't reset. This is because the powerup sets hole to #t and that in turn
      ;sets hole-counter to 20. If that happens after the curve is reseted the
      ;hole-counter is 20.
      [set! hole-counter -80])
    (super-new)))
