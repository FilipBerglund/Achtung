#lang racket/gui
(require "Abstractions.rkt")
(provide size-powerup% speed-powerup% clear-powerup% collision-powerup% super-powerup%)
(define powerup%
  (class object%
    (init-field color
                [x-pos (random 100 700)];Where the powerup is drawn
                [y-pos (random 100 (- frame-height 100))]
                [spawn-countdown (random 1000 1300)]
                [effect-duration 300]
                [rarity 1000]
                [spawn-duration 800]
                [tmp-duration 800];How long the 
                [affected-curve #f]
                [powerup-bitmap (make-object bitmap% (- frame-width 200) (+ frame-height 40) #f 0.5)]
                [powerup-dc (new bitmap-dc% [bitmap powerup-bitmap])])

    ;Resets the powerups when a new round starts. Removes the effect from the curves
    ;and removes the powerups from canvas.
    (define/public (new-round)
      (set! tmp-duration 1)
      (send powerup-dc erase)
      (set! spawn-countdown 0))

    ;Curves can collide with powerups regardless of which level they are on.
    (define/public (get-bitmap-dc bitmap-level)
      powerup-dc)

    ;Resets some variables to default.
    (define/public (reset-powerup)
      (send powerup-dc erase)
      (set! x-pos (random 100 (- frame-width 350)))
      (set! y-pos (random 100 (- frame-height 100))))

    ;Draws the powerup to dc.
    (define/public (draw-powerup dc)
      (send powerup-dc set-pen color 4 'solid)
      (send powerup-dc draw-rectangle (- x-pos 10) (- y-pos 10) 20 20)
      (send dc draw-bitmap-section powerup-bitmap
            (- x-pos 12) (- y-pos 12)
            (- x-pos 12) (- y-pos 12) 30 30))

    ;Controls the spawn of the powerup.
    ;Spawn-countdown is the entire cycle of the powerup, this is what
    ;changes during the life of the powerup.
    ;Spawn-duration is how long the powerup is spawned if no one takes it.
    (define/public (update dc)
      (cond ((equal? spawn-countdown 0)
             (reset-powerup)
             (set! spawn-countdown (random rarity (+ rarity 300))))
            ((< spawn-countdown spawn-duration)
             (send this draw-powerup dc)
             (set! spawn-countdown (sub1 spawn-countdown)))
            (else
             (set! spawn-countdown (sub1 spawn-countdown)))))
    
    (define/public (apply-on-hit-effect curve)
      (set! tmp-duration effect-duration)
      (set! affected-curve curve)
      (reset-powerup)
      (set! spawn-countdown (random rarity (+ rarity 300))))    
    (super-new)))

;Changes the size of the curve that takes it.
(define size-powerup%
  (class powerup%
    (inherit-field effect-duration
                   tmp-duration
                   affected-curve
                   powerup-dc
                   x-pos
                   y-pos
                   spawn-countdown
                   rarity)
    
    (define (effect-loop)
      ;tmp-duration is the temporary variable that keeps track of how long
      ;a curve has been affected with a powerup.
      (set! tmp-duration (- tmp-duration 1))
      (when (equal? tmp-duration 0)
        ;It has to move forward a bit because otherwise it collides with itself
        ;when the size is set to default.
        (send affected-curve set-x-pos (+ (send affected-curve get-x-pos)
                                          (* (/ (send affected-curve get-size) 2) (cos (send affected-curve get-angle)))))
        (send affected-curve set-y-pos (+ (send affected-curve get-y-pos)
                                          (* (/ (send affected-curve get-size) 2) (sin (send affected-curve get-angle)))))
        (send affected-curve set-size! 7)))

    ;This runs the effect-loop
    (define powerup-clock
      (new timer%
           [just-once? #f]
           [notify-callback effect-loop]))
    
    (define/override (apply-on-hit-effect curve)
      (super apply-on-hit-effect curve)
      (send powerup-clock start 10 #f)
      (send affected-curve set-size! 20))
    (super-new)))

;Changes the speed of the curve that takes it.
(define speed-powerup%
  (class powerup%
    (inherit-field effect-duration
                   tmp-duration
                   affected-curve
                   powerup-dc
                   x-pos
                   y-pos
                   spawn-countdown
                   rarity)
    
    (define (effect-loop)
      (set! tmp-duration (sub1 tmp-duration))
      (when (equal? tmp-duration 0)
        (send affected-curve set-speed! 3)))
    
    (define powerup-clock
      (new timer%
           [just-once? #f]
           [notify-callback effect-loop]))
    
    (define/override (apply-on-hit-effect curve)
      (send powerup-clock start 10 #f)
      (super apply-on-hit-effect curve)
      (send curve set-speed! 4.5))
    (super-new)))

;Clears the current bipmap of the curve that takes it.
;No reset function needed.
(define clear-powerup%
  (class powerup%
    (inherit-field effect-duration
                   affected-curve
                   powerup-dc
                   x-pos
                   y-pos
                   spawn-countdown
                   rarity)
    
    (define/override (apply-on-hit-effect curve)
      (super apply-on-hit-effect curve)
      (set! affected-curve curve)
      (send affected-curve erase-current-dc))
    (super-new)))

;Stops the curve that takes it from drawing the curve, and sets collision to #f.
;This makes the collisions act differently. Some objects apply-on-hit-effect take
;collision into consideration and others don't.
(define collision-powerup%
  (class powerup%
    (inherit-field effect-duration
                   tmp-duration
                   affected-curve
                   powerup-dc
                   x-pos
                   y-pos
                   spawn-countdown
                   rarity)
    
    (define (effect-loop)
      (set! tmp-duration (sub1 tmp-duration))
      (when (equal? tmp-duration 0)
        (send affected-curve set-hole! #f)))
    
    (define powerup-clock
      (new timer%
           [just-once? #f]
           [notify-callback effect-loop]))
    
    (define/override (apply-on-hit-effect curve)
      (super apply-on-hit-effect curve)
      (send powerup-clock start 10 #f)
      (send affected-curve set-hole! #t))
    (super-new)))

;The superpowerup sends all curves that take it to another bitmap, "behind" the old one.
;They can only collide with other curves on that new bitmap. You can return to the old
;bitmap in one of two ways. Either by waiting till the powerup runs out, or by taking
;it again on another plays on the canvas (this will be clearer later after you've read
;draw-powerup and apply-on-hit-effect).
(define super-powerup%
  (class powerup%
    (inherit-field effect-duration
                   affected-curve
                   tmp-duration
                   powerup-dc
                   color
                   x-pos
                   y-pos
                   powerup-bitmap
                   spawn-duration
                   spawn-countdown
                   rarity)
    (init-field [affected-curves (list )];More than one curve can take this powerup.
                [x-pos2 (- frame-width 440)];The powerup is displayed as two circles, these are the
                ;coordinates for the second one.
                [y-pos2 (- (/ frame-height 2) 60)])
    
    (define/override (reset-powerup)
      (send powerup-dc erase))
    
    (define/override (update dc)
      (cond ((equal? spawn-countdown 0)
             (reset-powerup)
             (set! spawn-countdown (random rarity (+ rarity 300))))
            ((< spawn-countdown spawn-duration)
             (send this draw-powerup dc)
             ;Now the powerup is on as long as the powerup is displayed on canvas.
             ;This way the powerup ends at the same time for all curves.
             (set! tmp-duration spawn-countdown)
             (set! spawn-countdown (sub1 spawn-countdown)))
            (else
             (set! spawn-countdown (sub1 spawn-countdown)))))

    (define/override (draw-powerup dc)
      (send dc set-pen white 5 'solid)
      ;A countdown for how long the powerup is active is displayed to the right of
      ;the playing field.
      (send dc draw-line
            (- frame-width 240) (* (- frame-height 10) (/ (- effect-duration spawn-countdown) effect-duration))
            (- frame-width 240) (- frame-height 10))
      (send powerup-dc set-pen color 4 'xor)
      (send powerup-dc set-brush black 'transparent)
      (send powerup-dc set-text-foreground white)
      ;Draws two circles at opposite sides of the playing field.
      (send powerup-dc draw-ellipse x-pos y-pos 120 120)
      (send powerup-dc draw-ellipse x-pos2 y-pos2 120 120)
      (send powerup-dc draw-text "DOWN" (+ x-pos 33) (+ y-pos 51));Taking the left
      ;one sends a curve to the "back" or, if you will, "down" the hole.
      (send powerup-dc draw-text "UP" (+ x-pos2 48) (+ y-pos2 51));The one to the right
      ;sends you to the "front" by going "up" the hole. 
      (send dc draw-bitmap powerup-bitmap 0 0 'solid))
    
    (define (effect-loop)
      (set! tmp-duration (- tmp-duration 1))
      (when (equal? tmp-duration 0)
        (send powerup-dc erase)
        (set! spawn-countdown (random rarity (+ rarity 300)))
        (map (lambda (x) (send x combine-bitmaps) (send x erase-superpowerup-dc))
             affected-curves);Combines the "back" and "front" bitmap so that all
        ;that's been drawn is now on the default "front" bitmap.
        (map (lambda (x) (send x set-current-bitmap&dc-default))
             affected-curves);Sends all curves that took the powerup to the "front".
        ))
    
    (define powerup-clock
      (new timer%
           [just-once? #f]
           [notify-callback effect-loop]))
    
    (define/override (apply-on-hit-effect curve)
      (send powerup-clock start 10 #f)
      (set! affected-curves (cons curve affected-curves))
      ;For the left circle the x-pos is less than 300. This might have been done in
      ;a fancier way but this is super simple and effective.
      (if (< (send curve get-x-pos) (/ (- frame-width 250) 2))
          (send curve set-current-bitmap&dc-superpowerup);Sends the curve to the "back".
          (send curve set-current-bitmap&dc-default);Sends the curve to the "back".
          ))
    (super-new)))
