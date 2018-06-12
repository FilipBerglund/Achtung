#lang racket/gui
;Writen by: FILIP BERGLUND
;In this file powerup% and it's subclasses are defined. These
;change (often temporarily) some variables in a curve% object
;when a curve collides with a powerup.

;2018-05-24: Changed the name of spawn-countdown, rarity,
;tmp-duration to life-cycle-countdown, variance, effect-duration-countdown respectively.
;Also moved variables from init-field to field. And added life-cycle-length.

(require "settings.rkt")
(provide size-powerup% speed-powerup% clear-powerup% collision-powerup% super-powerup%)
(define powerup%
  (class object%
    (init-field color
                [x-pos (random 100 700)];Where the powerup is drawn
                [y-pos (random 100 (- frame-height 100))]
                ;The length of the whole life of the powerup if no one takes it.
                [life-cycle-length 1000]
                ;How much the life-cycle-length can vary.
                [variance 800]
                ;Temporary variable for life-cycle-length that changes throughout
                ;the life of the powerup, with a bit of variance so that it's not
                ;too predictable and so that the powerups don't all spawn at the
                ;same time.
                [life-cycle-countdown (random life-cycle-length
                                              (+ life-cycle-length variance))]
                ;How long the effect lasts for a curve that takes the powerup.
                [effect-duration 300]
                ;How long the powerup is spawened before it despawns again.
                [spawn-duration 800])
    (field [effected-curve #f]
           ;Temporary wariable for how long the curve is effected.
           [effect-duration-countdown 800]
           [powerup-bitmap (make-object bitmap% (- frame-width 200) (+ frame-height 40) #f 0.5)]
           [powerup-dc (new bitmap-dc% [bitmap powerup-bitmap])])

    ;Resets the powerups when a new round starts. Removes the effect from the curves
    ;and removes the powerups from canvas.
    (define/public (new-round)
      (set! effect-duration-countdown 1)
      (send powerup-dc erase)
      (set! life-cycle-countdown 0))

    ;Curves can collide with powerups regardless of which level they are on.
    ;IN: symbol OUT: bitmap
    (define/public (get-bitmap-dc bitmap-level)
      powerup-dc)

    ;Resets some variables to default. Is called when the powerup despawns or when
    ;a curve collides with it.
    (define/public (reset-powerup)
      (send powerup-dc erase)
      (set! x-pos (random 100 (- frame-width 350)))
      (set! y-pos (random 100 (- frame-height 100))))

    ;Draws the powerup to dc.
    ;IN: dc
    (define/public (draw-powerup dc)
      (send powerup-dc set-pen color 4 'solid)
      (send powerup-dc draw-rectangle (- x-pos 10) (- y-pos 10) 20 20)
      (send dc draw-bitmap-section powerup-bitmap
            (- x-pos 12) (- y-pos 12)
            (- x-pos 12) (- y-pos 12) 30 30))

    ;Controls the spawn of the powerup.
    ;life-cycle-countdown is the entire cycle of the powerup, this is what
    ;changes during the life of the powerup.
    ;Spawn-duration is how long the powerup is spawned if no one takes it.
    (define/public (update dc)
      (cond ((equal? life-cycle-countdown 0)
             (reset-powerup)
             (set! life-cycle-countdown (random life-cycle-length
                                                (+ life-cycle-length variance))))
            ((< life-cycle-countdown spawn-duration)
             (draw-powerup dc)
             (set! life-cycle-countdown (sub1 life-cycle-countdown)))
            (else
             (set! life-cycle-countdown (sub1 life-cycle-countdown)))))

    ;This function is called when a object of curve% collides with this.
    ;IN: curve%
    (define/public (apply-on-hit-effect curve)
      (set! effect-duration-countdown effect-duration)
      (set! effected-curve curve)
      (reset-powerup)
      (set! life-cycle-countdown (random life-cycle-length
                                         (+ life-cycle-length variance))))    
    (super-new)))

;Changes the size of the curve that takes it.
(define size-powerup%
  (class powerup%
    (inherit-field effect-duration-countdown
                   effected-curve)

    ;Resets the effect to the effected-curve when effect-duration-countdown is 0.
    (define (effect-loop)
      ;effect-duration-countdown is the temporary variable that keeps track of how long
      ;a curve has been effected with this powerup.
      (set! effect-duration-countdown (- effect-duration-countdown 1))
      (when (equal? effect-duration-countdown 0)
        ;It has to move forward a bit because otherwise it collides with itself
        ;when the size is set to default.
        (send effected-curve set-x-pos
              (+ (send effected-curve get-x-pos)
                 (* (/ (send effected-curve get-size) 2)
                    (cos (send effected-curve get-angle)))))
        (send effected-curve set-y-pos
              (+ (send effected-curve get-y-pos)
                 (* (/ (send effected-curve get-size) 2)
                    (sin (send effected-curve get-angle)))))
        (send effected-curve set-size! default-size)))

    ;Runs the effect-loop
    (define powerup-clock
      (new timer%
           [just-once? #f]
           [notify-callback effect-loop]))

    ;This function is called when a object of curve% collides with this.
    ;IN: curve%
    (define/override (apply-on-hit-effect curve)
      (super apply-on-hit-effect curve)
      (send powerup-clock start 10 #f)
      (send effected-curve set-size! 20))
    (super-new)))

;Changes the speed of the curve that takes it.
(define speed-powerup%
  (class powerup%
    (inherit-field effect-duration-countdown
                   effected-curve)

    ;Resets the effect to the effected-curve when effect-duration-countdown is 0.
    (define (effect-loop)
      ;effect-duration-countdown is the temporary variable that keeps track of how long
      ;a curve has been effected with this powerup.
      (set! effect-duration-countdown (sub1 effect-duration-countdown))
      (when (equal? effect-duration-countdown 0)
        (send effected-curve set-speed! default-speed)))

    ;Runs the effect-loop.
    (define powerup-clock
      (new timer%
           [just-once? #f]
           [notify-callback effect-loop]))

    ;This function is called when a object of curve% collides with this.
    ;IN: curve%
    (define/override (apply-on-hit-effect curve)
      (super apply-on-hit-effect curve)
      (send powerup-clock start 10 #f)
      (send curve set-speed! 4.5))
    (super-new)))

;Clears the current bipmap of the curve that takes it.
;No reset function needed.
(define clear-powerup%
  (class powerup%
    (define/override (apply-on-hit-effect curve)
      (super apply-on-hit-effect curve)
      (send curve erase-current-dc))
    (super-new)))

;Stops the curve that takes it from drawing the curve, and sets collision to #f.
;This makes the collisions act differently. Some objects apply-on-hit-effect take
;collision into consideration and others don't.
(define collision-powerup%
  (class powerup%
    (inherit-field effect-duration-countdown
                   effected-curve)

    ;Resets the effect to the effected-curve when effect-duration-countdown is 0.
    (define (effect-loop)
      ;effect-duration-countdown is the temporary variable that keeps track of how long
      ;a curve has been effected with this powerup.
      (set! effect-duration-countdown (sub1 effect-duration-countdown))
      (when (equal? effect-duration-countdown 0)
        (send effected-curve set-hole! #f)))

    ;Runs the effect-loop.
    (define powerup-clock
      (new timer%
           [just-once? #f]
           [notify-callback effect-loop]))

    ;This function is called when a object of curve% collides with this.
    ;IN: curve%
    (define/override (apply-on-hit-effect curve)
      (super apply-on-hit-effect curve)
      (send powerup-clock start 10 #f)
      (send effected-curve set-hole! #t))
    (super-new)))

;The superpowerup sends all curves that take it to another bitmap, "behind" the old one.
;They can only collide with other curves on that new bitmap. You can return to the old
;bitmap in one of two ways. Either by waiting till the powerup runs out, or by taking
;it again on another plays on the canvas (this will be clearer later after you've read
;draw-powerup and apply-on-hit-effect).
(define super-powerup%
  (class powerup%
    (inherit-field effect-duration
                   effect-duration-countdown
                   powerup-dc
                   color
                   x-pos
                   y-pos
                   variance
                   powerup-bitmap
                   spawn-duration
                   life-cycle-countdown
                   life-cycle-length)
    (init-field ;The powerup is drawn as two circles, these are the
     ;coordinates for the second one.
     [x-pos2 (- frame-width 440)]
     [y-pos2 (- (/ frame-height 2) 60)]
     ;More than one curve can take this powerup.
     [effected-curves (list )])


    ;The super reset-powerup changes the x-pos y-pos as well, we dont want that here.
    (define/override (reset-powerup)
      (send powerup-dc erase))

    ;Now the powerup is on as long as the powerup is displayed on canvas.
    ;This way the powerup ends at the same time for all curves.
    (define/override (update dc)
      (cond ((equal? life-cycle-countdown 0)
             (reset-powerup)
             (set! life-cycle-countdown (random life-cycle-length
                                                (+ life-cycle-length variance))))
            ((< life-cycle-countdown spawn-duration)
             (draw-powerup dc)
             ;----------------Difference-from--the-superclass-------------
             (set! effect-duration-countdown life-cycle-countdown)
             ;------------------------------------------------------------
             (set! life-cycle-countdown (sub1 life-cycle-countdown)))
            (else
             (set! life-cycle-countdown (sub1 life-cycle-countdown)))))

    ;How the superpowerup is drawn to dc.
    ;IN: a-dc OUT: void
    (define/override (draw-powerup dc)
      (send dc set-pen white 5 'solid)
      ;A countdown for how long the powerup is active is displayed to the right in
      ;the playing field.
      (send dc draw-line
            (- frame-width 240) (* (- frame-height 10)
                                   (/ (- effect-duration life-cycle-countdown)
                                      effect-duration))
            (- frame-width 240) (- frame-height 10))
      (send powerup-dc set-pen color 4 'xor)
      (send powerup-dc set-brush black 'transparent)
      (send powerup-dc set-text-foreground white)
      ;Draws two circles at opposite sides of the playing field.
      (send powerup-dc draw-ellipse x-pos y-pos 120 120)
      (send powerup-dc draw-ellipse x-pos2 y-pos2 120 120)
      ;Taking the left one sends a curve to the "back" or, if you will, "down" the hole.
      (send powerup-dc draw-text "DOWN" (+ x-pos 33) (+ y-pos 51))
      ;The one to the right sends you to the "front" by going "up" the hole. 
      (send powerup-dc draw-text "UP" (+ x-pos2 48) (+ y-pos2 51))
      (send dc draw-bitmap powerup-bitmap 0 0 'solid))

    ;Combines the "back" and "front" bitmap of the effected-curves that took the
    ;powerup so that all that's been drawn is now on the default "front" bitmap. Also
    ;moves all the effected-curves to the front bitmap again.
    (define (effect-loop)
      ;effect-duration-countdown is the temporary variable that keeps track of how long
      ;the powerup has been active.
      (set! effect-duration-countdown (- effect-duration-countdown 1))
      (when (equal? effect-duration-countdown 0)
        (send powerup-dc erase)
        (map (lambda (a-curve)
               (send a-curve combine-bitmaps)
               (send a-curve erase-superpowerup-dc))
             effected-curves)
        (map (lambda (a-curve) (send a-curve set-current-bitmap&dc-default))
             effected-curves)))

    ;Runs the effect-loop.
    (define powerup-clock
      (new timer%
           [just-once? #f]
           [notify-callback effect-loop]))

    ;This function is called when a object of curve% collides with this.
    ;IN: curve%
    (define/override (apply-on-hit-effect curve)
      (send powerup-clock start 10 #f)
      (set! effected-curves (cons curve effected-curves))
      ;For the left circle the x-pos is less than half the screen width.
      ;This might have been done in a fancier way but this is super simple
      ;and effective.
      (if (< (send curve get-x-pos) (/ (- frame-width 250) 2))
          ;Sends the curve to the "back".
          (send curve set-current-bitmap&dc-superpowerup)
          ;Sends the curve to the "front".
          (send curve set-current-bitmap&dc-default)))
    (super-new)))