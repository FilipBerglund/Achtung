#lang racket/gui
(require "Abstractions.rkt")
(provide size-powerup% speed-powerup% clear-powerup% collision-powerup% super-powerup%)
(define powerup%
  (class object%
    (init-field color
                [x-pos (random 100 700)];Where the powerup is drawn
                [y-pos (random 100 500)]
                [spawn-countdown (random 1000 1300)]
                [effect-duration 300]
                [rarity 1000]
                [spawn-duration 800]
                [tmp-duration 800];How long the 
                [affected-curve #f]
                [powerup-bitmap (make-object bitmap% 850 650 #f 0.5)]
                [powerup-dc (new bitmap-dc% [bitmap powerup-bitmap])])

    (define/public (new-round)
      (set! tmp-duration 2)
      (send powerup-dc erase)
      (set! spawn-countdown 0))
    
    (define/public (get-bitmap-dc bitmap-level)
      powerup-dc)
    
    (define/public (reset-powerup dc)
      (send powerup-dc erase)
      (set! x-pos (random 100 700))
      (set! y-pos (random 100 500))
      (send dc draw-bitmap powerup-bitmap 0 0 'solid))

    (define/public (draw-powerup dc)
      (send powerup-dc set-pen color 4 'solid)
      (send powerup-dc draw-rectangle (- x-pos 10) (- y-pos 10) 20 20)
      (send dc draw-bitmap-section powerup-bitmap
            (- x-pos 12) (- y-pos 12)
            (- x-pos 12) (- y-pos 12) 30 30))

    (define/public (update dc)
      (cond ((equal? spawn-countdown 0)
             (reset-powerup dc)
             (set! spawn-countdown (random rarity (+ rarity 300))))
            ((< spawn-countdown spawn-duration)
             (send this draw-powerup dc)
             (set! spawn-countdown (sub1 spawn-countdown)))
            (else
             (set! spawn-countdown (sub1 spawn-countdown)))))
    (super-new)))

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
      (set! tmp-duration (- tmp-duration 1))
      (when (equal? tmp-duration 0)
        (send affected-curve set-x-pos (+ (send affected-curve get-x-pos)
                                          (* (/ (send affected-curve get-size) 2) (cos (send affected-curve get-angle)))))
        (send affected-curve set-y-pos (+ (send affected-curve get-y-pos)
                                          (* (/ (send affected-curve get-size) 2) (sin (send affected-curve get-angle)))))
        (send affected-curve set-size! 7)))
    
    (define powerup-clock
      (new timer%
           [just-once? #f]
           [notify-callback effect-loop]))
    
    (define/public (apply-on-hit-effect curve)
      (set! tmp-duration effect-duration)
      (send powerup-clock start 10 #f)
      (set! affected-curve curve)
      (send affected-curve set-size! 20)
      (send powerup-dc erase)
      (set! x-pos (random 100 700))
      (set! y-pos (random 100 500))
      (set! spawn-countdown (random rarity (+ rarity 300))))
    (super-new)))

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
    
    (define/public (apply-on-hit-effect curve)
      (set! tmp-duration effect-duration)
      (send powerup-clock start 10 #f)
      (set! affected-curve curve)
      (send affected-curve set-speed! 4.5)
      (send powerup-dc erase)
      (set! x-pos (random 100 700))
      (set! y-pos (random 100 500))
      (set! spawn-countdown (random rarity (+ rarity 300))))
    (super-new)))

(define clear-powerup%
  (class powerup%
    (inherit-field effect-duration
                   affected-curve
                   powerup-dc
                   x-pos
                   y-pos
                   spawn-countdown
                   rarity)
    
    (define/public (apply-on-hit-effect curve)
      (set! affected-curve curve)
      (send affected-curve erase-current-dc)
      (send powerup-dc erase)
      (set! x-pos (random 100 700))
      (set! y-pos (random 100 500))
      (set! spawn-countdown (random rarity (+ rarity 300))))
    (super-new)))

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
    
    (define/public (apply-on-hit-effect curve)
      (set! tmp-duration effect-duration)
      (send powerup-clock start 10 #f)
      (set! affected-curve curve)
      (send affected-curve set-hole! #t)
      (send powerup-dc erase)
      (set! x-pos (random 100 700))
      (set! y-pos (random 100 500))
      (set! spawn-countdown (random rarity (+ rarity 300))))
    (super-new)))

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
    (init-field [affected-curves (list )]
                [x-pos2 610]
                [y-pos2 260])
    
    (define/override (reset-powerup dc)
      (send powerup-dc erase)
      (send dc draw-bitmap powerup-bitmap 0 0 'solid))
    
    (define/override (update dc)
      (cond ((equal? spawn-countdown 0)
             (reset-powerup dc)
             (set! spawn-countdown (random rarity (+ rarity 300))))
            ((< spawn-countdown spawn-duration)
             (send this draw-powerup dc)
             ;Now the powerup is on as long as the powerup is displayed on canvas.
             (set! tmp-duration spawn-countdown)
             (set! spawn-countdown (sub1 spawn-countdown)))
            (else
             (set! spawn-countdown (sub1 spawn-countdown)))))

    (define/override (draw-powerup dc)
      (send dc set-pen white 5 'solid)
      (send dc draw-line 810 (* 600 (/ (- effect-duration spawn-countdown) effect-duration)) 810 600)
      (send powerup-dc set-pen color 4 'xor)
      (send powerup-dc set-brush black 'xor)
      (send powerup-dc set-text-foreground white)
      (send powerup-dc draw-ellipse x-pos y-pos 120 120)
      (send powerup-dc draw-ellipse x-pos2 y-pos2 120 120)
      (send powerup-dc draw-text "DOWN" (+ x-pos 33) (+ y-pos 51))
      (send powerup-dc draw-text "UP" (+ x-pos2 48) (+ y-pos2 51))
      (send dc draw-bitmap powerup-bitmap 0 0 'solid))
    
    (define (effect-loop)
      (set! tmp-duration (- tmp-duration 1))
      (when (equal? tmp-duration 0)
        (send powerup-dc erase)
        (set! spawn-countdown (random rarity (+ rarity 300)))
        (map (lambda (x) (send x combine-bitmaps) (send x erase-superpowerup-dc))
             affected-curves)
        (map (lambda (x) (send x set-current-bitmap&dc-default))
             affected-curves)))
    
    (define powerup-clock
      (new timer%
           [just-once? #f]
           [notify-callback effect-loop]))
    
    (define/public (apply-on-hit-effect curve)
      (send powerup-clock start 10 #f)
      (set! affected-curves (cons curve affected-curves))
      (if (< (send curve get-x-pos) 300)
          (send curve set-current-bitmap&dc-superpowerup)
          (send curve set-current-bitmap&dc-default)))
    (super-new)))
