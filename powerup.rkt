#lang racket/gui
(require "Abstractions.rkt")
(provide size-powerup% speed-powerup% clear-powerup% collision-powerup%)
(define powerup%
  (class object%
    (init-field color
                [x-pos (random 100 700)]
                [y-pos (random 100 500)]
                [effects (list )]
                [spawn-countdown (random 1000 1500)]
                [effect-duration 400]
                [rarity 1000]
                [spawn-duration 600]
                [tmp-duration 600]
                [affected-curve 1]
                [powerup-bitmap (make-object bitmap% 850 650 #f 0.5)]
                [powerup-dc (new bitmap-dc% [bitmap powerup-bitmap])])

    (define/public (new-round)
      (set! tmp-duration 10))
    
    (define/public (get-bitmap-dc)
      powerup-dc)
    
    (define (reset-powerup dc)
      (send powerup-dc erase)
      (set! x-pos (random 100 700))
      (set! y-pos (random 100 500)))

    (define/public (draw-powerup dc)
      (send powerup-dc set-pen color 20 'solid)
      (send powerup-dc draw-ellipse x-pos y-pos 8 8)
      (send dc draw-bitmap-section powerup-bitmap
            (- x-pos 20) (- y-pos 20)
            (- x-pos 20) (- y-pos 20) 40 40 'xor))

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
      (unless (send curve dead?)
      (set! tmp-duration effect-duration)
      (send powerup-clock start 10 #f)
      (set! affected-curve curve)
      (send affected-curve set-size! 20)
      (send powerup-dc clear)
      (set! x-pos (random 100 700))
      (set! y-pos (random 100 500))
      (set! spawn-countdown (random rarity (+ rarity 300)))))
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
      (send affected-curve set-speed! 5)
      (send powerup-dc clear)
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
      (send affected-curve clear-dc)
      (send powerup-dc clear)
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
      (send powerup-dc clear)
      (set! x-pos (random 100 700))
      (set! y-pos (random 100 500))
      (set! spawn-countdown (random rarity (+ rarity 300))))
    (super-new)))