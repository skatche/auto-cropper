#lang racket

(provide copy-bitmap
         blank-mask
         apply-mask!
         apply-mask
         draw-marquee)

(require racket/draw)

(define (copy-bitmap bmp)
  (let* ([new-bmp
          (make-bitmap (send bmp get-width)
                       (send bmp get-height))]
         [dc (new bitmap-dc% [bitmap new-bmp])])
    (send dc draw-bitmap bmp 0 0)
    new-bmp))

(define blank-mask
  (case-lambda
    [(w h)
     (let* ([mask (make-bitmap w h)]
            [mask-dc (new bitmap-dc% [bitmap mask])])
       (send mask-dc set-background "black")
       (send mask-dc clear)
       mask)]
    [(bmp)
     (blank-mask (send bmp get-width)
                 (send bmp get-height))]))

; NOTE: apply-mask! applies the mask IN-PLACE. Use apply-mask to get a copy.
(define (apply-mask! bmp mask [alpha 1.0])
  (let ([dc (new bitmap-dc% [bitmap bmp])])
    (send dc set-alpha alpha)
    (send dc draw-bitmap mask 0 0)))

(define (apply-mask bmp mask [alpha 1.0])
  (let ([copy (copy-bitmap bmp)])
    (apply-mask! copy mask alpha)
    copy))

(define (draw-marquee dc x y width height)
  (send dc set-brush "white" 'transparent)
  (send dc set-pen "white" 1 'solid)
  (send dc draw-rectangle x y width height)
  (send dc set-pen "black" 1 'dot)
  (send dc draw-rectangle x y width height))