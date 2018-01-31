#lang racket/gui

(provide (all-defined-out))

(require "utils.rkt")

; The main canvas class
(define cropping-canvas%
  (class canvas%
    (inherit min-client-width
             min-client-height
             refresh
             get-dc
             screen->client)
    
    (define bitmap #f)
    (define mask #f)
    (define mask-dc #f)

    (init [width 300]
          [height 200])
    (define min-width width)
    (define min-height height)

    ; Bitmap wrangling
    (define/public (set-bitmap bmp [new-mask #f])
      (if bmp
          (let ([w (send bmp get-width)]
                [h (send bmp get-height)])
            ; Resize canvas to accommodate
            (min-client-width w)
            (min-client-height h)

            ; Create mask, or use new-mask
            (set! bitmap bmp)
            (set! mask (or new-mask (make-bitmap w h)))
            (set! mask-dc (new bitmap-dc% [bitmap mask]))

            ; Fill mask with black, if new
            (or new-mask
                (begin (send mask-dc set-background "black")
                       (send mask-dc clear))))

          ; else
          (begin
            (set! bitmap #f)
            (set! mask #f)
            (set! mask-dc #f)
            (min-client-width min-width)
            (min-client-height min-height)))
      (refresh))

    (define/public (set-mask new-mask)
      (set! mask new-mask)
      (and mask
           (begin (set! mask-dc (new bitmap-dc% [bitmap mask]))
                  (refresh))))

    ; Bitmap/mask export functions
    (define/public (get-bitmap)
      bitmap)
    (define/public (get-mask-copy) ; Note: doesn't return actual mask
      (copy-bitmap mask))
    ; get-cropped-bitmap applies the mask and returns the result
    (define/public (get-cropped-bitmap)
      (apply-mask bitmap mask))
          

    ; Handle mouse events
    (define start-x #f)
    (define start-y #f)
    (define current-x #f)
    (define current-y #f)
    (define click-type #f) ; Stores 'left or 'right depending on click type
    
    (define/override (on-event evt)
      (let ([x (send evt get-x)]
            [y (send evt get-y)])
        (cond
          [(send evt button-down?)
           (set! start-x x)
           (set! start-y y)
           (set! current-x start-x)
           (set! current-y start-y)
           (set! click-type
                 (cond
                   [(send evt button-down? 'left) 'left]
                   [(send evt button-down? 'right) 'right]
                   [else #f]))]
          
          [(send evt button-up?)
           (and click-type
                (begin (add-mask-rectangle start-x start-y
                                           current-x current-y
                                           click-type)
                       (set! click-type #f)))]
          
          [(send evt dragging?)
           (set! current-x x)
           (set! current-y y)]
          [else (yield)])
        (refresh)))

    ; Mask modification
    (define (add-mask-rectangle x0 y0 x1 y1 click-type)
      (and bitmap
           (send mask-dc set-background "black")
           (let ([width (abs (- x0 x1))]
                 [height (abs (- y0 y1))]
                 [x (min x0 x1)]
                 [y (min y0 y1)])
             (send mask-dc set-clipping-rect x y width height)
             (if (equal? click-type 'left)
                 (send mask-dc erase)
                 (send mask-dc clear)))))

    ; Paint function
    (define (paint canvas dc)
      (if bitmap
          (begin
            (send dc erase)
            ; Draw the masked image
            (send dc draw-bitmap
                  (apply-mask bitmap mask 0.5)
                  0 0)
            ; Draw the selection marquee
            (and click-type
                 (let ([x (min start-x current-x)]
                       [y (min start-y current-y)]
                       [width (abs (- start-x current-x))]
                       [height (abs (- start-y current-y))])
                   (draw-marquee dc x y width height))))
          ; else
          (begin
            (send dc set-background "white")
            (send dc clear))))

    ; Super-initialization
    (super-new [paint-callback paint]
               [min-width min-width]
               [min-height min-height]
               [stretchable-width #f]
               [stretchable-height #f])
    ))