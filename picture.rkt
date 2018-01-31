#lang racket

(provide (all-defined-out))

(require racket/draw
         "utils.rkt")

; Datatype for storing a bitmap with multiple masks
(struct picture (bitmap masks))
; Initialize with one empty mask
(define (make-picture bmp)
  (picture bmp
           (list (blank-mask bmp))))

; Extend the list of masks with a new blank mask, or provided mask
(define (picture-add-mask pic [mask #f])
  (let ([bmp (picture-bitmap pic)])
    (picture bmp
             (append (picture-masks pic)
                     (list (or mask
                               (blank-mask bmp)))))))

(define (picture-get-mask pic n)
  (list-ref (picture-masks pic) n))

(define (picture-num-masks pic)
  (length (picture-masks pic)))

(define (picture-delete-mask pic n)
  (picture (picture-bitmap pic)
           (let ([masks (picture-masks pic)])
             (append
              (take n pic)
              (cdr (drop n pic))))))