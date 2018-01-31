#lang racket

; Lol backend

(provide setup
         save-picture
         get-next-picture)

(require racket/draw
         "utils.rkt"
         "picture.rkt")

(define input-files #f)
(define output-path #f)
(define current-out-index 0)

(define (setup input-dir output-dir)
  (set! input-files
        (directory-list input-dir #:build? #t))
  (set! output-path output-dir))

(define save-picture
  (let ([current-index 0])
    (lambda (pic)
      (for ([n (in-naturals)]
            [mask (in-list (picture-masks pic))])
        (send (apply-mask (picture-bitmap pic) mask)
              save-file
              (build-path output-path
                          ; Filename is in format [XXXX]-[YY].png
                          ; where XXXX is the image number and YY is the mask number
                          (string-append
                           (~r current-index #:min-width 4 #:pad-string "0")
                           "-"
                           (~r n #:min-width 2 #:pad-string "0")
                           ".png"))
              'png 90))
      (set! current-index (add1 current-index)))))

(define (get-next-picture)
  (if (empty? input-files)
      #f
      (let ([next-file (car input-files)])
        (set! input-files (cdr input-files))
        (with-handlers ([exn? (lambda (e) (get-next-picture))])
          (make-picture (read-bitmap next-file))))))