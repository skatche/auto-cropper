#lang racket/gui

(require "utils.rkt"
         "picture.rkt"
         "cropping-canvas.rkt"
         "backend.rkt")

; Internal variables
(define-values (screen-width screen-height)
  (get-display-size))

(define current-picture #f)
(define current-mask 0) ; Stores the index, not the mask


; Loading window
(define loading-dialog
  (new frame% [label "Auto-Cropper"]
       [x (quotient screen-width 4)]
       [y (quotient screen-height 4)]))

; Begin button should only be enabled if both text fields are valid.
; This callback is used for both text fields.
(define (begin-button-check obj evt)
  (let ([input-value (send input-directory-field get-value)]
        [output-value (send output-directory-field get-value)])
    (if (and (directory-exists? input-value)
             (or (eq? output-value "")
                 (directory-exists? output-value)))
        (send begin-button enable #t)
        (send begin-button enable #f))))

(define loading-pane
  (new vertical-pane% [parent loading-dialog]
       [alignment '(center center)]))

(define input-directory-pane
  (new horizontal-pane% [parent loading-pane]
       [alignment '(right center)]))

(define input-directory-field
  (new text-field% [parent input-directory-pane]
       [label "Input Directory"]
       [min-width 300]
       [callback begin-button-check])) ; Defined below.

(define input-directory-button
  (new button% [parent input-directory-pane]
       [label "..."]
       [callback
        (lambda (btn evt)
          (let ([input-dir (get-directory "Input Directory")])
            (and input-dir
                 (send input-directory-field set-value
                       (path->string input-dir))
                 ; Trigger the text field callback
                 (send input-directory-field command
                       (new control-event%
                            [event-type 'text-field]
                            [time-stamp (current-milliseconds)])))))]))

(define output-directory-pane
  (new horizontal-pane% [parent loading-pane]
       [alignment '(right center)]))

(define output-directory-field
  (new text-field% [parent output-directory-pane]
       [label "Output Directory"]
       [min-width 300]
       [callback begin-button-check])) ; Defined below.

(define output-directory-button
  (new button% [parent output-directory-pane]
       [label "..."]
       [callback
        (lambda (btn evt)
          (let ([output-dir (get-directory "Output Directory")])
            (and output-dir
                 (send output-directory-field set-value
                       (path->string output-dir))
                 (send output-directory-field command
                       (new control-event%
                            [event-type 'text-field]
                            [time-stamp (current-milliseconds)])))))]))

(define begin-button
  (new button% [parent loading-pane]
       [label "Begin"]
       [enabled #f]
       [callback
        (lambda (btn evt)
          (setup (send input-directory-field get-value)
                 (send output-directory-field get-value))
          (set! current-picture (get-next-picture))
          (send loading-dialog show #f)
          (update-main-frame)
          (send main-frame show #t))]))

(send loading-dialog show #t)

; Main window construction
(define main-frame
  (new frame% [label "Auto-Cropper"]
       [x (quotient screen-width 4)]
       [y (quotient screen-height 4)]))

(define main-vert
  (new vertical-pane% [parent main-frame]))

(define main-horiz
  (new horizontal-pane% [parent main-vert]))

(define mask-list
  (new list-box% [parent main-horiz]
       [label #f]
       [choices '("0")]
       [min-width 100]
       [stretchable-width #f]
       [callback
        (lambda (lst evt)
          (let ([selection (send mask-list get-selection)])
            (if selection
                (begin (set! current-mask selection)
                       (update-main-frame))
                ; else prevent deselection
                (send current-mask select current-mask))))]))

(define main-panel
  (new panel% [parent main-horiz]
       [style '(auto-hscroll auto-vscroll)]
       [min-width 800]
       [min-height 600]))

(define cropping-canvas
  (new cropping-canvas% [parent main-panel]
       [width 300]
       [height 200]))

(define buttons-pane
  (new horizontal-pane% [parent main-vert]
       [alignment '(center center)]
       [stretchable-height #f]))

(define add-mask-button
  (new button% [parent buttons-pane]
       [label "Add Mask"]
       [callback
        (lambda (btn evt)
          (set! current-picture
                (picture-add-mask current-picture))
          (set! current-mask (add1 current-mask))
          (update-main-frame))]))

(define next-button
  (new button% [parent buttons-pane]
       [label "Next"]
       [callback
        (lambda (btn evt)
          (save-picture current-picture)
          (with-handlers
              ([exn?
                (lambda (e)
                  (message-box "All done!" "No more pictures left."
                               main-frame)
                  (exit))])
            (set! current-picture (get-next-picture))
            (set! current-mask 0)
            (update-main-frame)))]))

; Called when the "Begin" button is pressed on the setup window, or when the
; picture or mask changes.
(define (update-main-frame)
  ; Update the list of masks
  (send mask-list set
        (map ~r
             (stream->list
              (in-range
               (picture-num-masks current-picture)))))
  (send mask-list select current-mask)
  ; Update the main canvas
  (send cropping-canvas set-bitmap
        (picture-bitmap current-picture)
        (picture-get-mask current-picture current-mask)))

;; Menu
;(define menu-bar
;  (new menu-bar% [parent main-frame]))
;(define file-menu
;  (new menu% [parent menu-bar]
;       [label "&File"]))
;(define file-open
;  (new menu-item% [parent file-menu]
;       [label "&Open"]
;       [callback (lambda (item evt)
;                   (send cropping-canvas set-bitmap
;                         (read-bitmap (get-file))))]))
;(define file-test
;  (new menu-item% [parent file-menu]
;       [label "&Test"]
;       [callback (lambda (item evt)
;                   (send cropping-canvas set-bitmap
;                         (send cropping-canvas get-cropped-bitmap)))]))