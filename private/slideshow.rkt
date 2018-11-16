#lang racket/base

(require racket/contract)
(provide
  slide-assembler/c
  (contract-out
    [slide-assembler/background
      (->* [slide-assembler/c #:color pict-color/c]
           [#:draw-border? boolean?
            #:border-color pict-color/c
            #:border-width (or/c #f real?)]
           slide-assembler/c)]
    [pixels->w%
      (-> nonnegative-real? real%)]
    [pixels->h%
      (-> nonnegative-real? real%)]
    [w%->pixels
      (-> real% nonnegative-real?)]
    [h%->pixels
      (-> real% nonnegative-real?)]
    [text/color
      (-> string? pict-color/c pict?)]
    [at-underline
      (->* [(or/c tag-path? pict-path?)] [#:abs-x real? #:abs-y real?] refpoint-placer?)]
    [at-leftline
      (->* [(or/c tag-path? pict-path?)] [#:abs-x real? #:abs-y real?] refpoint-placer?)]
    [at-highlight
      (->* [(or/c tag-path? pict-path?)] [#:abs-x real? #:abs-y real?] refpoint-placer?)]
    [make-underline
      (->* [(or/c real? pict?)] [#:height real? #:color pict-color/c] pict?)]
    [make-leftline
      (->* [(or/c real? pict?)] [#:width real? #:color pict-color/c] pict?)]
    [make-highlight*
      (-> pict? symbol? pict?)]))

(require
  pict
  pict-abbrevs/private/pict-abbrevs
  ppict/2
  (only-in slideshow/base client-w client-h margin current-main-font current-font-size))

;; =============================================================================

(define LINE-MARGIN 4)
(define highlight-pen-color (string->color% "royalblue"))
(define highlight-brush-color (color%-update-alpha highlight-pen-color 0.3))

(define slide-assembler/c
  (-> (or/c string? #f) exact-nonnegative-integer? pict? pict?))

(define ((slide-assembler/background base-assembler #:color color #:draw-border? [draw-border? #false] #:border-width [border-width #f] #:border-color [border-color #f]) slide-title slide-vspace slide-pict)
  (define foreground-pict (base-assembler slide-title slide-vspace slide-pict))
  (define background-pict
    (let ((+margin (* 2 margin))
          (-margin (- margin)))
      (inset
        (filled-rectangle (+ +margin client-w) (+ +margin client-h)
                          #:color color
                          #:draw-border? draw-border?
                          #:border-width border-width
                          #:border-color border-color)
        -margin)))
  (cc-superimpose background-pict
                  foreground-pict))

(define (pixels->w% x)
  (define max-x (+ (* 2 margin) client-w))
  (if (<= x max-x)
    (/ x max-x)
    (raise-arguments-error 'pixels->w% "pixel value exceeds screen width" "value" x "screen width" max-x)))

(define (pixels->h% y)
  (define max-y (+ (* 2 margin) client-h))
  (if (<= y max-y)
    (/ y max-y)
    (raise-arguments-error 'pixels->h% "pixel value exceeds screen height" "value" y "screen height" max-y)))

(define (w%->pixels w)
  (* w client-w))

(define (h%->pixels h)
  (* h client-h))

(define BLACK (string->color% "black"))

(define (text/color str color)
  (text str (cons (pict-color->color% color #:default BLACK) (current-main-font)) (current-font-size)))

(define (at-underline pp #:abs-x [abs-x 0] #:abs-y [abs-y LINE-MARGIN])
  (at-find-pict pp lb-find 'lt #:abs-x abs-x #:abs-y abs-y))

(define (make-underline pp #:height [height LINE-MARGIN] #:color [color highlight-pen-color])
  (rule (if (pict? pp) (pict-width pp) pp) height #:color color))

(define (at-leftline pp #:abs-y [abs-y 0] #:abs-x [abs-x -10])
  (at-find-pict pp lt-find 'lt #:abs-y abs-y #:abs-x abs-x))

(define (make-leftline pp #:width [width LINE-MARGIN] #:color [color highlight-pen-color])
  (rule width (if (pict? pp) (pict-height pp) pp) #:color color))

(define (at-highlight pp #:abs-x [abs-x 0] #:abs-y [abs-y -2])
  (at-find-pict pp lt-find 'lt #:abs-x abs-x #:abs-y abs-y))

(define (make-highlight pp #:color [color highlight-brush-color])
  (define-values [w h]
    (if (pict? pp)
      (values (pict-width pp) (pict-height pp))
      (values (car pp) (cdr pp))))
  (rule w h #:color color))

(define (make-highlight* pp tag #:color [color highlight-brush-color])
  (for/fold ((acc pp))
            ((tgt (in-list (find-tag* pp tag))))
    (ppict-do acc #:go (at-highlight tgt) (make-highlight (pict-path->wh pp tgt) #:color color))))

(define (pict-path->wh pp tgt)
  (define-values [x-lo y-lo] (lt-find pp tgt))
  (define-values [x-hi y-hi] (rb-find pp tgt))
  (cons (- x-hi x-lo) (- y-hi y-lo)))
