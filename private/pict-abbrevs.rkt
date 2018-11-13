#lang racket/base

(require racket/contract)
(provide
  ;slide-assembler/c
  ;nonnegative-real?
  ;real%
  (contract-out
    [revolution
      real?]
    [pict-color/c
      (-> any/c boolean?)]
    [rgb-triplet/c
      (-> any/c boolean?)]
    [color%-update-alpha
      (-> (is-a?/c color%) (real-in 0 1) (is-a?/c color%))]
    [rgb-triplet->color%
      (-> rgb-triplet/c (is-a?/c color%))]
    [hex-triplet->color%
      (-> (integer-in #x000000 #xffffff) (is-a?/c color%))]
    [pict-bbox-sup
      (->* [] #:rest (listof pict?) (listof pict?))]
    [pict-bbox-sup*
      (-> (listof pict?) (listof pict?))]
    [max*
      (-> (listof real?) real?)]
    [min*
      (-> (listof real?) real?)]
    [rule
      (->* [real? real?] [#:color pict-color/c] pict?)]
    [string->color%
      (-> string? (is-a?/c color%))]
    [save-pict
     (->* [path-string? pict?] [(or/c 'png 'jpeg 'xbm 'xpm 'bmp)] boolean?)]
;    [slide-assembler/background
;      (->* [slide-assembler/c #:color pict-color/c]
;           [#:draw-border? boolean?
;            #:border-color pict-color/c
;            #:border-width (or/c #f real?)]
;           slide-assembler/c)]
;;    [pixels->w%
;;      (-> nonnegative-real? real%)]
;;    [pixels->h%
;;      (-> nonnegative-real? real%)]
;    [w%->pixels
;      (-> real% nonnegative-real?)]
;    [h%->pixels
;      (-> real% nonnegative-real?)]
    ))

(require
  (only-in racket/class send is-a?/c make-object)
  (only-in racket/draw color% make-color the-color-database)
  (only-in racket/math pi)
  #;(only-in slideshow client-w client-h margin)
  pict)

;; =============================================================================

(define revolution (* 2 pi))

(define pict-color/c
  (or/c #f string? (is-a?/c color%)))

(define rgb-triplet/c
  (list/c real? real? real?))

(define (color%-update-alpha c a)
  (make-object color% (send c red) (send c green) (send c blue) a))

(define (rgb-triplet->color% rgb)
  (make-object color% (car rgb) (cadr rgb) (caddr rgb)))

(define (hex-triplet->color% x)
  (define-values [r g b]
    (values (arithmetic-shift x -16)
            (bitwise-and #x0000ff (arithmetic-shift x -8))
            (bitwise-and #x0000ff x)))
  (make-color r g b))

(define (pict-bbox-sup . p*)
  (pict-bbox-sup* p*))

;; Superimpose given picts onto blank backgrounds, such that each pict in
;;  result has same width and height (the max w/h)
(define (pict-bbox-sup* p*)
  (define w (max* (map pict-width p*)))
  (define h (max* (map pict-height p*)))
  (define bg (blank w h))
  (for/list ((p (in-list p*)))
    (cc-superimpose bg p)))

(define (string->color% str)
  (or (send the-color-database find-color str)
      (raise-argument-error 'string->color% "color-name?" str)))

(define (save-pict fn p [kind 'png])
  (define bm (pict->bitmap p))
  (send bm save-file fn 'png))

(define (max* n*)
  (if (null? n*)
    (raise-argument-error 'max* "non-empty list" n*)
    (reduce max n*)))

(define (min* n*)
  (if (null? n*)
    (raise-argument-error 'min* "non-empty list" n*)
    (reduce min n*)))

(define (reduce f x*)
  (for/fold ((acc (car x*)))
            ((x (in-list (cdr x*))))
    (f acc x)))

(define (rule w h #:color [c "black"])
  (filled-rectangle w h #:color c #:draw-border? #false))

;; -----------------------------------------------------------------------------
;; TODO move to slide moduel??

;(define slide-assembler/c
;  (-> (or/c string? #f) exact-nonnegative-integer? pict? pict?))
;
;(define ((slide-assembler/background base-assembler #:color color #:draw-border? [draw-border? #false] #:border-width [border-width #f] #:border-color [border-color #f]) slide-title slide-vspace slide-pict)
;  (define foreground-pict (base-assembler slide-title slide-vspace slide-pict))
;  (define background-pict
;    (let ((+margin (* 2 margin))
;          (-margin (- margin)))
;      (inset
;        (filled-rectangle (+ +margin client-w) (+ +margin client-h)
;                          #:color color
;                          #:draw-border? draw-border?
;                          #:border-width border-width
;                          #:border-color border-color)
;        -margin)))
;  (cc-superimpose background-pict
;                  foreground-pict))
;
;(define real% (real-in 0 1))
;
;(define nonnegative-real? (>=/c 0))
;
;; what if x is larger than client-w ???
;;(define (pixels->w% x)
;;  (/ x (+ (* 2 margin) client-w)))
;;
;;(define (pixels->h% y)
;;  (/ y (+ (* 2 margin) client-h)))
;
;(define (w%->pixels w)
;  (* w client-w))
;
;(define (h%->pixels h)
;  (* h client-h))

;;;(define (text/color str color)
;;;  (text str (cons c (current-main-font)) (current-font-size)))
;;;
;;;(define (at-underline pp #:abs-y [abs-y LINE-MARGIN])
;;;  (at-find-pict pp lb-find 'lt #:abs-y abs-y))
;;;
;;;(define (make-underline pp [height LINE-MARGIN] #:color [color HIGHLIGHT-COLOR] #:width [width #f])
;;;  (rule (or width (pict-width pp)) height #:color color))
;;;
;;;(define (at-leftline pp #:abs-y [abs-y 0] #:abs-x [abs-x -10])
;;;  (at-find-pict pp lt-find 'lt #:abs-y abs-y #:abs-x abs-x))
;;;
;;;(define (make-leftline pp [width LINE-MARGIN] #:color [color HIGHLIGHT-COLOR])
;;;  (rule width (pict-height pp) #:color color))
;;;
;;;(define (at-highlight pp #:abs-x [abs-x 0] #:abs-y [abs-y -2])
;;;  (at-find-pict pp lt-find 'lt #:abs-x abs-x #:abs-y abs-y))
;;;
;;;(define (make-highlight pp #:color [color HIGHLIGHT-BRUSH-COLOR])
;;;  (define-values [w h]
;;;    (if (pict? pp)
;;;      (values (pict-width pp) (pict-height pp))
;;;      (values (car pp) (cdr pp))))
;;;  (rule w h #:color color))
;;;
;;;(define (make-highlight* pp tag)
;;;  (for/fold ((acc pp))
;;;            ((tgt (in-list (find-tag* pp tag))))
;;;    (ppict-do acc #:go (at-highlight tgt) (make-highlight (pict-path->wh pp tgt)))))
;;;
;;;(define (pict-path->wh pp tgt)
;;;  (define-values [x-lo y-lo] (lt-find pp tgt))
;;;  (define-values [x-hi y-hi] (rb-find pp tgt))
;;;  (cons (- x-hi x-lo) (- y-hi y-lo)))



;; =============================================================================

(module+ test
  (require rackunit)

)
