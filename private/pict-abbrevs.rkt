#lang racket/base

(require racket/contract)
(provide
  nonnegative-real?
  real%
  pict-abbrevs-logger
  log-pict-abbrevs-debug
  log-pict-abbrevs-info
  log-pict-abbrevs-warning
  log-pict-abbrevs-error
  log-pict-abbrevs-fatal
  (contract-out
    [revolution
      real?]
    [pict-color/c
      (-> any/c boolean?)]
    [pict-color->color%
      (->* [pict-color/c] [#:default pict-color/c] (is-a?/c color%))]
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
    ))

(require
  (only-in racket/class send is-a? is-a?/c make-object)
  (only-in racket/draw color% make-color the-color-database)
  (only-in racket/math pi)
  pict)

;; =============================================================================

(define-logger pict-abbrevs)

(define revolution (* 2 pi))

(define real% (real-in 0 1))

(define nonnegative-real? (>=/c 0))

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

(define (pict-color->color% pc #:default [default #f])
  (cond
    [(is-a? pc color%)
     pc]
    [(string? pc)
     (string->color% pc)]
    [(not pc)
     (if default
       (pict-color->color% default #:default #false)
       (string->color% "white"))]
    [else
      (raise-argument-error 'pict-color->color% "pict-color/c" pc)]))

;; =============================================================================

(module+ test
  (require rackunit)

)
