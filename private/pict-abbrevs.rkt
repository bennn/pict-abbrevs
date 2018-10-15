#lang racket/base

(require racket/contract)
(provide
  (contract-out
    [save-pict
     (->* [path-string? pict?] [(or/c 'png 'jpeg 'xbm 'xpm 'bmp)] boolean?)]))

(require
  (only-in racket/class send)
  (only-in pict pict? pict->bitmap))

;; =============================================================================

(define (save-pict fn p [kind 'png])
  (define bm (pict->bitmap p))
  (send bm save-file fn 'png))

;; =============================================================================

(module+ test
  (require rackunit)

)
