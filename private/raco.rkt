#lang racket/base

(require
  pict
  racket/path
  racket/set
  (only-in gtp-plot/util
    save-pict))

;; =============================================================================

(define (infer-format ps*)
  (define ext* (for/set ([ps (in-list ps*)]) (path-get-extension ps)))
  (if (= 1 (set-count ext*))
    (car (set->list ext*))
    (raise-argument-error 'pict "files with same extension" ps*)))

(define path-string->pict
  bitmap)

(define (pict-eval cmd pict*)
  (define f (string->pict-function cmd))
  (apply f pict*))

(define (string->pict-function str)
  (case (string->symbol str)
   [(vl-append) vl-append]
   [(vc-append) vc-append]
   [else (raise-user-error 'string->pict-function str)]))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  (define *output* (make-parameter "raco-pict"))
  (command-line
   #:program "pict"
   #:once-any
   [("-o" "--output") of "Output filename" (*output* of)]
   #:args (cmd . FILE*)
   (define file-format ".png")
   (define out-file (path-add-extension (*output*) file-format))
   (define new-pict (pict-eval cmd (map path-string->pict FILE*)))
   (save-pict out-file new-pict)))
