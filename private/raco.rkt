#lang racket/base

;; TODO
;; - use namespace-attach, remove the `(case (string->symbol ....) ....)`
;; - need to namespace-require after doing the attach

(require
  pict
  racket/path
  racket/set
  (only-in gtp-plot/util
    save-pict))

;; =============================================================================

(define-namespace-anchor nsa)

(define pict-namespace
  (parameterize ([current-namespace (make-empty-namespace)])
    (namespace-attach-module (namespace-anchor->namespace nsa) 'pict)
    (namespace-require 'pict)
    (current-namespace)))

;; -----------------------------------------------------------------------------

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
  (eval (string->symbol str) pict-namespace))

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
   (and (save-pict out-file new-pict)
        (printf "Saved result to '~a'~n" out-file))))
