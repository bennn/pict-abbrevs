#lang racket/base

;; TODO
;; - use namespace-attach, remove the `(case (string->symbol ....) ....)`
;; - need to namespace-require after doing the attach

(require
  pict
  racket/path
  racket/set
  (only-in lang-file/read-lang-file
    lang-file?)
  (only-in pict-abbrevs/private/pict-abbrevs
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

(define (path-string->pict ps)
  (if (lang-file? ps)
    (lang-file->pict ps)
    (bitmap ps)))

(define RACO-PICT-KEY 'raco-pict)

(define (lang-file->pict ps)
  (define require-spec `(submod ,ps ,RACO-PICT-KEY))
  (define (fail)
    (raise-argument-error 'lang-file->pict (format "(and/c path-string? (has-submod ~a))" RACO-PICT-KEY) ps))
  (dynamic-require require-spec RACO-PICT-KEY fail))

(define (pict-eval cmd pict*)
  (define f (string->pict-function cmd))
  (apply f pict*))

(define (string->pict-function str)
  (eval (string->symbol str) pict-namespace))

;; =============================================================================

(module+ main
  (require racket/cmdline)
  (define *output* (make-parameter "raco-pict.png"))
  (command-line
   #:program "pict"
   #:once-any
   [("-o" "--output") of "Output filename" (*output* of)]
   #:args (cmd . FILE*)
   (define out-file (*output*))
   (define new-pict
     (if (null? FILE*)
       (path-string->pict cmd)
       (pict-eval cmd (map path-string->pict FILE*))))
   (and (save-pict out-file new-pict)
        (printf "Saved result to '~a'~n" out-file))))
