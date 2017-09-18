#lang info
(define collection "pict-abbrevs")
(define deps '("base" "pict-lib" "gtp-plot"))
(define build-deps '("rackunit-lib" "racket-doc" "scribble-doc" "pict-doc"))
(define pkg-desc "Pict command-line stuff")
(define version "0.0")
(define pkg-authors '(ben))
(define scribblings '(("docs/pict-abbrevs.scrbl" () (omit-start))))
(define raco-commands '(("pict" (submod pict-abbrevs/private/raco main) "Do pict stuff" #f)))
