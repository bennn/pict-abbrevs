#lang info
(define collection "pict-abbrevs")
(define deps '("base" "pict-lib" "lang-file" "draw-lib"))
(define build-deps '("rackunit-lib" "racket-doc" "scribble-doc" "pict-doc" "draw-doc" "plot-doc" "plot-lib" "scribble-lib"))
(define pkg-desc "Pict command-line stuff")
(define version "0.2")
(define pkg-authors '(ben))
(define scribblings '(("scribblings/pict-abbrevs.scrbl" () (omit-start))))
(define raco-commands '(("pict" (submod pict-abbrevs/private/raco main) "Do pict stuff" #f)))
