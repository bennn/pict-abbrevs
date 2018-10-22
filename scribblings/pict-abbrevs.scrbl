#lang scribble/manual

@require[
  scribble/example
  (for-label
    pict
    pict-abbrevs
    plot/no-gui
    racket/base
    racket/class
    racket/contract
    racket/draw
    racket/math)]

@(define (make-eval) (make-base-eval '(require pict pict-abbrevs)))

@title{Pict Abbrevs}

@defmodule[pict-abbrevs]

@defthing[#:kind "value" revolution real?]{
  Equal to @racket[(* 2 pi)], or @hyperlink["https://tauday.com/tau-manifesto"]{the tau constant}.
  Useful for rotating picts.

  @examples[#:eval (make-eval)
    (arrowhead 30 (*   0 revolution))
    (arrowhead 30 (* 1/4 revolution))
    (arrowhead 30 (* 1/2 revolution))
  ]
}

@defthing[#:kind "contract" pict-color/c (-> any/c boolean?)]{
  Flat contract for the kinds of values that @racketmodname[pict] functions
   can usually interpret as colors.
}

@defthing[#:kind "contract" rgb-triplet/c (-> any/c boolean?)]{
  Flat contract for an RBG triplet.
  See also: @secref["utils" #:doc '(lib "plot/scribblings/plot.scrbl")].
}

@defproc[(color%-update-alpha [c (is-a?/c color%)] [a (real-in 0 1)]) (is-a?/c color%)]{
  @examples[#:eval (make-eval)
    (define red (string->color% "red"))
    (disk 20 #:color red)
    (disk 20 #:color (color%-update-alpha red 0.3))]
}

@defproc[(rgb-triplet->color% [c rgb-triplet/c]) (is-a?/c color%)]{
  Converts an RGB triplet to a color object.
}

@defproc[(hex-triplet->color% [n (integer-in #x000000 #xffffff)]) (is-a?/c color%)]{
  Converts a Hex Code RGB to a color object.
  Expects inputs between @litchar{#x000000} and @litchar{#xffffff}.
}

@deftogether[(
  @defproc[(pict-bbox-sup [p pict?] ...) (listof pict?)]
  @defproc[(pict-bbox-sup* [p* (listof pict?)]) (listof pict?)]
)]{
  Returns a list of picts with identical width and height.
  The new picts may be old picts superimposed upon a blank background.

  @examples[#:eval (make-eval)
    (map frame (pict-bbox-sup (disk 10) (disk 20) (disk 5)))]
}

@defproc[(max* [r* (listof real?)]) real?]{
  Similar to @racket[max] but expects a list of numbers.
  Raises an @racket[exn:fail:contract?] error when given an empty list.

  @examples[#:eval (make-eval)
    (max* '(8 6 7))]
}

@defproc[(min* [r* (listof real?)]) real?]{
  Similar to @racket[min] but expects a list of numbers.
  Raises an @racket[exn:fail:contract?] error when given an empty list.

  @examples[#:eval (make-eval)
    (max* '(8 6 7))]
}

@defproc[(rule [w real?] [h real?] [#:color c pict-color/c "black"]) pict?]{
  Returns a @racket[filled-rectangle] with the given width, height, and color.

  @examples[#:eval (make-eval)
    (rule 20 2)
    (rule 1 10)
    (rule 8 8)]
}

@defproc[(string->color% [str string?]) (is-a?/c color%)]{
  Converts a string to a color object.
}

@defproc[(save-pict [p pict?] [ps path-string?] [kind (or/c 'png 'jpeg 'xbm 'xpm 'bmp) 'png]) boolean?]{
  Exports the given pict to the file @racket[ps], using @racket[kind] to determine the output format.
}


@section{raco pict}

To vertically append image files, or Racket modules:

@exec{raco pict vl-append FILE ...}

If @litchar{FILE} is a module, then it must contain a submodule named @tt{raco-pict}
that exports an identifier named @tt{raco-pict}.
For example:

@codeblock|{
#lang racket/base
(module+ raco-pict
  (require pict)
  (define raco-pict (disk 40)))
}|
