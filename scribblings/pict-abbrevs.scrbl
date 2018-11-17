#lang scribble/manual

@require[
  scribble/example
  (for-label
    pict
    ppict/2
    pict-abbrevs
    plot/no-gui
    racket/base
    racket/class
    racket/contract
    racket/draw
    racket/math
    (only-in slideshow/base current-slide-assembler margin client-w client-h))]

@(define x-eval (make-base-eval '(require pict ppict/2 pict-abbrevs (only-in slideshow/base margin client-w client-h current-font-size))))
@(define (make-eval) x-eval)

@title{Pict Abbrevs}

@defmodule[pict-abbrevs]{
  The @racketmodname[pict-abbrevs] module exports the following bindings and
  re-exports the contents of @racketmodname[pict-abbrevs/slideshow].
}

@defthing[#:kind "value" revolution real?]{
  Equal to @racket[(* 2 pi)], or @hyperlink["https://tauday.com/tau-manifesto"]{the tau constant}.
  Useful for rotating picts.
  @margin-note{If the name @litchar{revolution} is too long, then do @racket[(require (rename-in pict-abbrevs [revolution turn]))].}

  @examples[#:eval (make-eval)
    (arrowhead 30 (*   0 revolution))
    (arrowhead 30 (* 1/4 revolution))
    (arrowhead 30 (* 1/2 revolution))
  ]
}

@defthing[#:kind "contract" real% flat-contract?]{
  Same as @racket[(between/c 0 1)].
}

@defthing[#:kind "contract" nonnegative-real? flat-contract?]{
  Same as @racket[(>=/c 0)].

  @examples[#:eval (make-eval)
    (nonnegative-real? 0)
    (nonnegative-real? 2.77)
    (nonnegative-real? 9001)
    (nonnegative-real? -1)
    (nonnegative-real? 'X)]
}

@defthing[#:kind "contract" pict-color/c (-> any/c boolean?)]{
  Flat contract for the kinds of values that @racketmodname[pict] functions
   can usually interpret as colors.
}

@defproc[(pict-color->color% [pc pict-color/c] [#:default default pict-color/c]) (is-a?/c color%)]{
  Creates a @racket[color%] object.
  If @racket[pc] and @racket[default] are @racket[#false], returns @racket[(string->color% "white")].

  @examples[#:eval (make-eval)
    (pict-color->color% "blue")
    (pict-color->color% #f)]
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

@defproc[(add-rectangle-background [pp pict?]
                                   [#:radius radius real? 10]
                                   [#:color color pict-color/c "white"]
                                   [#:draw-border? draw-border? boolean? #false]
                                   [#:x-margin x-margin real? 0]
                                   [#:y-margin y-margin real? 0]) pict?]{
  Add a rectangle behind a pict.

  @examples[#:eval (make-eval)
    (add-rectangle-background (standard-fish 100 50) #:color "bisque")]
}

@defproc[(add-rounded-border [pp pict?]
                             [#:radius radius real? 10]
                             [#:background-color bg-color pict-color/c "white"]
                             [#:frame-width frame-width real? 1]
                             [#:frame-color frame-color pict-color/c "black"]
                             [#:x-margin x-margin real? 0]
                             [#:y-margin y-margin real? 0]) pict?]{
  Add a bordered rectangle behind a pict.

  @examples[#:eval (make-eval)
    (add-rounded-border (standard-fish 100 50) #:x-margin 20 #:y-margin 30)]
}

@defproc[(add-spotlight-background [pp pict?]
                                   [#:blur blur (or/c #f real?) 15]
                                   [#:border-color border-color pict-color/c "plum"]
                                   [#:color color pict-color/c border-color]
                                   [#:border-width border-width real? 10]
                                   [#:x-margin x-margin real? 40]
                                   [#:y-margin y-margin real? 40]) pict?]{
  Superimposes the given pict on a blurred ellipse.

  @examples[#:eval (make-eval)
    (add-spotlight-background (jack-o-lantern 80))
    (add-spotlight-background (jack-o-lantern 80)
                              #:border-color "firebrick"
                              #:color (color%-update-alpha (string->color% "white") 0)
                              #:border-width 15
                              #:x-margin 30
                              #:y-margin 5)]
}


@section{Slideshow Abbrevs}

@defmodule[pict-abbrevs/slideshow]{
  Helpers for working with @racketmodname[slideshow] or @racketmodname[ppict/2].
}

@defthing[#:kind "contract" slide-assembler/c chaperone-contract?]{
  Contract for a function that can be used to build @racketmodname[slideshow] slides.
  See also @racket[current-slide-assembler].
}

@defproc[(slide-assembler/background [base-assembler slide-assembler/c]
                                     [#:color background-color pict-color/c]
                                     [#:draw-border? draw-border? boolean? #false]
                                     [#:border-color border-color pict-color/c #false]
                                     [#:border-width border-width (or/c #f real?) #false]) slide-assembler/c]{
  Returns a slide assembler that: (1) uses the given @racket[base-assembler] to
  create a pict, and (2) superimposes the pict onto a @racket[filled-rectangle]
  that covers the screen.
  The optional arguments set the style of the background rectangle.

  @codeblock|{
    #lang racket/base
    (require pict-abbrevs slideshow)

    (parameterize ((current-slide-assembler (slide-assembler/background (current-slide-assembler) #:color "red"))
                   (current-font-size 60))
      (slide (t "HOLA")))
  }|
}

@defproc[(pixels->w% [x nonnegative-real?]) real%]{
  Converts a pixel distance to a percentage of the max screen width (i.e., @racket[(+ (* 2 margin) client-w)]).
  Raise an @racket[exn:fail:contract?] exception if the given distance exceeds the max width.
}

@defproc[(pixels->h% [x nonnegative-real?]) real%]{
  Converts a pixel distance to a percentage of the max screen height (i.e., @racket[(+ (* 2 margin) client-h)]).
  Raise an @racket[exn:fail:contract?] exception if the given distance exceeds the max height.
}

@defproc[(w%->pixels [w real%]) nonnegative-real?]{
  Converts a percent to the number of pixels required to cover that percent
   of @racket[client-w].

  @examples[#:eval (make-eval)
    (w%->pixels 1/10)
    (w%->pixels 5/10)
    (= client-w (w%->pixels 1))]
}

@defproc[(h%->pixels [w real%]) nonnegative-real?]{
  Converts a percent to the number of pixels required to cover that percent
   of @racket[client-h].
}

@defproc[(text/color [str string?] [c pict-color/c]) pict?]{
  Draws colored text.

  @examples[#:eval (make-eval)
    (text/color "red" "red")
    (parameterize ((current-font-size 20))
      (text/color "orchid" "orchid"))]
}

@deftogether[(
  @defproc[(at-underline [pp (or/c tag-path? pict-path?)] [#:abs-x abs-x real?] [#:abs-y abs-y real?]) refpoint-placer?]
  @defproc[(at-leftline [pp (or/c tag-path? pict-path?)] [#:abs-x abs-x real?] [#:abs-y abs-y real?]) refpoint-placer?]
)]{
  Returns a placer that places picts to a reference point relative to an existing
  pict within the base.
  For @racket[at-underline] the reference point is the bottom-left.
  For @racket[at-leftline] the reference point is the top-left.
  If given, @racket[abs-x] and @racket[abs-y] shift the reference point.
  See also @racket[at-find-pict].
}

@defproc[(make-underline [pp (or/c pict? real?)] [#:height height real?] [#:color color pict-color/c] [#:width width #f (or/c #f real?)]) pict?]{
  Draw a horizontal line wide enough to underline the given pict.

  @examples[#:eval (make-eval)
    (let ((word (text "Word")))
      (ppict-do
        (file-icon 50 40 "bisque")
        #:go (coord 1/2 1/2 'cc)
        word
        #:go (at-underline word)
        (make-underline word)))]
}

@defproc[(make-leftline [pp (or/c pict? real?)] [#:height height real?] [#:color color pict-color/c] [#:width width #f (or/c #f real?)]) pict?]{
  Draw a vertical line that is equally high as the given pict.

  @examples[#:eval (make-eval)
    (let ((word (text "Word")))
      (ppict-do
        (file-icon 100 80 "bisque")
        #:go (coord 1/2 1/2 'cc)
        word
        #:go (at-leftline word)
        (make-leftline word #:width 10)))]
}

@defproc[(make-highlight* [pp pict?] [tag symbol?] [#:color color pict-color/c]) pict?]{
  Add a background of the given color to all picts tagged (in the sense of
  @racket[tag-pict?]) with @racket[tag] in the scene @racket[pp].

  @examples[#:eval (make-eval)
    (ppict-do
      (blank 80 40)
      #:set (for/fold ((acc ppict-do-state))
                      ((i (in-range 8)))
              (ppict-do
                acc
                #:go (coord (/ (* i 10)  80) 9/10)
                (if (even? i)
                  (tag-pict (text "X") 'X)
                  (tag-pict (text "O") 'O))))
      #:set (make-highlight* ppict-do-state 'X))]
}

@defthing[#:kind "value" highlight-pen-color pict-color/c]{
  Default color for underlines, etc.
}

@defthing[#:kind "value" highlight-brush-color pict-color/c]{
  Default color for highlights.
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
