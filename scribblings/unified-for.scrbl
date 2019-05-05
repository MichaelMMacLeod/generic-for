#lang scribble/manual

@(require racket/contract/base
          racket/sandbox
          scribble/examples
          (for-label (except-in racket/base for)
                     racket/contract/base
                     racket/format
                     racket/list
                     racket/match
                     unified-for
                     ))

@title{Unified @racket[for] Loops}

@author[(author+email "Michael M. MacLeod" "mmmacleo@ucsd.edu"
                      #:obfuscate? #t)]

@defmodule[unified-for]

This package consolidates the various flavors of @racket[for]---@racket[for],
@racket[for/list], @racket[for/vector], @racket[for/fold], and so on---into an
extensible @racket[for] macro that compiles directly to efficient
@seclink["Named_let" #:doc '(lib "scribblings/guide/guide.scrbl")]{named let} code,

The unified @racket[for] gains its functionality through @secref{Iterators} and
@secref{Accumulators}, which can be defined using TODO:define-iterator and
TODO:define-accumulator respectively. It also allows identifiers to be bound with
@seclink["match" #:doc '(lib "scribblings/reference/reference.scrbl")]{match} patterns.

@(define example-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'racket/base
                     #:requires '("../main.rkt"
                                  racket/list
                                  racket/format))))

@defform/subs[(for maybe-accumulator (loop-clause ...) body ...+)
              [(maybe-accumulator (code:line)
                                  accumulator-id
                                  (accumulator-id arg-form ...))
               (loop-clause [maybe-match-patterns iterator-clause])
               (maybe-match-patterns (code:line id ...)
                                     (code:line match-pattern-expr ...))
               (iterator-clause (code:line iterator-id)
                                (iterator-id arg-form ...))
               ]]{
 Iteratively evaluates @racket[body]s.
}

@examples[#:eval example-evaluator
          (for ([x (from-range 9)])
            (display x))
          (for (to-vector #:length 4)
               ([(app real-part r) (from-vector #(1+2i 3+4i 5+6i 7+8i))])
            (* r 2))
          (let ([table #hash((a . 0) (b . 1) (c . 2) (d . 3) (e . 4))])
            (for (to-fold [even-valued-keys empty])
                 ([key value (from-hash table)])
              (if (even? value)
                  (cons key even-valued-keys)
                  even-valued-keys)))]

@section{Iterators}

@section{Accumulators}

@defform/subs[(to-list maybe-reverse?)
              [(maybe-reverse? (code:line)
                               (code:line #:reverse? reverse?-expr))]
              #:contracts ([reverse?-expr boolean?])]{
 Accumulates single values into a list.

 If @racket[#:reverse?] is not provided, or @racket[reverse?-expr] evaluates to
 @racket[#t], @racket[to-list] accumulates items like @racket[for/list]. Otherwise,
 @racket[to-list] returns items in the opposite order. This can be more
 efficient than @racket[#:reverse? #f]---See @secref{Performance} for more information.
}

@examples[#:eval example-evaluator
          (for to-list
               ([x (from-list '(1 2 3 4 5))])
            (/ 1 x))
          (for (to-list #:reverse? #f)
               ([x (from-list '(1 2 3 4 5))])
            (/ 1 x))]

@defform/subs[(to-vector length-option)
              [(length-option (code:line)
                              expandable-option
                              fixed-option)
               (expandable-option (code:line #:grow-from initial-capacity-expr)
                                  (code:line #:grow-from initial-capacity-expr growth-option))
               (fixed-option (code:line #:length length-expr)
                             (code:line #:length length-expr #:fill fill-expr))
               (growth-option (code:line #:by multiplier-expr)
                              (code:line #:with growth-proc))]
              #:contracts ([initial-capacity-expr exact-positive-integer?]
                           [length-expr exact-nonnegative-integer?]
                           [fill-expr any/c]
                           [multiplier-expr (and/c exact-integer? (>=/c 2))]
                           [growth-proc (->i ([old-size exact-positive-integer?])
                                             [new-size (old-size)
                                              (and/c exact-integer? (>/c old-size))])])]{
}

@section{Performance}

The performance of @racket[for] depends in part upon the
choice of accumulator and iterators. All iterators and accumulators provided from
this package perform on-par with their @racketmodname[racket] counterparts, with
some including extra functionality, like @racket[to-list]'s @racket[#:reverse?],
and @racket[to-vector]'s @racket[#:grow-from], which can result in improved
performance.

The @racket[for] syntax only expands into code that uses @racket[match] if
non-identifier patterns are used. Otherwise, it is expanded directly into
code that uses @racket[let-values]. This improves iteration speed by a
small amount and reduces compiled bytecode sizes.

Check out the TODO:unified-for/benchmarks package for measurements.

@subsection{to-list}

The @racket[to-list] accumulator collects items by @racket[cons]ing them together.
Since this strategy produces a list in the opposite order of iteration,
@racket[to-list] @racket[reverse]s the result by default.
If @racket[#:reverse? #f] is provided, @racket[to-list] does not @racket[reverse] the
result, which improves performance.