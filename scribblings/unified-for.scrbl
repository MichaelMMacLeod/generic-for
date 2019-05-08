#lang scribble/manual

@(require racket/contract/base
          racket/sandbox
          scribble/examples
          (for-label (except-in racket/base for)
                     racket/contract/base
                     racket/format
                     racket/list
                     racket/math
                     racket/match
                     unified-for
                     ))

@title{Unified @racket[for] Loop}

@author[(author+email "Michael M. MacLeod" "mmmacleo@ucsd.edu")]

@defmodule[unified-for]

This package consolidates the various
@secref["Iteration_and_Comprehension_Forms"
        #:doc '(lib "scribblings/reference/reference.scrbl")] into a
single @racket[for] macro that compiles directly to efficient
@seclink["Named_let" #:doc '(lib "scribblings/guide/guide.scrbl")]{named let}
code. It also allows identifiers to be bound with
@seclink["match" #:doc '(lib "scribblings/reference/reference.scrbl")]{match} patterns.

@(define example-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'racket/base
                     #:requires '(unified-for
                                  racket/list
                                  racket/pretty
                                  racket/format))))

@defform/subs[(for maybe-accumulator (loop-clause ...) body ...+)
              [(maybe-accumulator (code:line)
                                  accumulator-id
                                  (accumulator-id arg-form ...))
               (loop-clause [match-pattern ... iterator-clause])
               (iterator-clause (code:line iterator-id)
                                (iterator-id arg-form ...))
               ]]{
 Iteratively evaluates @racket[body]s.
}

@section{Iterators}

An @deftech{iterator} is a
@seclink["stxtrans" #:doc '(lib "scribblings/reference/reference.scrbl")]{Syntax Transformer}
for use in the @racket[_iterator-clause] of @racket[for].

@defform[(from-list lst)
         #:contracts ([lst list?])]{
 Iterates over a @racket[list?].

 @examples[#:eval example-evaluator
           (for ([x (from-list '(1 2 3 4 5))])
             (display x))]
}

@defform[(from-vector vect)
         #:contracts ([vect vector?])]{
 Iterates over a @racket[vector?].

 @examples[#:eval example-evaluator
           (for ([x (from-vector #(1 2 3 4 5))])
             (display x))]
}

@defform/subs[(from-range option)
              [(option end-expr
                       (code:line start-expr end-expr)
                       (code:line start-expr end-expr step-expr))]
              #:contracts ([end-expr real?]
                           [start-expr real?]
                           [end-expr real?])]{
 Iterates over a range of @racket[real?] values from @racket[start] (inclusive) until
 @racket[end] (exclusive) by @racket[step]. If @racket[start-expr] or
 @racket[step-expr] are not provided, they are @racket[0] and @racket[1] respectively.

 @examples[#:eval example-evaluator
           (for ([x (from-range 5)])
             (display x))
           (for ([x (from-range 5 10)])
             (display x))
           (for ([x (from-range 10 0 -2)])
             (display x))]
}

@defform/subs[(from-naturals maybe-start)
              [(maybe-start (code:line)
                            start-expr)]
              #:contracts ([maybe-start exact-nonnegative-integer?])]{
 Iterates forever over @racket[natural?] numbers beginning with @racket[start], or
 @racket[0] if @racket[start] is not supplied.

 @examples[#:eval example-evaluator
           (for ([index from-naturals]
                 [v (from-list '(a b c d e f g))])
             (display (cons index v)))
           (for ([index+1 (from-naturals 1)]
                 [v (from-list '(a b c d e f g))])
             (display (cons index+1 v)))]
}

@defform[(from-hash hash-expr)
         #:contracts ([hash-expr hash?])]{
 Iterates over the keys and values of a @racket[hash?].

 @examples[#:eval example-evaluator
           (for ([key value (from-hash #hash((a . 1) (b . 2) (c . 3)))])
             (display (cons key value)))]
}

@subsection{Defining New Iterators and Accumulators}

Both @tech{iterator}s and @tech{accumulator}s are 

@racketgrammar*[[iterator
                 (([(outer-id ...) outer-expr] ...)
                  (outer-check-expr ...)
                  ([loop-id loop-expr] ...)
                  pos-guard-expr
                  ([(inner-id ...) inner-expr] ...)
                  pre-guard-expr
                  match-expr
                  post-guard-expr
                  (loop-arg-expr ...))]
                [accumulator
                 (([(outer-id ...) outer-expr] ...)
                  (outer-check-expr ...)
                  ([loop-id loop-expr] ...)
                  pos-guard-expr
                  ([(inner-id ...) inner-expr] ...)
                  pre-guard-expr
                  (body-result-id ...)
                  post-guard-expr
                  (loop-arg-expr ...)
                  done-expr)]]

@racketblock[(let*-values ([(outer-id ...) outer-expr] ...)
               outer-check-expr ...
               (let loop ([loop-id loop-expr] ...)
                 (when pos-guard-expr
                   (let*-values ([(inner-id ...) inner-expr] ...)
                     (when pre-guard-expr
                       (match match-expr @#,litchar{....})
                       (when post-guard-expr
                         (loop loop-arg-expr ...)))))))]

@racketblock[(let*-values ([(outer-id ...) outer-expr])
               outer-check-expr ...
               (let loop ([loop-id loop-expr] ...)
                 (if pos-guard-expr
                     (let*-values ([(inner-id ...) inner-expr] ...)
                       (if pre-guard-expr
                           (let-values ([(body-result-id ...) @#,litchar{....}])
                             (if post-guard-expr
                                 (loop loop-arg-expr ...)
                                 done-expr))
                           done-expr))
                     done-expr)))]

@section{Accumulators}

An @deftech{accumulator} is a
@seclink["stxtrans" #:doc '(lib "scribblings/reference/reference.scrbl")]{syntax transformer}
for use in the @racket[_maybe-accumulator] clause of @racket[for].

@defform[(to-void)]{
 Returns @|void-const|. The result of the @racket[for]'s @racket[_body] clause is
 ignored. It is the default @tech{accumulator} when none is provided to @racket[for].

 @examples[#:eval example-evaluator
           (for to-void
             ([x (from-range 5)]
              [y (from-range 4 0 -1)])
             (define x+y (+ x y))
             (display x+y)
             x+y)
           (for ([x (from-range 5)]
                 [y (from-range 4 0 -1)])
             (define x+y (+ x y))
             (display x+y)
             x+y)]
}

@defform/subs[(to-list maybe-reverse?)
              [(maybe-reverse? (code:line)
                               (code:line #:reverse? reverse?-expr))]
              #:contracts ([reverse?-expr boolean?])]{
 Accumulates single values into a @racket[list?].

 If @racket[#:reverse?] is not provided, or @racket[reverse?-expr] evaluates to
 @racket[#t], @racket[to-list] accumulates items like @racket[for/list]. Otherwise,
 @racket[to-list] returns items in the opposite order.

 @margin-note{
  Using @racket[#:reverse #f] can be more efficient than the default behavior. See
  @seclink["performance:to-list"]{Performance: to-list} for more information.
 }
 
 @examples[#:eval example-evaluator
           (for to-list
             ([x (from-range 5)])
             (* x 2))
           (for (to-list #:reverse? #f)
             ([x (from-range 5)])
             (* x 2))]
}

@defform/subs[(to-vector length-option)
              [(length-option (code:line)
                              expandable-option
                              fixed-option)
               (expandable-option (code:line #:grow-from initial-capacity-expr)
                                  (code:line #:grow-from initial-capacity-expr #:by multiplier-expr))
               (fixed-option (code:line #:length length-expr)
                             (code:line #:length length-expr #:fill fill-expr))]
              #:contracts ([initial-capacity-expr exact-positive-integer?]
                           [length-expr exact-nonnegative-integer?]
                           [fill-expr any/c]
                           [multiplier-expr (and/c exact-integer? (>/c 1))])]{
 Accumulates single values into a mutable @racket[vector?].

 If @racket[expandable-option] is supplied, @racket[to-vector] will copy the existing
 values to a fresh mutable @racket[vector?] each time iteration exceeds its length.
 The size of the new vector is calculated as @racket[(* old-length multiplier-expr)].
 The vector is trimmed to the correct size when iteration concludes.

 When no arguments are supplied, @racket[to-vector] uses the @racket[expandable-option]s
 @racket[#:grow-from 16 #:by 2].
 
 @examples[#:eval example-evaluator
           (for to-vector
             ([x (from-range 5)])
             (* x 2))
           (for (to-vector #:grow-from 1
                           #:by 3)
             ([x (from-range 5)])
             (* x 2))]

 @margin-note{
  Supplying a length via @racket[#:length length-expr] can be more efficient than the
  default behavior. See @seclink["performance:to-vector"]{Performance: to-vector}
  for more information.
 }

 If @racket[fixed-option] is supplied, @racket[to-vector] creates a single mutable
 @racket[vector?] at the beginning of iteration. If iteration exceeds the
 length of the vector, results are silently ignored. The @racket[length-expr] option
 specifies the size of the vector, and @racket[fill-expr] specifies what to place
 in the vector if it is not completely filled by iteration. By default,
 @racket[fill-expr] is @racket[0].

 @examples[#:eval example-evaluator
           (for (to-vector #:length 10)
             ([x (from-range 5)])
             (* x 2))
           (for (to-vector #:length 10 #:fill #f)
             ([x (from-range 5)])
             (* x 2))]
}

@section{Performance}

The performance of @racket[for] depends largely upon the
accumulator and iterators supplied to it. All iterators and accumulators provided from
this package perform the same or faster than their @racketmodname[racket] counterparts.

The majority of code behind the @racket[for] transformer deals with removing
dead/useless forms from its expansion. For instance, @racket[for] will only expand
into code that uses @racket[match] if non-identifier patterns are used. Otherwise,
it uses @racket[let-values] directly.

@examples[#:eval example-evaluator
          (define-namespace-anchor anchor)
          (parameterize ([current-namespace
                          (namespace-anchor->namespace anchor)])
            (displayln "Only identifiers are binded; this expansion doesn't have match:")
            (pretty-print
             (syntax->datum
              (expand-to-top-form
               #'(for ([v (from-list '(1 2 3 4 5))])
                   (display v)))))
            (displayln "But this one, which uses a match pattern, does:")
            (pretty-print
             (syntax->datum
              (expand-to-top-form
               #'(for ([(var v) (from-list '(1 2 3 4 5))])
                   (display v))))))]

@subsection[#:tag "performance:to-list"]{to-list}

The @racket[to-list] accumulator collects items by @racket[cons]ing them together.
Since this strategy produces a list in the opposite order of iteration,
@racket[to-list] @racket[reverse]s the result by default.
If @racket[#:reverse? #f] is supplied, @racket[to-list] does not @racket[reverse] the
result, which improves performance.

@subsection[#:tag "performance:to-vector"]{to-vector}

Supplying @racket[#:length _length-expr] in @racket[to-vector] ensures that only
one vector is ever created. This has the potential to perform faster than the default
behavior of allocating a new vector when iteration exceeds the old vector's length.