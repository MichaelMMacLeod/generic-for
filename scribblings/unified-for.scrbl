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

@author[(author+email "Michael M. MacLeod" "michaelmmacleod@gmail.com")]

@defmodule[unified-for]

This package consolidates the various
@secref["Iteration_and_Comprehension_Forms"
         #:doc '(lib "scribblings/reference/reference.scrbl")] into a
single @racket[for] macro that compiles directly to efficient
@seclink["Named_let" #:doc '(lib "scribblings/guide/guide.scrbl")]{named let}
code. It also allows identifiers to be bound via
@seclink["match" #:doc '(lib "scribblings/reference/reference.scrbl")]{match} patterns.

Warning: this package is experimental and subject to breaking changes.

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
                                  accumulator
                                  (accumulator arg-form ...))
               (loop-clause [match-pattern ...+ iterator-clause])
               (iterator-clause (code:line iterator)
                                (iterator arg-form ...))]]{
 Iteratively binds @racket[match-pattern]s with @racket[iterator]s, evaluates
 @racket[body]s, and collects the results with the @racket[accumulator].
 An @tech{accumulator} or @tech{iterator} with no subforms can be
 supplied without parentheses. The default @tech{accumulator} is @racket[to-void].
 
 All identifiers are bound via
 @seclink["match" #:doc '(lib "scribblings/reference/reference.scrbl")]{match}
 patterns.
 Each pattern must successfully match,
 otherwise a @racket[exn:misc:match?] exception is thrown.

 @examples[#:eval example-evaluator
           (for (to-fold [evens '()]
                         [odds '()])
                ([x (from-range 10)])
             (if (even? x)
                 (values (cons x evens) odds)
                 (values evens (cons x odds))))
           (for ([key value (from-hash #hash((a . 0) (b . 1) (c . 2)))])
             (displayln (~a key ": " value)))
           (for (to-vector #:length 3)
                ([(cons (? symbol? _)
                        (app real-part y))
                  (from-list '((k1 . 1+2i) (k2 . 2+3i) (k3 . 3+4i)))])
             y)]  
}

@section{Iterators}

An @deftech{iterator} is a
@seclink["stxtrans"
         #:doc '(lib "scribblings/reference/reference.scrbl")]{Syntax Transformer}
for use in the @racket[_iterator-clause] of @racket[for]. See
@secref{extending-for} on deriving new @tech{iterator}s.

@defform[(from-list lst)
         #:contracts ([lst list?])]{
 Iterates over a @racket[list?]. Similar to @racket[in-list], except that
 @racket[from-list] is legal only within @racket[for].

 @examples[#:eval example-evaluator
           (for ([x (from-list '(1 2 3 4 5))])
             (display x))]
}

@defform[(from-vector vect)
         #:contracts ([vect vector?])]{
 Iterates over a @racket[vector?]. Similar to @racket[in-vector], except that
 @racket[from-vector] is legal only within @racket[for].

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
 @racket[end] (exclusive) by @racket[step]. Similar to @racket[in-range], except that
 @racket[from-range] is legal only within @racket[for].

 If @racket[start-expr] or
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
 @racket[0] if @racket[start] is not supplied. Similar to @racket[in-naturals],
 except that @racket[from-naturals] is legal only within @racket[for].

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
 Iterates over the keys and values of a @racket[hash?]. Similar to @racket[in-hash],
 except that @racket[from-hash] is legal only within @racket[for]. Note that unlike
 @seclink["Iteration_and_Comprehension_Forms"
          #:doc '(lib "scribblings/reference/reference.scrbl")]{for}
 from @racket[racket/base], there must be no parentheses around the key and value
 @racket[_match-pattern]s.

 @examples[#:eval example-evaluator
           (for ([key value (from-hash #hash((a . 1) (b . 2) (c . 3)))])
             (display (cons key value)))]
}

@section{Accumulators}

An @deftech{accumulator} is a
@seclink["stxtrans" #:doc '(lib "scribblings/reference/reference.scrbl")]{syntax transformer}
for use in the @racket[_maybe-accumulator] clause of @racket[for]. See
@secref{extending-for} on deriving new @tech{accumulator}s.

@defform[(to-void)]{
 Returns @|void-const|. Similar to
 @seclink["Iteration_and_Comprehension_Forms"
          #:doc '(lib "scribblings/reference/reference.scrbl")]{for}.
 The result of the @racket[for]'s @racket[_body] clause is
 ignored. It is the default @tech{accumulator} when none is supplied to @racket[for].

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
 Accumulates elements into a @racket[list?]. Similar to @racket[for/list].

 If @racket[#:reverse?] is not provided, or @racket[reverse?-expr] evaluates to
 @racket[#t], @racket[to-list] accumulates items like @racket[for/list]. Otherwise,
 @racket[to-list] returns items in the opposite order.

 @margin-note{
  The @racket[to-list] @tech{accumulator} normally collects elements in reverse order
  by @racket[cons]ing them together, then applying @racket[reverse] to
  the result. With @racket[#:reverse? #f], @racket[to-list] does not @racket[reverse]
  the result. This can give better performance.
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
 Accumulates elements into a mutable @racket[vector?]. Similar to @racket[for/vector].

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

 If @racket[fixed-option] is supplied, @racket[to-vector] creates a single mutable
 @racket[vector?]. Iteration is stopped as soon as the
 vector is completely filled. The @racket[length-expr] option
 specifies the size of the vector, and @racket[fill-expr] specifies what to place
 in the vector if it is not completely filled by iteration. By default,
 @racket[fill-expr] is @racket[0].

 @margin-note{
  Supplying a length via @racket[#:length length-expr] can be more efficient than the
  default behavior, since the accumulator will only ever create one vector.
 }

 @examples[#:eval example-evaluator
           (for (to-vector #:length 10)
                ([x (from-range 5)])
             (* x 2))
           (for (to-vector #:length 10 #:fill #f)
                ([x (from-range 5)])
             (* x 2))
           (for (to-vector #:length 5)
                ([x (from-range 10)])
             (display x)
             x)]
}

@defform/subs[(to-fold [arg-id init-expr] ... maybe-result)
              [(maybe-result (code:line)
                             #:result result-form)]
              #:contracts ([init-expr any/c])]{
 Accumulates elements into any number of @racket[arg-id]s. Similar to @racket[for/fold].

 The @racket[init-expr]s are evaluated and bound to @racket[arg-id]s in the
 @racket[_body] forms of the @racket[for] loop. The body of the @racket[for] loop
 must evaluate to as many @racket[values] as there are @racket[arg-id]s. These
 @racket[values] are then bound to each @racket[arg-id] in the next iteration.

 If @racket[result-form] is supplied, it is evaluated at the end of iteration and
 its result returned. By default, @racket[result-form] is @racket[(values arg-id ...)].

 @examples[#:eval example-evaluator
           (for (to-fold [sum 0]
                         #:result (* 2 sum))
                ([n (from-range 10)])
             (+ sum n))
           (for (to-fold [real-parts '()]
                         [imag-parts '()])
                ([c (from-list '(1+1i 2+5i 4+2i 9+5i))])
             (values (cons (real-part c) real-parts)
                     (cons (imag-part c) imag-parts)))]
                         
}

@section[#:tag "extending-for"]{Extending for}