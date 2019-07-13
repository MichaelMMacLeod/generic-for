#lang scribble/manual

@;@(require racket/contract/base
@;          racket/sandbox
@;          scribble/examples
@;          (for-label (except-in racket/base for)
@;                     racket/contract/base
@;                     racket/format
@;                     racket/list
@;                     racket/math
@;                     racket/match
@;                     syntax/parse
@;                     unified-for
@;                     ))
@;
@;@title{Unified @racket[for] Loop}
@;
@;@author[(author+email "Michael M. MacLeod" "michaelmmacleod@gmail.com")]
@;
@;@defmodule[unified-for]
@;
@;This package consolidates the various
@;@secref["Iteration_and_Comprehension_Forms"
@;         #:doc '(lib "scribblings/reference/reference.scrbl")] into a
@;single @racket[for] macro that compiles directly to efficient
@;@seclink["Named_let" #:doc '(lib "scribblings/guide/guide.scrbl")]{named let}
@;code. It also allows identifiers to be bound via
@;@seclink["match" #:doc '(lib "scribblings/reference/reference.scrbl")]{match} patterns.
@;
@;Warning: this package is experimental and subject to breaking changes.
@;
@;@(define example-evaluator
@;   (parameterize ([sandbox-output 'string]
@;                  [sandbox-error-output 'string])
@;     (make-evaluator 'racket/base
@;                     #:requires '(unified-for
@;                                  (for-syntax racket)
@;                                  racket/list
@;                                  racket/pretty
@;                                  racket/format))))
@;
@;@defform/subs[(for maybe-accumulator (loop-clause ...) body ...+)
@;              [(maybe-accumulator (code:line)
@;                                  accumulator
@;                                  (accumulator arg-form ...))
@;               (loop-clause [match-pattern ...+ iterator-clause])
@;               (iterator-clause (code:line iterator)
@;                                (iterator arg-form ...))]]{
@; Iteratively binds @racket[match-pattern]s with @racket[iterator]s, evaluates
@; @racket[body]s, and collects the results with the @racket[accumulator].
@; An @tech{accumulator} or @tech{iterator} with no subforms can be
@; supplied without parentheses. The default @tech{accumulator} is @racket[to-void].
@; 
@; All identifiers are bound via
@; @seclink["match" #:doc '(lib "scribblings/reference/reference.scrbl")]{match}
@; patterns.
@; Each pattern must successfully match,
@; otherwise a @racket[exn:misc:match?] exception is thrown.
@;
@; @examples[#:eval example-evaluator
@;           (for (to-fold [evens '()]
@;                         [odds '()])
@;                ([x (from-range 10)])
@;             (if (even? x)
@;                 (values (cons x evens) odds)
@;                 (values evens (cons x odds))))
@;           (for ([key value (from-hash #hash((a . 0) (b . 1) (c . 2)))])
@;             (displayln (~a key ": " value)))
@;           (for (to-vector #:length 3)
@;                ([(cons (? symbol? _)
@;                        (app real-part y))
@;                  (from-list '((k1 . 1+2i) (k2 . 2+3i) (k3 . 3+4i)))])
@;             y)]  
@;}
@;
@;@section{Iterators}
@;
@;An @deftech{iterator} is a
@;@seclink["stxtrans"
@;         #:doc '(lib "scribblings/reference/reference.scrbl")]{Syntax Transformer}
@;for use in the @racket[_iterator-clause] of @racket[for]. See
@;@secref{extending-for} on deriving new @tech{iterator}s.
@;
@;@defform[(from-list lst)
@;         #:contracts ([lst list?])]{
@; Iterates over a @racket[list?]. Similar to @racket[in-list], except that
@; @racket[from-list] is legal only within @racket[for].
@;
@; @examples[#:eval example-evaluator
@;           (for ([x (from-list '(1 2 3 4 5))])
@;             (display x))]
@;}
@;
@;@defform[(from-vector vect)
@;         #:contracts ([vect vector?])]{
@; Iterates over a @racket[vector?]. Similar to @racket[in-vector], except that
@; @racket[from-vector] is legal only within @racket[for].
@;
@; @examples[#:eval example-evaluator
@;           (for ([x (from-vector #(1 2 3 4 5))])
@;             (display x))]
@;}
@;
@;@defform/subs[(from-range option)
@;              [(option end-expr
@;                       (code:line start-expr end-expr)
@;                       (code:line start-expr end-expr step-expr))]
@;              #:contracts ([end-expr real?]
@;                           [start-expr real?]
@;                           [end-expr real?])]{
@; Iterates over a range of @racket[real?] values from @racket[start] (inclusive) until
@; @racket[end] (exclusive) by @racket[step]. Similar to @racket[in-range], except that
@; @racket[from-range] is legal only within @racket[for].
@;
@; If @racket[start-expr] or
@; @racket[step-expr] are not provided, they are @racket[0] and @racket[1] respectively.
@;
@; @examples[#:eval example-evaluator
@;           (for ([x (from-range 5)])
@;             (display x))
@;           (for ([x (from-range 5 10)])
@;             (display x))
@;           (for ([x (from-range 10 0 -2)])
@;             (display x))]
@;}
@;
@;@defform/subs[(from-naturals maybe-start)
@;              [(maybe-start (code:line)
@;                            start-expr)]
@;              #:contracts ([maybe-start exact-nonnegative-integer?])]{
@; Iterates forever over @racket[natural?] numbers beginning with @racket[start], or
@; @racket[0] if @racket[start] is not supplied. Similar to @racket[in-naturals],
@; except that @racket[from-naturals] is legal only within @racket[for].
@;
@; @examples[#:eval example-evaluator
@;           (for ([index from-naturals]
@;                 [v (from-list '(a b c d e f g))])
@;             (display (cons index v)))
@;           (for ([index+1 (from-naturals 1)]
@;                 [v (from-list '(a b c d e f g))])
@;             (display (cons index+1 v)))]
@;}
@;
@;@defform[(from-hash hash-expr)
@;         #:contracts ([hash-expr hash?])]{
@; Iterates over the keys and values of a @racket[hash?]. Similar to @racket[in-hash],
@; except that @racket[from-hash] is legal only within @racket[for]. Note that unlike
@; @seclink["Iteration_and_Comprehension_Forms"
@;          #:doc '(lib "scribblings/reference/reference.scrbl")]{for}
@; from @racket[racket/base], there must be no parentheses around the key and value
@; @racket[_match-pattern]s.
@;
@; @examples[#:eval example-evaluator
@;           (for ([key value (from-hash #hash((a . 1) (b . 2) (c . 3)))])
@;             (display (cons key value)))]
@;}
@;
@;@section{Accumulators}
@;
@;An @deftech{accumulator} is a
@;@seclink["stxtrans" #:doc '(lib "scribblings/reference/reference.scrbl")]{syntax transformer}
@;for use in the @racket[_maybe-accumulator] clause of @racket[for]. See
@;@secref{extending-for} on deriving new @tech{accumulator}s.
@;
@;@defform[(to-void)]{
@; Returns @|void-const|. Similar to
@; @seclink["Iteration_and_Comprehension_Forms"
@;          #:doc '(lib "scribblings/reference/reference.scrbl")]{for}.
@; The result of the @racket[for]'s @racket[_body] clause is
@; ignored. It is the default @tech{accumulator} when none is supplied to @racket[for].
@;
@; @examples[#:eval example-evaluator
@;           (for to-void
@;                ([x (from-range 5)]
@;                 [y (from-range 4 0 -1)])
@;             (define x+y (+ x y))
@;             (display x+y)
@;             x+y)
@;           (for ([x (from-range 5)]
@;                 [y (from-range 4 0 -1)])
@;             (define x+y (+ x y))
@;             (display x+y)
@;             x+y)]
@;}
@;
@;@defform/subs[(to-list maybe-reverse?)
@;              [(maybe-reverse? (code:line)
@;                               (code:line #:reverse? reverse?-expr))]
@;              #:contracts ([reverse?-expr boolean?])]{
@; Accumulates elements into a @racket[list?]. Similar to @racket[for/list].
@;
@; If @racket[#:reverse?] is not provided, or @racket[reverse?-expr] evaluates to
@; @racket[#t], @racket[to-list] accumulates items like @racket[for/list]. Otherwise,
@; @racket[to-list] returns items in the opposite order.
@;
@; @margin-note{
@;  The @racket[to-list] @tech{accumulator} normally collects elements in reverse order
@;  by @racket[cons]ing them together, then applying @racket[reverse] to
@;  the result. With @racket[#:reverse? #f], @racket[to-list] does not @racket[reverse]
@;  the result. This can give better performance.
@; }
@; 
@; @examples[#:eval example-evaluator
@;           (for to-list
@;                ([x (from-range 5)])
@;             (* x 2))
@;           (for (to-list #:reverse? #f)
@;                ([x (from-range 5)])
@;             (* x 2))]
@;}
@;
@;@defform/subs[(to-vector length-option)
@;              [(length-option (code:line)
@;                              expandable-option
@;                              fixed-option)
@;               (expandable-option (code:line #:grow-from initial-capacity-expr)
@;                                  (code:line #:grow-from initial-capacity-expr #:by multiplier-expr))
@;               (fixed-option (code:line #:length length-expr)
@;                             (code:line #:length length-expr #:fill fill-expr))]
@;              #:contracts ([initial-capacity-expr exact-positive-integer?]
@;                           [length-expr exact-nonnegative-integer?]
@;                           [fill-expr any/c]
@;                           [multiplier-expr (and/c exact-integer? (>/c 1))])]{
@; Accumulates elements into a mutable @racket[vector?]. Similar to @racket[for/vector].
@;
@; If @racket[expandable-option] is supplied, @racket[to-vector] will copy the existing
@; values to a fresh mutable @racket[vector?] each time iteration exceeds its length.
@; The size of the new vector is calculated as @racket[(* old-length multiplier-expr)].
@; The vector is trimmed to the correct size when iteration concludes.
@;
@; When no arguments are supplied, @racket[to-vector] uses the @racket[expandable-option]s
@; @racket[#:grow-from 16 #:by 2].
@; 
@; @examples[#:eval example-evaluator
@;           (for to-vector
@;                ([x (from-range 5)])
@;             (* x 2))
@;           (for (to-vector #:grow-from 1
@;                           #:by 3)
@;                ([x (from-range 5)])
@;             (* x 2))]
@;
@; If @racket[fixed-option] is supplied, @racket[to-vector] creates a single mutable
@; @racket[vector?]. Iteration is stopped as soon as the
@; vector is completely filled. The @racket[length-expr] option
@; specifies the size of the vector, and @racket[fill-expr] specifies what to place
@; in the vector if it is not completely filled by iteration. By default,
@; @racket[fill-expr] is @racket[0].
@;
@; @margin-note{
@;  Supplying a length via @racket[#:length length-expr] can be more efficient than the
@;  default behavior, since the accumulator will only ever create one vector.
@; }
@;
@; @examples[#:eval example-evaluator
@;           (for (to-vector #:length 10)
@;                ([x (from-range 5)])
@;             (* x 2))
@;           (for (to-vector #:length 10 #:fill #f)
@;                ([x (from-range 5)])
@;             (* x 2))
@;           (for (to-vector #:length 5)
@;                ([x (from-range 10)])
@;             (display x)
@;             x)]
@;}
@;
@;@defform/subs[(to-fold [arg-id init-expr] ... maybe-result)
@;              [(maybe-result (code:line)
@;                             #:result result-form)]
@;              #:contracts ([init-expr any/c])]{
@; Accumulates elements into any number of @racket[arg-id]s. Similar to @racket[for/fold].
@;
@; The @racket[init-expr]s are evaluated and bound to @racket[arg-id]s in the
@; @racket[_body] forms of the @racket[for] loop. The body of the @racket[for] loop
@; must evaluate to as many @racket[values] as there are @racket[arg-id]s. These
@; @racket[values] are then bound to each @racket[arg-id] in the next iteration.
@;
@; If @racket[result-form] is supplied, it is evaluated at the end of iteration and
@; its result returned. By default, @racket[result-form] is @racket[(values arg-id ...)].
@;
@; @examples[#:eval example-evaluator
@;           (for (to-fold [sum 0]
@;                         #:result (* 2 sum))
@;                ([n (from-range 10)])
@;             (+ sum n))
@;           (for (to-fold [real-parts '()]
@;                         [imag-parts '()])
@;                ([c (from-list '(1+1i 2+5i 4+2i 9+5i))])
@;             (values (cons (real-part c) real-parts)
@;                     (cons (imag-part c) imag-parts)))]
@;                         
@;}
@;
@;@section[#:tag "extending-for"]{Extending for}
@;
@;Creating a new @tech{iterator} or @tech{accumulator} involves using
@;@racket[define-syntax] to make a
@;@seclink["stxtrans"
@;         #:doc '(lib "scribblings/reference/reference.scrbl")]{Syntax Transformer}
@;that expands into a @racket[syntax] list. It is similar to the process of
@;using @racket[:do-in] to extend the traditional
@;@seclink["Iteration_and_Comprehension_Forms"
@;         #:doc '(lib "scribblings/reference/reference.scrbl")]{for}
@;loop. The @racket[for] macro @racket[local-expand]s each @tech{iterator} and
@;@tech{accumulator} and splices their results into its own expansion.
@;
@;@racketgrammar*[[iterator
@;                 (([(outer-id ...) outer-expr] ...)
@;                  (outer-check-expr ...)
@;                  ([loop-id loop-expr] ...)
@;                  pos-guard-expr
@;                  ([(inner-id ...) inner-expr] ...)
@;                  pre-guard-expr
@;                  match-expr
@;                  post-guard-expr
@;                  (loop-arg-expr ...))]
@;                [accumulator
@;                 (([(outer-id ...) outer-expr] ...)
@;                  (outer-check ...)
@;                  ([loop-id loop-expr] ...)
@;                  pos-guard-expr
@;                  ([(inner-id ...) inner-expr] ...)
@;                  pre-guard-expr
@;                  (body-result-id ...)
@;                  post-guard-expr
@;                  (loop-arg-expr ...)
@;                  done-expr)]]
@;
@;Both @tech{accumulator}s and @tech{iterator}s expand to similar forms. The first
@;element, @racket[([(_outer-id ...) _outer-expr] ...)] specifies identifiers and
@;expressions to be bound via @racket[let*-values] outside the loop. This is useful
@;when the @tech{iterator} or @tech{accumulator} needs to evaluate an expression only
@;once. For example, @racket[to-vector] with the @racket[#:fill] option creates its
@;@racket[vector?] here.
@;
@;The second element, @racket[(_outer-check-expr ...)], specifies a list of expressions which
@;are evaluated for their side effects, after @racket[_outer-id]s are bound, and before
@;the loop begins. This is useful for checking that all
@;sub-forms of the @tech{iterator} or @tech{accumulator} are of valid types. For
@;example, @racket[from-range] uses this space to throw an exception if its
@;sub-forms do not evaluate to @racket[real?] numbers.
@;
@;Next is @racket[([_loop-id _loop-expr] ...)]. These identifiers are bound to their
@;expressions at the start of the loop, once all @racket[_outer-check]s have been
@;evaluated. Later, during the iteration of the @racket[for] form, they are bound to
@;the result of evaluating the @racket[_loop-arg]s. For example, the @racket[from-list]
@;@tech{iterator} binds
@;@racket[_loop-id] to the @racket[list?] being iterated over. The @racket[to-fold]
@;@tech{accumulator} uses these bindings to keep track of its @racket[_arg-id]s and
@;their bindings.
@;
@;The @racket[_pos-guard-expr] form is evaluated once at the beginning of each iteration
@;of the loop. If it produces a @racket[#t] value, the loop continues. Otherwise,
@;iteration ends immediately, and the @tech{accumulator}'s @racket[_done-expr] is
@;returned. This form is useful for checking whether the sequence
@;being iterated over is empty or not. For example, @racket[from-vector] uses this
@;space to ensure that the current index in the vector, which it bound as a
@;@racket[_loop-arg] is less than its length. The @racket[from-naturals]
@;@tech{iterator} expands here to @racket[#t] its iteration is infinite.
@;
@;After each @racket[_pos-guard-expr] is checked, @racket[([(_inner-id ...) _inner-expr] ...)]
@;is bound via @racket[let*-values]. This is useful for creating bindings that
@;differ on each iteration, and happen before the evaluation of @racket[for]'s
@;@racket[_body]s.
@;
@;After @racket[_inner-id]s are bound, the @racket[_pre-guard-expr] is evaluated.
@;If it produces a @racket[#t] value, the loop continues. Otherwise, iteration ends
@;immediately and the @tech{accumulator}'s @racket[_done-expr] is
@;returned. This can be useful for ending iteration based off of a value
@;bound to an @racket[_inner-id].
@;
@;The next form is different for @tech{iterator}s and @tech{accumulator}s. For
@;@tech{iterator}s, it is @racket[_match-expr], and it specifies what expression to
@;match against @racket[for]'s @racket[_match-pattern]s. For example,
@;@racket[from-hash]'s @racket[_match-expr] evaluates to two @racket[values],
@;the current key and value of the @racket[hash?] being iterated over. For
@;@tech{accumulator}s, this form is @racket[(_body-result-id ...)]. It specifies
@;the identifiers to bind via @racket[let-values] to the result of @racket[for]'s
@;@racket[_body]s. The @racket[to-list] @tech{accumulator} supplies one identifier
@;here, which it @racket[cons]es onto its @racket[_loop-id] in its @racket[_loop-arg-expr].
@;
@;Both @tech{iterator}s and @tech{accumulator}s then have a @racket[_post-guard-expr].
@;If @racket[_post-guard-expr] evaluates to a @racket[#t] value, the loop continues.
@;Otherwise, iteration ends immediately and the @tech{accumulator}'s @racket[_done-expr]
@;is returned. This can be useful for ending iteration
@;based off of a value bound to a @racket[_body-result-id] in the case of
@;@tech{accumulator}s, or a side effect of @racket[for]'s @racket[_body]s, in the case
@;of @tech{iterator}s.
@;
@;The @racket[(_loop-arg-expr ...)] form is then evaluated, and its result is
@;bound to each @racket[_loop-id]s on the next iteration. An @tech{iterator}, like
@;@racket[from-vector], uses this form to step to the next element in the
@;sequence, usually by adding @racket[1] to an index, or using a @racket[cdr]-like
@;operation. An @tech{accumulator}, like @racket[to-list], uses this form to add
@;an element to its collection, usually the one bound by @racket[_body-result-id].
@;
@;Each @tech{accumulator} must specify one more form, @racket[_done-expr], which
@;is evaluated and returned whenever any @racket[_pos-guard-expr],
@;@racket[_pre-guard-expr], or @racket[_post-gurd-expr] returns a @racket[#f] value.
@;For example, @racket[to-list] with @racket[#:reverse? #t] uses this space to
@;@racket[reverse] the accumulated list bound to its @racket[_loop-id].
@;
@;Here is the full expansion of a @racket[for] form, with one @tech{accumulator}
@;bound to @racket[a], and any number of @tech{iterator}s bound to @racket[(i ...)].
@;
@;@(racketblock
@;  (let*-values ([(a.outer-id ...) a.outer-expr] ...
@;                [(i.outer-id ...) i.outer-expr] ... ...)
@;    a.outer-check-expr ...
@;    i.outer-check-expr ... ...
@;    (let loop ([a.loop-id a.loop-expr] ...
@;               [i.loop-id i.loop-expr] ... ...)
@;      (if (and a.pos-guard-expr i.pos-guard-expr ...)
@;          (let*-values ([(a.inner-id ...) a.inner-expr] ...
@;                        [(i.inner-id ...) i.inner-expr] ... ...)
@;            (if (and a.pre-guard-expr i.pre-guard-expr ...)
@;                (let-values ([(a.body-result-id ...)
@;                              (match-let-values
@;                                  ([(pattern ...) i.match-expr] ...)
@;                               body ...)])
@;                  (if (and a.post-guard-expr i.post-guard-expr ...)
@;                      (loop a.loop-arg-expr ... i.loop-arg-expr ... ...)
@;                      a.done-expr))
@;                a.done-expr))
@;          a.done-expr))))
@;
@;@examples[#:escape get-me-out-of-here
@;          (require (for-syntax racket/base syntax/parse)
@;                   (prefix-in u: unified-for))
@;          (define-syntax (from-vector stx)
@;            (syntax-parse stx
@;              [(_ v:expr)
@;               #`(([(vect) v]
@;                   [(len)
@;                    #,(syntax/loc #'v
@;                        (vector-length vect))])
@;                  ()
@;                  ([pos 0])
@;                  (< pos len)
@;                  ()
@;                  #t
@;                  (vector-ref vect pos)
@;                  #t
@;                  ((add1 pos)))]))
@;          (u:for ([x (from-vector #(0 1 2 3 4 5))])
@;            (display x))
@;          (eval:error (u:for ([x (from-vector 'not-a-vector)])
@;                        (display x)))
@;          (define-syntax (to-fold stx)
@;            (syntax-parse stx
@;              #:track-literals
@;              [(_ [arg:id val:expr] ...+)
@;               #'(to-fold [arg val] ... #:result (values arg ...))]
@;              [(_ [arg:id val:expr] ...+ #:result result:expr)
@;               (with-syntax ([(last-body ...) (generate-temporaries #'([arg val] ...))])
@;                 #'(()
@;                    ()
@;                    ([arg val] ...)
@;                    #t
@;                    ()
@;                    #t
@;                    (last-body ...)
@;                    #t
@;                    (last-body ...)
@;                    result))]))
@;          (u:for (to-fold [factorial 1])
@;                 ([x (u:from-range 1 10)])
@;            (* factorial x))
@;          (define-syntax (to-void stx)
@;            (syntax-parse stx
@;              [(_)
@;               #'(() () () #t () #t (_) #t () (void))]))
@;          (u:for to-void
@;                 ([x (u:from-range 5)])
@;            (display x))]
