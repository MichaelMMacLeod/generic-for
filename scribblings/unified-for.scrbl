#lang scribble/manual

@(require racket/contract/base
          racket/sandbox
          scribble/examples
          (for-label unified-for
                     racket/contract/base
                     racket/format
                     (except-in racket/base for)))

@title{Unified for Loops}

@author{Michael MacLeod}

@defmodule[unified-for]

This package consolidates the various flavors of @racket[for] iteration
constructs---@racket[for], @racket[for/list], @racket[for/vector],
@racket[for/fold], and so on---into a single
@deftech{unified for} macro that compiles directly to efficient
@seclink["Named_let" #:doc '(lib "scribblings/guide/guide.scrbl")]{named let} code.

@defform/subs[(for maybe-accumulator (for-clause ...) body ...)
              [(maybe-accumulator (code:line)
                                  (accumulator accumulator-arg ...)
                                  accumulator)
               (for-clause [var-id ... (iterator iterator-arg ...)])]]{
}

@section{Accumulators}

@defform/subs[(to-fold [accum-id init-expr] ... maybe-result)
              [(maybe-result (code:line)
                             (#:result result-expr))]
              #:contracts ([init-expr any/c])]{
}

@(define example-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'racket/base
                     #:requires '(unified-for
                                  racket/format))))

@examples[#:eval example-evaluator
          (for (to-fold [sum 0] [rev-roots null])
               ([i (from-list '(1 2 3 4))])
            (values (+ sum i) (cons (sqrt i) rev-roots)))
          (for (to-fold [acc null] [seen (hash)]
                        #:result (reverse acc))
               ([x (from-list '(0 1 1 2 3 4 4 4))])
            (cond [(hash-ref seen x #f)
                   (values acc seen)]
                  [else (values (cons x acc)
                                (hash-set seen x #t))]))]

@section{Iterators}

@defform[(from-list list-expr)]{
}

@examples[#:eval example-evaluator
          (for ([x (from-list '(1 2 3 4 5))])
            (display x))]

@defform[(from-range end-expr)]{
}

@examples[#:eval example-evaluator
          (for ([x (from-range 3)])
            (display x))
          (for ([x y z (from-range 3)])
            (display (~a x y z)))]