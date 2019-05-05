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

@(define example-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'racket/base
                     #:requires '(unified-for
                                  racket/format))))

@defform/subs[(for maybe-accumulator (for-clause ...) body ...)
              [(maybe-accumulator (code:line)
                                  (accumulator accumulator-arg ...)
                                  accumulator)
               (for-clause [var-id ... (iterator iterator-arg ...)]
                           (iterator var-id ... iterator-arg ...))]]{
}

@examples[#:eval example-evaluator
          (for ([x (from-range 9)])
            (display x))
          (for ([(from-range x 9)])
            (display x))]

@section{Accumulators}

@defform/subs[(to-fold [accum-id init-expr] ... maybe-result)
              [(maybe-result (code:line)
                             (#:result result-expr))]
              #:contracts ([init-expr any/c])]{
}

@(for to-list
      ([k (? even? v) (from-hash #hash((k1 . 0) (k2 . 1) (k3 . 2)))]
       [i (from-range 10)])
   (define c (+ i v))
   (format "~a: c" k c))

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

@defform[(from-list arg-id list-expr)]{
}

@examples[#:eval example-evaluator
          (for ([x (from-list '(1 2 3 4 5))])
            (display x))]

@defform[(from-range arg-id ... end-expr)]{
}

@examples[#:eval example-evaluator
          (for ([x (from-range 3)])
            (display x))
          (for ([x y z (from-range 3)])
            (display (~a x y z)))]