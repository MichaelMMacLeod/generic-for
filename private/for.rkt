#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/apply-transformer
                     syntax/parse)
         racket/match
         "accumulator.rkt"
         "iterator.rkt")

(provide (rename-out [unified-for for]))

(define-syntax (unified-for stx)
  (syntax-parse stx
    [(_ ([pattern:expr ...+ iterator:iterator] ...) body ...+)
     #'(unified-for to-void
                    ([pattern ... iterator] ...)
                    body ...)]
    [(_ (~optional accumulator:accumulator
                   #:defaults ([accumulator #'to-void]))
        ([(~describe "match pattern" pattern:expr)
          ...+
          iterator:iterator] ...)
        body ...+)
     #'(let*-values ([(accumulator.outer-id ...) accumulator.outer-expr]
                     ...
                     [(iterator.outer-id ...) iterator.outer-expr]
                     ... ...)
         accumulator.outer-check ...
         iterator.outer-check ... ...
         (let loop ([accumulator.loop-id accumulator.loop-expr]
                    ...
                    [iterator.loop-id iterator.loop-expr]
                    ... ...)
           (if (and accumulator.pos-guard iterator.pos-guard ...)
               (let*-values ([(accumulator.inner-id ...) accumulator.inner-expr]
                             ...
                             [(iterator.inner-id ...) iterator.inner-expr]
                             ... ...)
                 (if (and accumulator.pre-guard iterator.pre-guard ...)
                     (let-values ([(accumulator.body-result ...)
                                   (match-let-values
                                       ([(pattern ...) iterator.match-expr] ...)
                                     body ...)])
                       (if (and accumulator.post-guard iterator.post-guard ...)
                           (loop accumulator.loop-arg ... iterator.loop-arg ... ...)
                           accumulator.done-expr))
                     accumulator.done-expr))
               accumulator.done-expr)))]))
