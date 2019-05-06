#lang racket/base

(require (for-syntax racket/base
                     syntax/apply-transformer
                     syntax/parse)
         racket/match
         "accumulator.rkt"
         "iterator.rkt")

(provide (rename-out [unified-for for]))

(begin-for-syntax
  (define-syntax-class accumulator
    (pattern unexpanded:expr
             #:with (([(outer-id:id ...) outer-expr:expr] ...)
                     ([loop-id:id loop-expr:expr] ...)
                     (body-result:id ...)
                     (loop-arg:expr ...)
                     done-expr:expr)
             (local-expand (if (identifier? #'unexpanded)
                               #'(unexpanded)
                               #'unexpanded)
                           'expression
                           #f))))

(define-syntax (unified-for stx)
  (syntax-parse stx
    [(_ ([pattern:expr ...+ iterator:unexpanded-iterator] ...) body ...+)
     #'(unified-for to-void
                    ([pattern ... iterator] ...)
                    body ...)]
    [(_ accumulator:accumulator
        ([pattern:expr ...+ iterator:unexpanded-iterator] ...)
        body ...+)
     (with-syntax
       ([match-body
         (if (andmap identifier? (syntax->list #'(pattern ... ...)))
             #'(let*-values
                   ([(pattern ...) iterator.match-expr] ...)
                 body ...)
             #'(match-let*-values
                   ([(pattern ...) iterator.match-expr] ...)
                 body ...))])
       #'(let*-values ([(accumulator.outer-id ...) accumulator.outer-expr]
                       ...
                       [(iterator.outer-id ...) iterator.outer-expr]
                       ... ...)
           (let loop ([accumulator.loop-id accumulator.loop-expr]
                      ...
                      [iterator.loop-id iterator.loop-expr]
                      ... ...)
             (if (and iterator.pos-guard ... ...)
                 (let-values ([(accumulator.body-result ...) match-body])
                   (loop accumulator.loop-arg ... iterator.loop-arg ... ...))
                 accumulator.done-expr))))]))
