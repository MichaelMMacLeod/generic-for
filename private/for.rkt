#lang racket/base

(require (for-syntax racket/base
                     syntax/apply-transformer
                     syntax/parse)
         racket/match
         "accumulator.rkt")

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
                           #f)))

  (define-syntax-class iterator
    (pattern unexpanded:expr
             #:with
             (([(outer-id:id ...) outer-expr:expr] ...)
              ([loop-id:id loop-expr:expr] ...)
              (pos-guard:expr ...)
              (match-expr:expr ...)
              (loop-arg:expr ...))
             (local-expand (if (identifier? #'unexpanded)
                               #'(unexpanded)
                               #'unexpanded)
                           'expression
                           #f))))

(define-syntax (unified-for stx)
  (syntax-parse stx
    [(_ ([pattern:expr ...+ iterator:iterator] ...) body ...+)
     #'(unified-for to-void
                    ([pattern ... iterator] ...)
                    body ...)]
    [(_ accumulator:accumulator
        ([id:id ...+ iterator:iterator] ...)
        body ...+)
     #'(let*-values ([(accumulator.outer-id ...) accumulator.outer-expr]
                     ...
                     [(iterator.outer-id ...) iterator.outer-expr]
                     ... ...)
         (let loop ([accumulator.loop-id accumulator.loop-expr]
                    ...
                    [iterator.loop-id iterator.loop-expr]
                    ... ...)
           (if (and iterator.pos-guard ... ...)
               (let-values
                   ([(accumulator.body-result ...)
                     (let*-values
                         ([(id ...) iterator.match-expr ...] ...)
                       body ...)])
                 (loop accumulator.loop-arg ...
                       iterator.loop-arg ... ...))
               accumulator.done-expr)))]
    [(_ accumulator:accumulator
        ([pattern:expr ...+ iterator:iterator] ...)
        body ...+)
     #'(let*-values ([(accumulator.outer-id ...) accumulator.outer-expr]
                     ...
                     [(iterator.outer-id ...) iterator.outer-expr]
                     ... ...)
         (let loop ([accumulator.loop-id accumulator.loop-expr]
                    ...
                    [iterator.loop-id iterator.loop-expr]
                    ... ...)
           (if (and iterator.pos-guard ... ...)
               (let-values
                   ([(accumulator.body-result ...)
                     (match-let*-values
                         ([(pattern ...) iterator.match-expr ...] ...)
                       body ...)])
                 (loop accumulator.loop-arg ...
                       iterator.loop-arg ... ...))
               accumulator.done-expr)))]))
