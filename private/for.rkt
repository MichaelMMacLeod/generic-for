#lang racket/base

(require (for-syntax racket/base
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
    [(_ accumulator:accumulator
        ([(~describe "match pattern" pattern:expr)
          ...+
          (~describe "iterator" iterator:iterator)] ...)
        body ...+)
     (with-syntax
       ([match-body
         (if (andmap identifier? (syntax->list #'(pattern ... ...)))
             #'(let*-values
                   ([(pattern ...) iterator.match-expr] ...)
                 body ...)
             #'(match-let*-values
                   ([(pattern ...) iterator.match-expr] ...)
                 body ...))]
        [(accumulator-outer-checks ...)
         (if (equal? #f (syntax-e #'accumulator.outer-check))
             #'()
             #'(accumulator.outer-check))]
        [(iterator-outer-checks ...)
         (foldl (λ (check useful-checks)
                  (if (equal? #f (syntax-e check))
                      useful-checks
                      (cons check useful-checks)))
                '()
                (syntax->list #'(iterator.outer-check ...)))])
       #'(let*-values ([(accumulator.outer-id ...) accumulator.outer-expr]
                       ...
                       [(iterator.outer-id ...) iterator.outer-expr]
                       ... ...)
           accumulator-outer-checks ...
           iterator-outer-checks ...
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
                       (let-values ([(accumulator.body-result ...) match-body])
                         (if (and accumulator.post-guard iterator.post-guard ...)
                             (loop accumulator.loop-arg ... iterator.loop-arg ... ...)
                             accumulator.done-expr))
                       accumulator.done-expr))
                 accumulator.done-expr))))]))
