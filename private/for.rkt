#lang racket/base

(require (for-syntax racket/base
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
    [(_ accumulator:accumulator
        ([(~describe "match pattern" pattern:expr)
          ...+
          (~describe "iterator" iterator:iterator)] ...)
        body ...+)
     (with-syntax*
       ([(accumulator-outer-checks ...)
         (if (equal? #f (syntax-e #'accumulator.outer-check))
             #'()
             #'(accumulator.outer-check))]
        [(iterator-outer-checks ...)
         (foldl (λ (check useful-checks)
                  (if (equal? #f (syntax-e check))
                      useful-checks
                      (cons check useful-checks)))
                '()
                (syntax->list #'(iterator.outer-check ...)))]
        [(accumulator-pos-guards ...)
         (if (equal? #t (syntax-e #'accumulator.pos-guard))
             #'()
             #'(accumulator.pos-guard))]
        [(iterator-pos-guards ...)
         (foldl (λ (check useful-checks)
                  (if (equal? #t (syntax-e check))
                      useful-checks
                      (cons check useful-checks)))
                '()
                (syntax->list #'(iterator.pos-guard ...)))]
        [(accumulator-pre-guards ...)
         (if (equal? #t (syntax-e #'accumulator.pre-guard))
             #'()
             #'(accumulator.pre-guard))]
        [(iterator-pre-guards ...)
         (foldl (λ (check useful-checks)
                  (if (equal? #t (syntax-e check))
                      useful-checks
                      (cons check useful-checks)))
                '()
                (syntax->list #'(iterator.pre-guard ...)))]
        [(accumulator-post-guards ...)
         (if (equal? #t (syntax-e #'accumulator.post-guard))
             #'()
             #'(accumulator.post-guard))]
        [(iterator-post-guards ...)
         (foldl (λ (check useful-checks)
                  (if (equal? #t (syntax-e check))
                      useful-checks
                      (cons check useful-checks)))
                '()
                (syntax->list #'(iterator.post-guard ...)))]
        [match-body
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
           accumulator-outer-checks ...
           iterator-outer-checks ...
           (let loop ([accumulator.loop-id accumulator.loop-expr]
                      ...
                      [iterator.loop-id iterator.loop-expr]
                      ... ...)
             (if (and accumulator-pos-guards ... iterator-pos-guards ...)
                 (let*-values ([(accumulator.inner-id ...) accumulator.inner-expr]
                               ...
                               [(iterator.inner-id ...) iterator.inner-expr]
                               ... ...)
                   (if (and accumulator-pre-guards ... iterator-pre-guards ...)
                       (let-values ([(accumulator.body-result ...) match-body])
                         (if (and accumulator-post-guards ... iterator-post-guards ...)
                             (loop accumulator.loop-arg ... iterator.loop-arg ... ...)
                             accumulator.done-expr))
                       accumulator.done-expr))
                 accumulator.done-expr))))]))
