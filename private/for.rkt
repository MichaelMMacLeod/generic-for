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
                 body ...))]
        [post-guard-form
         (if (and (empty? (syntax->list #'(accumulator-post-guards ...)))
                  (empty? (syntax->list #'(iterator-post-guards ...))))
             #'(loop accumulator.loop-arg ... iterator.loop-arg ... ...)
             #'(if (and accumulator-post-guards ... iterator-post-guards ...)
                   (loop accumulator.loop-arg ... iterator.loop-arg ... ...)
                   accumulator.done-expr))]
        [body-result-form
         (if (empty? (syntax->list #'(accumulator.body-result ...)))
             #'(begin
                 match-body
                 post-guard-form)
             #'(let-values ([(accumulator.body-result ...) match-body])
                 post-guard-form))]
        [pre-guard-form
         (if (and (empty? (syntax->list #'(accumulator-pre-guards ...)))
                  (empty? (syntax->list #'(iterator-pre-guards ...))))
             #'body-result-form
             #'(if (and accumulator-pre-guards ... iterator-pre-guards ...)
                   body-result-form
                   accumulator.done-expr))]
        [pos-guard-form
         (if (and (empty? (syntax->list #'(accumulator-pos-guards ...)))
                  (empty? (syntax->list #'(iterator-pos-guards ...))))
             #'(let*-values ([(accumulator.inner-id ...) accumulator.inner-expr]
                             ...
                             [(iterator.inner-id ...) iterator.inner-expr]
                             ... ...)
                 pre-guard-form)
             #'(if (and accumulator-pos-guards ... iterator-pos-guards ...)
                   (let*-values ([(accumulator.inner-id ...) accumulator.inner-expr]
                                 ...
                                 [(iterator.inner-id ...) iterator.inner-expr]
                                 ... ...)
                     pre-guard-form)
                   accumulator.done-expr))])
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
             pos-guard-form)))]))
