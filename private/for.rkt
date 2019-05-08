#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/apply-transformer
                     syntax/parse)
         racket/format
         racket/match
         "accumulator.rkt"
         "iterator.rkt")

(provide (rename-out [ufor for])
         (all-from-out racket/format))

(define-syntax (ufor stx)
  (syntax-parse stx
    [(_ ([pattern:expr ...+ iterator:iterator] ...) body ...+)
     #'(ufor to-void ([pattern ... iterator] ...) body ...)]
    [(_ ([pattern:expr ...+ iterator:iterator]
         ...
         #:when when-expr:expr
         others ...)
        body ...+)
     #'(ufor to-void
             ([pattern ... iterator]
              ...
              #:when when-expr
              others ...)
             body ...)]
    [(_ accumulator:accumulator
        ([pattern:expr ...+ iterator:iterator] ...)
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
                                   (match-let*-values
                                       ([(pattern ...) iterator.match-expr] ...)
                                     body ...)])
                       (if (and accumulator.post-guard iterator.post-guard ...)
                           (loop accumulator.loop-arg ... iterator.loop-arg ... ...)
                           accumulator.done-expr))
                     accumulator.done-expr))
               accumulator.done-expr)))]
    [(_ accumulator:accumulator
        ([pattern:expr ...+ iterator:iterator]
         ...
         #:when when-expr:expr
         [nested-pattern:expr ...+ nested-iterator:iterator]
         ...)
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
                     (match-let*-values
                         ([(pattern ...) iterator.match-expr] ...)
                       (if when-expr
                           (let-syntax
                               ([new-acc
                                 (λ (stx)
                                   #'(()
                                      ()
                                      ([accumulator.loop-id
                                        accumulator.loop-id] ...)
                                      accumulator.pos-guard
                                      ([(accumulator.inner-id ...)
                                        accumulator.inner-expr]
                                       ...)
                                      accumulator.pre-guard
                                      (accumulator.body-result ...)
                                      accumulator.post-guard
                                      (accumulator.loop-arg ...)
                                      accumulator.nested-done-expr
                                      accumulator.nested-done-expr))])
                             (let-values ([(accumulator.loop-id ...)
                                           (ufor new-acc
                                                 ([nested-pattern ... nested-iterator] ...)
                                                 body ...)])
                               (loop accumulator.loop-id ...
                                     iterator.loop-arg ... ...)))
                           (loop accumulator.loop-id ...
                                 iterator.loop-arg ... ...)))
                     (if (and accumulator.post-guard iterator.post-guard ...)
                         (loop accumulator.loop-id ... iterator.loop-arg ... ...)
                         accumulator.done-expr)))
               accumulator.done-expr)))]))
