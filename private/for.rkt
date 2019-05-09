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

(provide (rename-out [ufor-normalize-0 for])
         (all-from-out racket/format))

;;; The `ufor-normalize-0` and `ufor-normalize-1` transformers insert all optional forms
;;; not supplied by the user, so `ufor` doesn't have to deal with it.
;;; The `ufor` transformer receives the normalized forms and does the actual heavy lifting.

;; Splices in `to-void` if no accumulator is supplied, then gives the result to
;; `ufor-normalize-1` to expand.
(define-syntax (ufor-normalize-0 stx)
  (syntax-parse stx
    [(_ (~optional accumulator:accumulator
                   #:defaults ([accumulator #'to-void]))
        (clause ...)
        body ...+)
     #'(ufor-normalize-1 accumulator (clause ...) body ...)]))

;; Splices in `#:when #t` from the form received from `ufor-normalize-0` after all iterator
;; clauses if the last iterator clause is not a `#:when` clause, then gives the result to
;; `ufor` to expand.
(define-syntax (ufor-normalize-1 stx)
  (syntax-parse stx
    ;; Matches when there are no `#:when` clauses.
    [(_ accumulator:accumulator
        (clause:expr ...)
        body ...+)
     #'(ufor accumulator (clause ... #:when #t) body ...)]

    ;; Matches when the last iterator clause is a `#:when` clause, and returns the whole form.
    [(_ accumulator:accumulator (clause:expr ... #:when when-expr:expr) body ...+)
     #'(ufor accumulator (clause ... #:when when-expr) body ...)]

    ;; Matches when there is at least one `#:when` clause not appearing as the last iterator
    ;; clause, then splices in #:when #t as the last clause.
    [(_ accumulator:accumulator
        (clause:expr
         ...
         #:when when-expr:expr
         other
         ...
         other-expr:expr)
        body ...+)
     #'(ufor accumulator
             (clause ... #:when when-expr other ... other-expr #:when #t)
             body ...)]))

;; Expands the normalized form received from `ufor-normalize-1`. If there is a `#:when` clause
;; in-between other iterator clauses, recursively expands until a non-nested form is reached.
(define-syntax (ufor stx)
  (syntax-parse stx
    ;; Non-nested version, where the only `#:when` clause appears at the end of the iterator
    ;; clauses.
    [(_ accumulator:accumulator
        ([pattern:expr ...+ iterator:iterator]
         ...
         #:when when-expr:expr)
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
                     (if when-expr
                         (let-values ([(accumulator.body-result ...)
                                       (match-let*-values
                                           ([(pattern ...) iterator.match-expr] ...)
                                         body ...)])
                           (if (and accumulator.post-guard iterator.post-guard ...)
                               (loop accumulator.loop-arg ... iterator.loop-arg ... ...)
                               accumulator.done-expr))
                         accumulator.done-expr)
                     accumulator.done-expr))
               accumulator.done-expr)))]

    ;; Nested version, where there is a `#:when` clause in-between other iterator clauses.
    ;; The accumulator is modified in the nested expansion so that it does not do redundant
    ;; tests, and so that it returns accumulator.nested-done-expr instead of
    ;; accumulator.done-expr.
    [(_ accumulator:accumulator
        ([pattern:expr ...+ iterator:iterator]
         ...
         #:when when-expr:expr
         nested-clauses
         ...+
         #:when final-when-expr:expr)
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
                               ([nested-accumulator
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
                                           (ufor nested-accumulator
                                                 (nested-clauses
                                                  ...
                                                  #:when final-when-expr)
                                                 body ...)])
                               (loop accumulator.loop-id ...
                                     iterator.loop-arg ... ...)))
                           (loop accumulator.loop-id ...
                                 iterator.loop-arg ... ...)))
                     (if (and accumulator.post-guard iterator.post-guard ...)
                         (loop accumulator.loop-id ... iterator.loop-arg ... ...)
                         accumulator.done-expr)))
               accumulator.done-expr)))]))
