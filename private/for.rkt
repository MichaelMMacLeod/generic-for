#lang racket/base

;;;
;;; for.rkt
;;;
;;; Provides for, the unified-for loop.
;;;

(require (for-syntax racket/base
                     syntax/parse
                     "make-accumulator.rkt"
                     "syntax-classes.rkt")
         racket/match)

(provide (rename-out [unified-for for]))

;; This macro provides a general framework for looping. By itself, unified-for
;; doesn't do much. The interesting aspects of loop functionality come from
;; which iterators and accumulators are used.

(define-syntax (unified-for stx)
  (syntax-parse stx
    [(_ (~datum *)
        a:accumulator
        ([(~describe "match pattern" pattern:expr) ...+ i:iterator])
        body ...+)
     #'(unified-for a ([pattern ... i]) body ...)]
    [(_ (~datum *)
        a:accumulator
        ([(~describe "match pattern" pattern0:expr) ...+ i0:iterator]
         [(~describe "match pattern" pattern:expr) ...+ i:iterator] ...)
        body ...+)
     #`(let*-values ([(a.outside-bindings-id ...) a.outside-bindings-expr]
                     ...
                     [(i0.outside-bindings-id ...) i0.outside-bindings-expr]
                     ...)
         a.outside-checks ... i0.outside-checks ...
         (let loop ([a.loop-bindings-id a.loop-bindings-expr]
                    ...
                    [i0.loop-bindings-id i0.loop-bindings-expr]
                    ...)
           (if (and a.inside-guards ... i0.inside-guards ...)
               (let*-values ([(a.inside-bindings-id ...) a.inside-bindings-expr]
                             ...
                             [(i0.inside-bindings-id ...) i0.inside-bindings-expr]
                             ...)
                 a.inside-checks ... i0.inside-checks ...
                 (if (and a.body-guards ... i0.body-guards ...)
                     (match-let-values
                         ([(pattern0 ...) i0.body-input])
                       (let-syntax
                           ([nested-accumulator
                             (lambda (stx)
                               (syntax-parse stx
                                 [(_)
                                  (make-accumulator
                                   #:outside-bindings
                                   #'()
                                   #:outside-checks
                                   #'()
                                   #:loop-bindings
                                   #'([a.loop-bindings-id a.loop-bindings-id] ...)
                                   #:inside-guards
                                   #'(a.inside-guards ...)
                                   #:return
                                   #'(values a.loop-bindings-id ...)
                                   #:outside-return
                                   #'(values a.loop-bindings-id ...)
                                   #:inside-bindings
                                   #'([(a.inside-bindings-id ...) a.inside-bindings-expr] ...)
                                   #:inside-checks
                                   #'(a.inside-checks ...)
                                   #:body-guards
                                   #'(a.body-guards ...)
                                   #:inside-return
                                   #'(values a.loop-bindings-id ...)
                                   #:body-outputs
                                   #'(a.body-outputs ...)
                                   #:body-bindings
                                   #'([(a.body-bindings-id ...) a.body-bindings-expr] ...)
                                   #:body-checks
                                   #'(a.body-checks ...)
                                   #:recurse-guards
                                   #'(a.recurse-guards ...)
                                   #:body-return
                                   #'(values a.loop-bindings-id ...)
                                   #:recurse-arguments
                                   #'(a.recurse-arguments ...))]))])
                         a.body-checks ... i0.body-checks ...
                         (if (and a.recurse-guards ... i0.recurse-guards ...)
                             (loop (unified-for *
                                                nested-accumulator
                                                ([pattern ... i] ...) body ...)
                                   i0.recurse-arguments ...)
                             a.body-return)))
                     a.inside-return))
               a.outside-return)))]
    [(_ a:accumulator
        ([(~describe "match pattern" pattern:expr) ...+ i:iterator] ...)
        body ...+)
     #`(let*-values ([(a.outside-bindings-id ...) a.outside-bindings-expr]
                     ...
                     [(i.outside-bindings-id ...) i.outside-bindings-expr]
                     ... ...)
         a.outside-checks ... i.outside-checks ... ...
         (let loop ([a.loop-bindings-id a.loop-bindings-expr]
                    ...
                    [i.loop-bindings-id i.loop-bindings-expr]
                    ... ...)
           (if (and a.inside-guards ... i.inside-guards ... ...)
               (let*-values ([(a.inside-bindings-id ...) a.inside-bindings-expr]
                             ...
                             [(i.inside-bindings-id ...) i.inside-bindings-expr]
                             ... ...)
                 a.inside-checks ... i.inside-checks ... ...
                 (if (and a.body-guards ... i.body-guards ... ...)
                     (let-values ([(a.body-outputs ...)
                                   (match-let-values
                                       ([(pattern ...) i.body-input] ...)
                                     body ...)])
                       (let*-values ([(a.body-bindings-id ...) a.body-bindings-expr]
                                     ...
                                     [(i.body-bindings-id ...) i.body-bindings-expr]
                                     ... ...)
                         a.body-checks ... i.body-checks ... ...
                         (if (and a.recurse-guards ... i.recurse-guards ... ...)
                             (loop a.recurse-arguments ... i.recurse-arguments ... ...)
                             a.body-return)))
                     a.inside-return))
               a.outside-return)))]))
