#lang racket/base

;;;
;;; for.rkt
;;;
;;; Provides for, the unified-for loop.
;;;

(require (for-syntax racket/base
                     syntax/parse
                     "syntax-classes.rkt")
         racket/match)

(provide (rename-out [unified-for for]))

;; This macro provides a general framework for looping. By itself, unified-for
;; doesn't do much. The interesting aspects of loop functionality come from
;; which iterators and accumulators are used.
(define-syntax (unified-for stx)
  (syntax-parse stx
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
