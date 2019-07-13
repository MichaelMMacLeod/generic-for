#lang racket/base

(require "accumulator.rkt"
         "iterator.rkt"
         "for.rkt"
         (for-syntax racket/base syntax/parse racket/syntax))

(provide (all-defined-out)
         (all-from-out "accumulator.rkt"))

(begin-for-syntax
  (define-splicing-syntax-class outer-bindings-directive
    #:attributes (outer-bindings)
    (pattern (~optional (~seq #:outer-bindings outer-bindings)
                        #:defaults ([outer-bindings #'()]))))

  (define-splicing-syntax-class outer-checks-directive
    #:attributes (outer-checks)
    (pattern (~optional (~seq #:outer-checks outer-checks)
                        #:defaults ([outer-checks #'()]))))

  (define-splicing-syntax-class initial-arguments-directive
    #:attributes (initial-arguments)
    (pattern (~optional (~seq #:initial-arguments initial-arguments)
                        #:defaults ([initial-arguments #'()]))))

  (define-splicing-syntax-class pos-guard-directive
    #:attributes (pos-guard)
    (pattern (~optional (~seq #:pos-guard pos-guard)
                        #:defaults ([pos-guard #'#t]))))

  (define-splicing-syntax-class pos-done-directive
    #:attributes (pos-done)
    (pattern (~optional (~seq #:pos-done pos-done))))

  (define-splicing-syntax-class inner-bindings-directive
    #:attributes (inner-bindings)
    (pattern (~optional (~seq #:inner-bindings inner-bindings)
                        #:defaults ([inner-bindings #'()]))))

  (define-splicing-syntax-class pre-guard-directive
    #:attributes (pre-guard)
    (pattern (~optional (~seq #:pre-guard pre-guard)
                        #:defaults ([pre-guard #'#t]))))

  (define-splicing-syntax-class pre-done-directive
    #:attributes (pre-done)
    (pattern (~optional (~seq #:pre-done pre-done))))

  (define-splicing-syntax-class body-results-directive
    #:attributes (body-results)
    (pattern (~optional (~seq #:body-results body-results)
                        #:defaults ([body-results #'(_)]))))

  (define-splicing-syntax-class body-bindings-directive
    #:attributes (body-bindings)
    (pattern (~optional (~seq #:body-bindings body-bindings)
                        #:defaults ([body-bindings #'()]))))

  (define-splicing-syntax-class post-guard-directive
    #:attributes (post-guard)
    (pattern (~optional (~seq #:post-guard post-guard)
                        #:defaults ([post-guard #'#t]))))

  (define-splicing-syntax-class post-done-directive
    #:attributes (post-done)
    (pattern (~optional (~seq #:post-done post-done))))

  (define-splicing-syntax-class loop-arguments-directive
    #:attributes (loop-arguments)
    (pattern (~optional (~seq #:loop-arguments loop-arguments)
                        #:defaults ([loop-arguments #'()]))))

  (define-splicing-syntax-class return-directive
    #:attributes (return)
    (pattern (~optional (~seq #:return return)
                        #:defaults ([return #'(void)])))))

(define-syntax (define-accumulator stx)
  (syntax-parse stx
    [(_ name:id
        [(pattern-directive ...)
         (~alt (~seq #:declare declare-pattern-id declare-stxclass
                     (~optional (~seq #:role declare-role-expr)))
               (~seq #:post post-action-pattern)
               (~seq #:with with-syntax-pattern with-expr)
               (~seq #:fail-when fail-when-condition-expr fail-when-message-expr)
               (~seq #:fail-unless fail-unless-condition-expr fail-unless-message-expr)
               (~seq #:when when-condition-expr)
               (~seq #:do [do-def-or-expr ...])
               (~seq #:undo [undo-def-or-expr ...]))
         ...
         outer-bindings:outer-bindings-directive
         outer-checks:outer-checks-directive
         initial-arguments:initial-arguments-directive
         pos-guard:pos-guard-directive
         pos-done:pos-done-directive
         inner-bindings:inner-bindings-directive
         pre-guard:pre-guard-directive
         pre-done:pre-done-directive
         body-results:body-results-directive
         body-bindings:body-bindings-directive
         post-guard:post-guard-directive
         post-done:post-done-directive
         loop-arguments:loop-arguments-directive
         return:return-directive]
        ...+)
     #:with to-name (format-id #'name "to-~a" #'name)
     #:with for/name (format-id #'name "for/~a" #'name)
     #'(begin
         (define-syntax (to-name stx)
           (syntax-parse stx
             [(pattern-directive ...)
              (~@ #:declare declare-pattern-id declare-stxclass
                  (~? (~@ #:role declare-role-expr))) ...
              (~@ #:post post-action-pattern) ...
              (~@ #:with with-syntax-pattern with-expr) ...
              (~@ #:fail-when fail-when-condition-expr fail-when-message-expr) ...
              (~@ #:fail-unless fail-unless-condition-expr fail-unless-message-expr) ...
              (~@ #:when when-condition-expr) ...
              (~@ #:do [do-def-or-expr ...]) ...
              (~@ #:undo [undo-def-or-expr ...]) ...
              #`(outer-bindings.outer-bindings
                 outer-checks.outer-checks
                 initial-arguments.initial-arguments
                 pos-guard.pos-guard
                 (~? pos-done.pos-done return.return)
                 inner-bindings.inner-bindings
                 pre-guard.pre-guard
                 (~? pre-done.pre-done
                     (~? pos-done.pos-done return.return))
                 body-results.body-results
                 body-bindings.body-bindings
                 post-guard.post-guard
                 (~? post-done.post-done
                     (~? pos-done.pos-done
                         (~? pre-done.pre-done return.return)))
                 loop-arguments.loop-arguments)]
             ...))
         (... (define-syntax (for/name stx)
                (syntax-parse stx
                  [(_ argument ...
                      ([match-pattern ...+ iterator:iterator] ...)
                      body ...+)
                   #'(for (to-name argument ...) ([match-pattern ... iterator] ...)
                       body ...)]))))]))
