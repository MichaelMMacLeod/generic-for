#lang racket/base

(require "accumulator.rkt"
         "for.rkt"
         (for-syntax racket/base syntax/parse racket/syntax))

(provide define-accumulator)

(define-syntax (define-accumulator stx)
  (syntax-parse stx
    [(_ name:id
        [(syntax-parse-args ...)
         (~optional (~seq #:outer-bindings outer-bindings)
                    #:defaults ([outer-bindings #'()]))
         (~optional (~seq #:outer-checks outer-checks)
                    #:defaults ([outer-checks #'()]))
         (~optional (~seq #:initial-arguments initial-arguments)
                    #:defaults ([initial-arguments #'()]))
         (~optional (~seq #:pos-guard pos-guard)
                    #:defaults ([pos-guard #'#t]))
         (~optional (~seq #:inner-bindings inner-bindings)
                    #:defaults ([inner-bindings #'()]))
         (~optional (~seq #:pre-guard pre-guard)
                    #:defaults ([pre-guard #'#t]))
         (~optional (~seq #:body-results body-results)
                    #:defaults ([body-results #'_]))
         (~optional (~seq #:post-guard post-guard)
                    #:defaults ([post-guard #'#t]))
         (~optional (~seq #:loop-arguments loop-arguments)
                    #:defaults ([loop-arguments #'()]))
         (~optional (~seq #:return return)
                    #:defaults ([return #'(void)]))]
        ...+)
     (with-syntax ([for/name (format-id #'name "for/~a" #'name)])
       #'(begin
           (define-syntax (name stx)
             (syntax-parse stx
               [(syntax-parse-args ...)
                #'(outer-bindings
                   outer-checks
                   initial-arguments
                   pos-guard
                   inner-bindings
                   pre-guard
                   body-results
                   post-guard
                   loop-arguments
                   return
                   )]
               ...))
           (... (define-syntax (for/name stx)
                  (syntax-parse stx
                    [(_ (argument ...) (iterator ...) body ...+)
                     #'(for (name argument ...)
                         (iterator ...)
                         body ...)])))))]))
