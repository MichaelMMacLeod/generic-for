#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide define-from-transformer)

(define-syntax (define-from-transformer stx)
  (syntax-parse stx
    #:track-literals
    [(_ from-transformer:id
        [syntax-pattern #:take take
                        #:drop drop
                        #:not-empty? not-empty?
                        #:collection collection]
        ...)
     #'(define-syntax (from-transformer stx)
         (syntax-parse stx
           #:track-literals
           [syntax-pattern
            #'(take drop not-empty? collection)]
           ...))]))
