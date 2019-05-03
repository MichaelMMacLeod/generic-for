#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide define-from-transformer)

(define-syntax (define-from-transformer stx)
  (syntax-parse stx
    #:track-literals
    [(_ (to-transformer:id arg ...)
        #:take take
        #:drop drop
        #:not-empty? not-empty?
        #:collection collection)
     #'(define-syntax (to-transformer stx)
         (syntax-parse stx
           #:track-literals
           [(arg ...)
            #'(take drop not-empty? collection)]))]))
