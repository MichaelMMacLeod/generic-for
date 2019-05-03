#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide define-to-transformer)

(define-syntax (define-to-transformer stx)
  (syntax-parse stx
    #:track-literals
    [(_ to-transformer:id
        [syntax-pattern #:empty empty
                        #:insert insert
                        #:collect collect]
        ...)
     #'(define-syntax (to-transformer stx)
         (syntax-parse stx
           #:track-literals
           [syntax-pattern
            #'(empty insert collect)]
           ...))]))
