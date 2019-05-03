#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         "to-transformer.rkt")

(provide to-list)

(define-syntax (to-list stx)
  (syntax-parse stx
    #:track-literals
    [((~optional (~seq #:reverse? reverse?)
                 #:defaults ([reverse? #'#f])))
     (to-transformer #:empty #'null
                     #:insert #'(λ (acc x)
                                  (cons x acc))
                     #:collect #'(if reverse? values reverse))]))
