#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         "from-transformer.rkt")

(provide from-list)

(define-syntax (from-list stx)
  (syntax-parse stx
    #:track-literals
    [((~optional (~seq #:reverse? reverse?)
                 #:defaults ([reverse? #'#f]))
      collection)
     (from-transformer #:take #'car
                       #:drop #'cdr
                       #:not-empty? #'pair?
                       #:collection #'(if reverse? (reverse collection) collection))]))
