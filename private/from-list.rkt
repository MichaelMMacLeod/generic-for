#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         "define-from-transformer.rkt")

(provide from-list)

(define-from-transformer from-list
  [((~optional (~seq #:reverse? reverse?)
               #:defaults ([reverse? #'#f]))
    collection)
   #:take car
   #:drop cdr
   #:not-empty? pair?
   #:collection (if reverse? (reverse collection) collection)])
