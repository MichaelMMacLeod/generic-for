#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         "define-from-transformer.rkt")

(provide from-list)

(define-from-transformer (from-list #:reverse? reverse? collection)
  #:take car
  #:drop cdr
  #:not-empty? pair?
  #:collection (if reverse? (reverse collection) collection))
