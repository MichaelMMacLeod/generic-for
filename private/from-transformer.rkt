#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide (for-syntax from-transformer))

(define-for-syntax (from-transformer #:take take
                                     #:drop drop
                                     #:not-empty? not-empty?
                                     #:collection collection)
  #`(#,take #,drop #,not-empty? #,collection))
