#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide (for-syntax to-transformer))

(define-for-syntax (to-transformer #:empty empty
                                   #:insert insert
                                   #:collect collect)
  #`(#,empty #,insert #,collect))
