#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         "define-to-transformer.rkt")

(provide to-list)

(define-to-transformer (to-list #:reverse? reverse?)
  #:empty null
  #:insert (λ (acc x)
             (cons x acc))
  #:collect (if reverse? values reverse))
