#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide to-list)

(define-syntax (to-list stx)
  (syntax-parse stx
    #:track-literals
    [()
     #'(to-list #:reverse? #f)]
    [(#:reverse? #t)
     #'(null (lambda (acc x) (cons x acc)) values)]
    [(#:reverse? #f)
     #'(null (lambda (acc x) (cons x acc)) reverse)]))
