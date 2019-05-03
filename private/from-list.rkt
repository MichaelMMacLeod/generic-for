#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide from-list)

(define-syntax (from-list stx)
  (syntax-parse stx
    #:track-literals
    [(#:reverse? #t collection)
     #'(car cdr pair? (reverse collection))]
    [(collection)
     #'(car cdr pair? collection)]))
