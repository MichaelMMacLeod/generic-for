#lang racket/base

(require "private/accumulator.rkt"
         "private/iterator.rkt"
         "private/for.rkt"
         "private/from-list.rkt"
         "private/to-list.rkt")

(provide (all-from-out "private/accumulator.rkt"
                       "private/iterator.rkt"
                       "private/for.rkt"
                       "private/from-list.rkt"
                       "private/to-list.rkt"))

(module+ test
  (require rackunit)

  (check-equal? (for to-list
                  ([x (from-list '(1 2 3 4 5))])
                  x)
                '(1 2 3 4 5))

  (check-equal? (for (to-list #:reverse? #t)
                  ([x (from-list '(1 2 3 4 5))])
                  x)
                '(5 4 3 2 1)))
