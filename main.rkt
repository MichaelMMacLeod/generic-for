#lang racket/base

(require "private/accumulator.rkt"
         "private/iterator.rkt"
         "private/for.rkt"
         "private/list.rkt"
         "private/vector.rkt")

(provide (all-from-out "private/accumulator.rkt"
                       "private/iterator.rkt"
                       "private/for.rkt"
                       "private/list.rkt"
                       "private/vector.rkt"))

(module+ test
  (require rackunit)

  (check-equal? (for to-list
                     ([x (from-list '(1 2 3 4 5))])
                  x)
                '(1 2 3 4 5))

  (check-equal? (for (to-list #:reverse? #t)
                     ([x (from-list '(1 2 3 4 5))])
                  x)
                '(5 4 3 2 1))

  (check-equal? (for (to-vector #:length 10)
                     ([i (from-list '(1 2 3 4 5))])
                  i)
                #(1 2 3 4 5 0 0 0 0 0))

  (check-equal? (for (to-vector #:length 10
                                #:fill #f)
                     ([i (from-list '(1 2 3 4 5))])
                  i)
                #(1 2 3 4 5 #f #f #f #f #f))

  (check-equal? (for (to-vector #:length 10
                                #:proc values)
                     ([i (from-list '(1 2 3 4 5))])
                  i)
                #(1 2 3 4 5 5 6 7 8 9))

  (check-equal? (for (to-vector #:length 10
                                #:proc (λ (x)
                                         (* 2 x)))
                     ([i (from-list '(1 2 3 4 5))])
                  i)
                #(1 2 3 4 5 10 12 14 16 18))

  (check-equal? (for to-vector
                     ([i (from-list '(1 2 3 4 5))])
                  i)
                #(1 2 3 4 5))

  (check-equal? (for (to-vector #:grow-from 1)
                     ([i (from-list '(1 2 3 4 5))])
                  i)
                #(1 2 3 4 5)))
