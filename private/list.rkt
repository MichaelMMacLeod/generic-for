#lang racket/base

(require racket/contract/base
         "iterator.rkt"
         "accumulator.rkt")

(provide (contract-out
          [from-list
           (->* (list?)
                (#:reverse? boolean?)
                Iterator?)]
          [to-list
           (->* ()
                (#:reverse? boolean?)
                Accumulator?)]))

(define (from-list lst #:reverse? [reverse? #f])
  (Iterator car
            cdr
            pair?
            (if reverse? (reverse lst) lst)))

(define (to-list #:reverse? [reverse? #f])
  (Accumulator null
               (λ (acc x)
                 (cons x acc))
               (if reverse? values reverse)))
