#lang racket/base

(require racket/contract/base
         "iterator.rkt"
         "accumulator.rkt")

(provide (contract-out
          [from-list
           (-> list? Iterator?)]
          [to-list
           (->* ()
                (#:reverse? boolean?)
                Accumulator?)]))

(define (from-list lst)
  (Iterator car
            cdr
            pair?
            lst))

(define (to-list #:reverse? [reverse? #f])
  (Accumulator null
               (λ (acc x)
                 (cons x acc))
               (if reverse? values reverse)))
