#lang racket/base

(require racket/contract/base
         "accumulator.rkt")

(provide (contract-out
          [to-list
           (->* ()
                (#:reverse? boolean?)
                Accumulator?)]))

(define (to-list #:reverse? [reverse? #f])
  (Accumulator null
               (λ (acc x)
                 (cons x acc))
               (if reverse? values reverse)))
