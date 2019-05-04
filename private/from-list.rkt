#lang racket/base

(require racket/contract/base
         "from-transformer.rkt")

(provide (contract-out
          [from-list
           (->* (list?)
                (#:reverse? boolean?)
                From-Transformer?)]))

(define (from-list lst #:reverse? [reverse? #f])
  (From-Transformer car
                    cdr
                    pair?
                    (if reverse? (reverse lst) lst)))
