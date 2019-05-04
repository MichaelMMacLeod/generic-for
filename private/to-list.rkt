#lang racket/base

(require racket/contract/base
         "to-transformer.rkt")

(provide (contract-out
          [to-list
           (->* ()
                (#:reverse? boolean?)
                To-Transformer?)]))

(define (to-list #:reverse? [reverse? #f])
  (To-Transformer null
                  (λ (acc x)
                    (cons x acc))
                  (if reverse? values reverse)))
