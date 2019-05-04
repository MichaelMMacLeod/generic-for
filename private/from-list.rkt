#lang racket/base

(require racket/contract/base
         "iterator.rkt")

(provide (contract-out
          [from-list
           (->* (list?)
                (#:reverse? boolean?)
                Iterator?)]))

(define (from-list lst #:reverse? [reverse? #f])
  (Iterator car
            cdr
            pair?
            (if reverse? (reverse lst) lst)))
