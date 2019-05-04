#lang racket/base

(require racket/contract/base)

(provide (contract-out
          #:forall (Collection Element)
          (struct Iterator
            ([take (-> Collection Element)]
             [drop (-> Collection Collection)]
             [not-empty? (-> Collection boolean?)]
             [collection Collection]))))

(struct Iterator (take drop not-empty? collection))
