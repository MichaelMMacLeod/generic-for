#lang racket/base

(require racket/contract/base)

(provide (contract-out
          #:forall (Collection Element)
          (struct From-Transformer
            ([take (-> Collection Element)]
             [drop (-> Collection Collection)]
             [not-empty? (-> Collection boolean?)]
             [collection Collection]))))

(struct From-Transformer (take drop not-empty? collection))
