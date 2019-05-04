#lang racket/base

(require racket/contract/base)

(provide (contract-out
          #:forall (Collection Element)
          (struct Accumulator
            ([empty Collection]
             [insert (-> Collection Element Collection)]
             [collect (-> Collection Collection)]))))

(struct Accumulator (empty insert collect))
