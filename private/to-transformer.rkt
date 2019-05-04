#lang racket/base

(require racket/contract/base)

(provide (contract-out
          #:forall (Collection Element)
          (struct To-Transformer
            ([empty Collection]
             [insert (-> Collection Element Collection)]
             [collect (-> Collection Collection)]))))

(struct To-Transformer (empty insert collect))
