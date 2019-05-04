#lang racket/base

(require racket/contract/base
         "accumulator.rkt")

(provide (contract-out
          [to-void
           (->* ()
                (#:proc (-> any/c any))
                Accumulator?)]))

(define (to-void #:proc [proc void])
  (Accumulator (void)
               (λ (acc x)
                 (void (proc x)))
               values))
