#lang racket/base

(require racket/contract/base
         racket/generator
         racket/stream
         "iterator.rkt"
         "accumulator.rkt")

(provide (contract-out
          [from-stream
           (-> stream? Iterator?)]
          [from-naturals
           (->* ()
                (exact-nonnegative-integer?)
                (stream/c exact-nonnegative-integer?))]))

(define (from-stream s)
  (Iterator stream-first
            stream-rest
            (not/c stream-empty?)
            s))

(define (from-naturals [start 0])
  (stream-cons start (from-naturals (add1 start))))
