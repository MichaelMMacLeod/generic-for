#lang racket/base

(require racket/contract/base
         racket/generator
         racket/stream
         "iterator.rkt"
         "accumulator.rkt")

(provide (contract-out
          [from-stream
           (-> stream? Iterator?)]
          [naturals
           (->* ()
                (exact-nonnegative-integer?)
                (stream/c exact-nonnegative-integer?))]))

(define (from-stream s)
  (Iterator stream-first
            stream-rest
            (not/c stream-empty?)
            s))

(define (naturals [start 0])
  (stream-cons start (naturals (add1 start))))
