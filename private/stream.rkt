#lang racket/base

(require racket/contract/base
         racket/generator
         racket/stream
         "iterator.rkt"
         "accumulator.rkt")

(provide (contract-out
          [from-stream
           (-> stream? Iterator?)]))

(define (from-stream s)
  (Iterator stream-first
            stream-rest
            (not/c stream-empty?)
            s))
