#lang racket/base

(require racket/contract/base
         racket/list
         "iterator.rkt"
         "accumulator.rkt")

(provide (contract-out
          [from-vector
           (-> vector? Iterator?)]
          [to-vector
           (->* ()
                (#:length (or/c #f exact-nonnegative-integer?)
                 #:fill any/c
                 #:grow-from exact-positive-integer?
                 #:proc (or/c #f (-> exact-nonnegative-integer? any/c)))
                Accumulator?)]))

(define (from-vector v)
  (Iterator (λ (x)
              (vector-ref (second x) (first x)))
            (λ (x)
              (list (add1 (first x)) (second x) (third x)))
            (λ (x)
              (< (first x) (third x)))
            (list 0 v (vector-length v))))

(define (to-vector #:length [length #f]
                   #:fill [fill 0]
                   #:grow-from [initial-capacity 16]
                   #:proc [proc #f])
  (cond [length
         (Accumulator
          (list 0
                (cond [proc
                       (build-vector length proc)]
                      [else
                       (make-vector length fill)]))
          (λ (acc x)
            (define index (first acc))
            (define the-vector (second acc))
            (when (< index length)
              (vector-set! the-vector index x))
            (list (add1 index) the-vector))
          second)]
        [else
         (Accumulator
          (list 0 (make-vector initial-capacity))
          (λ (acc x)
            (define index (first acc))
            (define the-vector (second acc))
            (define the-vector-length (vector-length the-vector))
            (cond [(< index the-vector-length)
                   (vector-set! the-vector index x)
                   (list (add1 index) the-vector)]
                  [else
                   (define expanded-vector (make-vector (* the-vector-length 2)))
                   (vector-copy! expanded-vector 0 the-vector)
                   (vector-set! expanded-vector index x)
                   (list (add1 index) expanded-vector)]))
          (λ (x)
            (define size (first x))
            (define original-vector (second x))
            (define shrunk-vector (make-vector size))
            (let loop ([i 0])
              (cond [(< i size)
                     (vector-set! shrunk-vector i (vector-ref original-vector i))
                     (loop (add1 i))]
                    [else shrunk-vector]))))]))
