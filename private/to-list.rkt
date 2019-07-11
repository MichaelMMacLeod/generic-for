#lang racket/base

(require "define-accumulator.rkt"
         "for.rkt"
         (for-syntax syntax/parse))

(provide to-list for/to-list)

(define-accumulator to-list
  [(_)
   #:initial-arguments ([acc '()])
   #:body-results (body-results)
   #:loop-arguments ((cons body-results acc))
   #:return (reverse acc)]
  [(_ #:reverse? reverse?:expr)
   #:initial-arguments ([acc '()])
   #:body-results (body-results)
   #:loop-arguments ((cons body-results acc))
   #:return (if reverse? (reverse acc) acc)])
