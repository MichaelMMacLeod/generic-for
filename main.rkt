#lang racket/base

(require racket/match
         (for-syntax racket/base
                     syntax/apply-transformer
                     syntax/parse)
         "private/for.rkt"
         "private/from-list.rkt"
         "private/to-list.rkt")

(provide (all-from-out "private/for.rkt"))

(for (to-list #:reverse? #f)
     ([(i) (from-list #:reverse? #t '(1 2 3 4 5))])
  (+ i 2))
