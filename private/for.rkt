#lang racket/base

(require (for-syntax racket/base
                     syntax/apply-transformer
                     syntax/parse)
         racket/match
         "accumulator.rkt")

(provide (rename-out [unified-for for]))

(define-for-syntax (apply-transformer transformer args)
  (local-apply-transformer (syntax-local-value transformer) args 'expression))

(define-syntax (unified-for stx)
  (syntax-parse stx
    [(_ (accumulator:id accumulator-args:expr ...)
        ([pattern:expr ...+ (iterator:id iterator-args:expr ...)] ...)
        body ...+)
     (define accumulator-result
       (apply-transformer #'accumulator #'(accumulator-args ...)))
     (define iterator-results
       (map apply-transformer
            (syntax->list #'(iterator ...))
            (syntax->list #'((iterator-args ...) ...))))
     (syntax-parse accumulator-result
       [(([a-loop-id:id a-loop-expr:expr] ...)
         (a-body-result:id ...)
         (a-loop-arg ...)
         a-done-expr:expr)
        (syntax-parse iterator-results
          [((([(i-outer-id:id ...) i-outer-expr] ...)
             ([i-loop-id:id i-loop-expr:expr] ...)
             (i-pos-guard:expr ...)
             (i-match-expr:expr ...)
             (i-loop-arg ...))
            ...)
           #'(let*-values ([(i-outer-id ...) i-outer-expr] ... ...)
               (let loop ([a-loop-id a-loop-expr]
                          ...
                          [i-loop-id i-loop-expr]
                          ... ...)
                 (if (and i-pos-guard ... ...)
                     (let-values
                         ([(a-body-result ...)
                           (match* (i-match-expr ... ...)
                             [(pattern ... ...)
                              body ...])])
                       (loop a-loop-arg ...
                             i-loop-arg ... ...))
                     a-done-expr)))])])]))

(define-syntax from-vector
  (syntax-parser
    [(v)
     #'(([(vect) v]
         [(len) (vector-length vect)])
        ([pos 0])
        ((< pos len))
        ((vector-ref vect pos))
        ((add1 pos)))]))

(define-syntax to-list
  (syntax-parser
    [()
     #'(([acc '()])
        (body-result)
        ((cons body-result acc))
        (reverse acc))]))

(unified-for (to-list)
             ([(cons a b) (from-vector #((1 . 2) (3 . 4) (5 . 6)))]
              [x (from-vector #(1 2 3))])
             (+ a b x))
