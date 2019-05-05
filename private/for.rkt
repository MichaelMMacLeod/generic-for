#lang racket/base

(require (for-syntax racket/base
                     syntax/apply-transformer
                     syntax/parse)
         racket/match
         "accumulator.rkt")

(provide (rename-out [unified-for for]))

#;(define-for-syntax (apply-transformer transformer args)
  (local-apply-transformer (syntax-local-value transformer) args 'expression))

(define-syntax (unified-for stx)
  (syntax-parse stx
    [(_ accumulator:expr
        ([pattern:expr ...+ iterator:expr] ...)
        body ...+)
     (define expanded-accumulator
       (local-expand #'accumulator 'expression #f))
     (define expanded-iterators
       (map (λ (iterator)
              (local-expand iterator 'expression #f))
            (syntax->list #'(iterator ...))))
     #;(define accumulator-result
       (apply-transformer #'accumulator #'(accumulator-args ...)))
     #;(define iterator-results
       (map apply-transformer
            (syntax->list #'(iterator ...))
            (syntax->list #'((iterator-args ...) ...))))
     (syntax-parse expanded-accumulator
       [(([(a-outer-id:id ...) a-outer-expr:expr] ...)
         ([a-loop-id:id a-loop-expr:expr] ...)
         (a-body-result:id ...)
         (a-loop-arg ...)
         a-done-expr:expr)
        (syntax-parse expanded-iterators
          [((([(i-outer-id:id ...) i-outer-expr:expr] ...)
             ([i-loop-id:id i-loop-expr:expr] ...)
             (i-pos-guard:expr ...)
             (i-match-expr:expr ...)
             (i-loop-arg ...))
            ...)
           #'(let*-values ([(a-outer-id ...) a-outer-expr]
                           ...
                           [(i-outer-id ...) i-outer-expr]
                           ... ...)
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
