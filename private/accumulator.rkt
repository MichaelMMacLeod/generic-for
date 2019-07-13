#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract/base
         racket/vector)

(provide (for-syntax accumulator
                     expanded-accumulator)
         (all-defined-out))

(begin-for-syntax
  (define-syntax-class accumulator
    (pattern unexpanded:expr
             #:with (([(outer-id:id ...) outer-expr:expr] ...)
                     (outer-check:expr ...)
                     ([loop-id:id loop-expr:expr] ...)
                     pos-guard:expr
                     pos-done-expr:expr
                     ([(inner-id:id ...) inner-expr:expr] ...)
                     pre-guard:expr
                     pre-done-expr:expr
                     (body-result:id ...)
                     ([(body-id:id ...) body-expr:expr] ...)
                     post-guard:expr
                     post-done-expr:expr
                     (loop-arg:expr ...))
             (local-expand (if (identifier? #'unexpanded)
                               (syntax/loc #'unexpanded
                                 (unexpanded))
                               #'unexpanded)
                           'expression
                           #f)))

  (define-syntax-class expanded-accumulator
    (pattern (([(outer-id:id ...) outer-expr:expr] ...)
              (outer-check:expr ...)
              ([loop-id:id loop-expr:expr] ...)
              pos-guard:expr
              pos-done-expr:expr
              ([(inner-id:id ...) inner-expr:expr] ...)
              pre-guard:expr
              pre-done-expr:expr
              (body-result:id ...)
              ([(body-id:id ...) body-expr:expr] ...)
              post-guard:expr
              post-done-expr:expr
              (loop-arg:expr ...)))))
