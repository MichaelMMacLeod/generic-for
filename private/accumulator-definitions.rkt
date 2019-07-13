#lang racket/base

(require "define-accumulator.rkt"
         "for.rkt"
         (for-syntax racket/base syntax/parse))

(provide (all-defined-out))

(define-accumulator multi
  [(_ (~optional (~seq unexpanded ...+)))
   #:with
   (expanded:expanded-accumulator ...)
   (map (lambda (e)
          (local-expand (if (identifier? e)
                            (quasisyntax/loc e
                              (#,e))
                            e)
                        'expression
                        #f))
        (syntax->list #'(~? (unexpanded ...) (to-void))))
   #:with (acc0:expanded-accumulator others ...) #'(expanded ...)
   #:with (acc:expanded-accumulator ...) #'(expanded ...)
   #:with (body-result ...) (generate-temporaries #`(acc0.body-result ...))
   #:outer-bindings ([(acc.outer-id ...) acc.outer-expr] ... ...)
   #:outer-checks (acc.outer-check ... ...)
   #:initial-arguments ([acc.loop-id acc.loop-expr] ... ...)
   #:pos-guard (and acc.pos-guard ...)
   #:pos-done (values acc.pos-done-expr ...)
   #:inner-bindings ([(acc.inner-id ...) acc.inner-expr] ... ...)
   #:pre-guard (and acc.pre-guard ...)
   #:pre-done (values acc.pre-done-expr ...)
   #:body-results (body-result ...)
   #:body-bindings ([(acc.body-result ...) (values body-result ...)] ...)
   #:post-guard (and acc.post-guard ...)
   #:post-done (values acc.post-done-expr ...)
   #:loop-arguments (acc.loop-arg ... ...)
   #:return (values acc.return ...)])

(define-accumulator hash
  [(_)
   #:initial-arguments ([acc (hash)])
   #:body-results (key value)
   #:loop-arguments ((hash-set acc key value))
   #:return acc])

(define-accumulator sum
  [(_)
   #:initial-arguments ([acc 0])
   #:body-results (body-result)
   #:loop-arguments ((+ acc body-result))
   #:return acc])

(define-accumulator product
  [(_)
   #:initial-arguments ([acc 1])
   #:body-results (body-result)
   #:loop-arguments ((* acc body-result))
   #:return acc])

(define-accumulator lists
  [(_ name:id ...)
   #:with (acc ...) (generate-temporaries #'(name ...))
   #:with (body-result ...) (generate-temporaries #'(name ...))
   #:initial-arguments ([acc '()] ...)
   #:body-results (body-result ...)
   #:loop-arguments ((cons body-result acc) ...)
   #:return (values (reverse acc) ...)])

(define-accumulator first
  [(_)
   #:body-results (body-result)
   #:post-guard #f
   #:post-done body-result
   #:return #f])

(define-accumulator last
  [(_)
   #:initial-arguments ([last-body-result #f])
   #:body-results (body-result)
   #:post-done body-result
   #:loop-arguments (body-result)
   #:return last-body-result])

(define-accumulator effect
  [(_ function:expr)
   #:body-results (body-result)
   #:post-guard (begin0 #t (function body-result))])

(define-accumulator or
  [(_)
   #:initial-arguments ([last-body-result #f])
   #:body-results (body-result)
   #:post-guard (not body-result)
   #:post-done body-result
   #:loop-arguments (body-result)
   #:return last-body-result])

(define-accumulator and
  [(_)
   #:initial-arguments ([last-body-result #t])
   #:body-results (body-result)
   #:post-guard body-result
   #:post-done #f
   #:loop-arguments (body-result)
   #:return last-body-result])

(define-accumulator void [(_)])

(define-accumulator hash-set
  [(_)
   #:initial-arguments ([table (hash)])
   #:body-results (body-result)
   #:loop-arguments ((hash-set table body-result #t))
   #:return table])

(define-accumulator list
  [(_ (~optional (~seq #:reverse? reverse?:expr)
                 #:defaults ([reverse? #'#f])))
   #:initial-arguments ([acc '()])
   #:body-results (body-result)
   #:loop-arguments ((cons body-result acc))
   #:return (if reverse? acc (reverse acc))])

(define-accumulator fold
  [(_ [arg:id val:expr] ...+ (~optional (~seq #:result result:expr)))
   #:with (body-result ...) (generate-temporaries #'([arg val] ...))
   #:initial-arguments ([arg val] ...)
   #:body-results (body-result ...)
   #:loop-arguments (body-result ...)
   #:return (~? result (values arg ...))])

(require (only-in racket/contract/base and/c >/c)
         (only-in racket/vector vector-copy))
(define-accumulator vector
  [(_ (~optional (~seq #:grow-from initial-capacity:expr)
                 #:defaults ([initial-capacity #'16]))
      (~optional (~seq #:by multiplier:expr)
                 #:defaults ([multiplier #'2])))
   #:outer-checks
   ((unless (exact-positive-integer? initial-capacity)
      #,(syntax/loc #'initial-capacity
          (raise-argument-error 'to-vector
                                "exact-positive-integer?"
                                initial-capacity)))
    (unless ((and/c exact-integer? (>/c 1)) multiplier)
      #,(syntax/loc #'multiplier
          (raise-argument-error 'to-vector
                                "(and/c exact-integer? (>/c 1))"
                                multiplier))))
   #:initial-arguments ([vect (make-vector initial-capacity)] [pos 0])
   #:body-results (body-result)
   #:loop-arguments
   ((let ([len (vector-length vect)])
      (cond [(< pos len)
             (vector-set! vect pos body-result)
             vect]
            [else
             (define new-len (* multiplier len))
             (define new-vect (make-vector new-len))
             (vector-copy! new-vect 0 vect)
             (vector-set! new-vect pos body-result)
             new-vect]))
    (add1 pos))
   #:return (vector-copy vect 0 pos)]
  [(_ #:length len:expr
      (~optional (~seq #:fill fill:expr)
                 #:defaults ([fill #'0])))
   #:outer-bindings ([(vect)
                      #,(syntax/loc #'len
                          (make-vector len fill))])
   #:initial-arguments ([pos 0])
   #:pos-guard (< pos len)
   #:body-results (body-result)
   #:loop-arguments ((begin0 (add1 pos)
                       (vector-set! vect pos body-result)))
   #:return vect])
