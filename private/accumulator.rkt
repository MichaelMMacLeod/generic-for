#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract/base
         racket/vector)

(provide (for-syntax accumulator
                     expanded-accumulator)
         vector-copy
         to-list
         to-vector
         to-hash-set
         to-fold
         to-void)

(begin-for-syntax
  (define-syntax-class accumulator
    (pattern (~and unexpanded:expr (~not expanded:expanded-accumulator))
             #:with (([(outer-id:id ...) outer-expr:expr] ...)
                     (outer-check:expr ...)
                     ([loop-id:id loop-expr:expr] ...)
                     pos-guard:expr
                     ([(inner-id:id ...) inner-expr:expr] ...)
                     pre-guard:expr
                     (body-result:id ...)
                     post-guard:expr
                     (loop-arg:expr ...)
                     done-expr:expr)
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
              ([(inner-id:id ...) inner-expr:expr] ...)
              pre-guard:expr
              (body-result:id ...)
              post-guard:expr
              (loop-arg:expr ...)
              done-expr:expr))))

(define-syntax (to-list stx)
  (syntax-parse stx
    [(_)
     #'(()
        ()
        ([acc '()])
        #t
        ()
        #t
        (body-result)
        #t
        ((cons body-result acc))
        (reverse acc))]
    [(_ #:reverse? reverse?:expr)
     #'(()
        ()
        ([acc '()])
        #t
        ()
        #t
        (body-result)
        #t
        ((cons body-result acc))
        (if reverse? (reverse acc) acc))]))

(define-syntax (to-vector stx)
  (syntax-parse stx
    #:track-literals
    [(_)
     #'(to-vector #:grow-from 16 #:by 2)]
    [(_ #:grow-from initial-capacity:expr)
     #'(to-vector #:grow-from initial-capacity #:by 2)]
    [(_ #:grow-from initial-capacity:expr #:by multiplier:expr)
     #`(([(vect) (make-vector initial-capacity)])
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
        ([iter-vect vect] [pos 0])
        #t
        ()
        #t
        (body-result)
        #t
        ((let ([len (vector-length iter-vect)])
           (cond [(< pos len)
                  (vector-set! iter-vect pos body-result)
                  iter-vect]
                 [else
                  (define new-len (* multiplier len))
                  (define new-vect (make-vector new-len))
                  (vector-copy! new-vect 0 iter-vect)
                  (vector-set! new-vect pos body-result)
                  new-vect]))
         (add1 pos))
        (vector-copy iter-vect 0 pos))]
    [(_ #:length len:expr)
     #'(to-vector #:length len #:fill 0)]
    [(_ #:length len:expr
        #:fill fill:expr)
     #`(([(vect)
          #,(syntax/loc #'len
              (make-vector len fill))])
        ()
        ([pos 0])
        (< pos len)
        ()
        #t
        (body-result)
        #t
        ((begin
           (vector-set! vect pos body-result)
           (add1 pos)))
        vect)]))

(define-syntax (to-hash-set stx)
  (syntax-parse stx
    [(_)
     #'(()
        ()
        ([table (hash)])
        #t
        ()
        #t
        (body-result)
        #t
        ((hash-set table body-result #t))
        table)]))

(define-syntax (to-fold stx)
  (syntax-parse stx
    #:track-literals
    [(_ [arg:id val:expr] ...+)
     #'(to-fold [arg val] ... #:result (values arg ...))]
    [(_ [arg:id val:expr] ...+ #:result result:expr)
     (with-syntax ([(last-body ...) (generate-temporaries #'([arg val] ...))])
       #'(()
          ()
          ([arg val] ...)
          #t
          ()
          #t
          (last-body ...)
          #t
          (last-body ...)
          result))]))

(define-syntax (to-void stx)
  (syntax-parse stx
    [(_)
     #'(() () () #t () #t (_) #t () (void))]))
