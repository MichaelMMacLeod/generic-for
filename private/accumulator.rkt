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
    (pattern unexpanded:expr
             #:with (([(outer-id:id ...) outer-expr:expr] ...)
                     outer-check:expr
                     ([loop-id:id loop-expr:expr] ...)
                     guard:expr
                     ([(inner-id:id ...) inner-expr:expr] ...)
                     (body-result:id ...)
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
              outer-check:expr
              ([loop-id:id loop-expr:expr] ...)
              guard:expr
              ([(inner-id:id ...) inner-expr:expr] ...)
              (body-result:id ...)
              (loop-arg:expr ...)
              done-expr:expr))))

(define-syntax (to-list stx)
  (syntax-parse stx
    [(_)
     #'(to-list #:reverse? #t)]
    [(_ #:reverse? (~var reverse? (expr/c #'boolean?)))
     #'(()
        #t
        ([acc '()])
        #t
        ()
        (body-result)
        ((cons body-result acc))
        (if reverse?.c (reverse acc) acc))]))

(define-syntax (to-vector stx)
  (syntax-parse stx
    #:track-literals
    [(_)
     #'(to-vector #:grow-from 16 #:by 2)]
    [(_ #:grow-from initial-capacity)
     #'(to-vector #:grow-from initial-capacity #:by 2)]
    [(_ #:grow-from initial-capacity #:by multiplier)
     #'(to-vector #:grow-from initial-capacity #:with (λ (len) (* len multiplier)))]
    [(_ #:grow-from (~var initial-capacity (expr/c #'exact-positive-integer?))
        #:with (~var growth-proc (expr/c #'(->i ([old-size exact-positive-integer?])
                                                [new-size (old-size)
                                                          (and/c exact-integer?
                                                                 (>/c old-size))]))))
     #'(()
        #t
        ([vect (make-vector initial-capacity.c)] [pos 0])
        #t
        ()
        (body-result)
        ((let ([len (vector-length vect)])
           (cond [(< pos len)
                  (vector-set! vect pos body-result)
                  vect]
                 [else
                  (define new-len (growth-proc.c len))
                  (define new-vect (make-vector new-len))
                  (vector-copy! new-vect 0 vect)
                  (vector-set! new-vect pos body-result)
                  new-vect]))
         (add1 pos))
        (vector-copy vect 0 pos))]
    [(_ #:length l)
     #'(to-vector #:length l #:fill 0)]
    [(_ #:length (~var l (expr/c #'exact-nonnegative-integer?))
        #:fill (~var fill (expr/c #'any/c)))
     #'(([(len) l.c] [(vect) (make-vector len fill.c)])
        #t
        ([pos 0])
        (< pos len)
        ()
        (body-result)
        ((begin
           (vector-set! vect pos body-result)
           (add1 pos)))
        vect)]))

(define-syntax (to-hash-set stx)
  (syntax-parse stx
    [(_)
     #'(()
        #t
        ([table (hash)])
        #t
        ()
        (body-result)
        ((hash-set table body-result #t))
        table)]))

(define-syntax (to-fold stx)
  (syntax-parse stx
    #:track-literals
    [(_ [arg:id val:expr] ...)
     #'(to-fold [arg val] ... #:result (values arg ...))]
    [(_ [arg:id (~var val (expr/c #'any/c))] ... #:result result:expr)
     (with-syntax ([(last-body ...) (generate-temporaries #'([arg val] ...))])
       #'(()
          #t
          ([arg val.c] ...)
          #t
          ()
          (last-body ...)
          (last-body ...)
          result))]))

(define-syntax (to-void stx)
  (syntax-parse stx
    [(_)
     #'(()
        #t
        ()
        #t
        ()
        (_)
        ()
        (void))]))
