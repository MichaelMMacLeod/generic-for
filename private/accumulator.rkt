#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
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
                     ([loop-id:id loop-expr:expr] ...)
                     (body-result:id ...)
                     (loop-arg:expr ...)
                     done-expr:expr)
             (local-expand (if (identifier? #'unexpanded)
                               #'(unexpanded)
                               #'unexpanded)
                           'expression
                           #f)))

  (define-syntax-class expanded-accumulator
    (pattern (([(outer-id:id ...) outer-expr:expr] ...)
              ([loop-id:id loop-expr:expr] ...)
              (body-result:id ...)
              (loop-arg:expr ...)
              done-expr:expr))))

(define-syntax (to-list stx)
  (syntax-parse stx
    [(_)
     #'(to-list #:reverse? #t)]
    [(_ #:reverse? reverse?:expr)
     #'(()
        ([acc '()])
        (body-result)
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
     #'(to-vector #:grow-from initial-capacity #:with (λ (len) (* len multiplier)))]
    [(_ #:grow-from initial-capacity:expr #:with growth-proc:expr)
     #'(()
        ([vect (make-vector initial-capacity)] [pos 0])
        (body-result)
        ((let ([len (vector-length vect)])
           (cond [(< pos len)
                  (vector-set! vect pos body-result)
                  vect]
                 [else
                  (define new-len (growth-proc len))
                  (define new-vect (make-vector new-len))
                  (vector-copy! new-vect 0 vect)
                  (vector-set! new-vect pos body-result)
                  new-vect]))
         (add1 pos))
        (vector-copy vect 0 pos))]
    [(_ #:length l:expr)
     #'(to-vector #:length l #:fill 0)]
    [(_ #:length l:expr #:fill fill:expr)
     #'(([(len) l] [(vect) (make-vector len fill)])
        ([pos 0])
        (body-result)
        ((begin
           (when (< pos len)
             (vector-set! vect pos body-result))
           (add1 pos)))
        vect)]))

(define-syntax (to-hash-set stx)
  (syntax-parse stx
    [(_)
     #'(()
        ([table (hash)])
        (body-result)
        ((hash-set table body-result #t))
        table)]))

(define-syntax (to-fold stx)
  (syntax-parse stx
    #:track-literals
    [(_ [arg:id val:expr] ...)
     #'(to-fold [arg val] ... #:result (values arg ...))]
    [(_ [arg:id val:expr] ... #:result result:expr)
     (with-syntax ([(last-body ...) (generate-temporaries #'([arg val] ...))])
       #'(()
          ([arg val] ...)
          (last-body ...)
          (last-body ...)
          result))]))

(define-syntax (to-void stx)
  (syntax-parse stx
    [(_)
     #'(()
        ()
        (_)
        ()
        (void))]))
