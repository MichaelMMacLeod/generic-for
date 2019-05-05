#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/vector)

(provide vector-copy
         to-list
         to-vector
         to-hash-set
         to-fold)

(define-syntax (to-list stx)
  (syntax-parse stx
    [(_)
     #'(()
        ([acc '()])
        (body-result)
        ((cons body-result acc))
        (reverse acc))]))

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

#;(define-syntax to-void
    (syntax-parser
      [()
       #'((_)
          ()
          ()
          (values (void)))]))
