#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/vector)

(provide vector-copy
         to-list
         to-vector)

(define-syntax (to-list stx)
  (syntax-parse stx
    [(_)
     #'(()
        ([acc '()])
        (body-result)
        ((cons body-result acc))
        (reverse acc))]))

(begin-for-syntax
  (define-syntax-class nat>0
    (pattern n:nat
             #:fail-when (<= (syntax-e #'n) 0)
             "expected a positive integer"))

  (define-syntax-class nat>1
    (pattern n:nat
             #:fail-when (<= (syntax-e #'n) 1)
             "expected an integer greater than 1")))

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
    [(_ #:length l:nat #:fill fill:expr)
     #'(([(len) l] [(vect) (make-vector len fill)])
        ([pos 0])
        (body-result)
        ((begin
           (when (< pos len)
             (vector-set! vect pos body-result))
           (add1 pos)))
        vect)]))

#;(define-syntax to-hash-set
    (syntax-parser
      [()
       #'((last-body)
          ([acc (hash)])
          ((hash-set acc last-body #t))
          acc)]))

#;(define-syntax to-list
    (syntax-parser
      [()
       (with-syntax ([(last-body) (generate-temporaries #'(tmp))])
         #'((last-body)
            ([acc null])
            ((cons last-body acc))
            (reverse acc)))]))

#;(define-syntax to-fold
    (syntax-parser
      #:track-literals
      [([fold-var:id start:expr]
        ...
        (~optional (~seq #:result result:expr)
                   #:defaults ([result #'(values fold-var ...)])))
       (with-syntax ([(last-body ...) (generate-temporaries #'((fold-var start) ...))])
         #'((last-body ...)
            ([fold-var start] ...)
            (last-body ...)
            result))]))

#;(define-syntax to-void
    (syntax-parser
      [()
       #'((_)
          ()
          ()
          (values (void)))]))
