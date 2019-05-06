#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide (for-syntax expanded-iterator
                     iterator)
         from-vector
         from-range
         from-list
         from-naturals
         from-hash)

(begin-for-syntax
  (define-syntax-class iterator
    (pattern unexpanded:expr
             #:with
             (([(outer-id:id ...) outer-expr:expr] ...)
              outer-check:expr
              ([loop-id:id loop-expr:expr] ...)
              guard:expr
              ([(inner-id:id ...) inner-expr:expr] ...)
              match-expr:expr
              (loop-arg:expr ...))
             (local-expand (if (identifier? #'unexpanded)
                               (syntax/loc #'unexpanded
                                 (unexpanded))
                               #'unexpanded)
                           'expression
                           #f)))

  (define-syntax-class expanded-iterator
    (pattern (([(outer-id:id ...) outer-expr:expr] ...)
              outer-check:expr
              ([loop-id:id loop-expr:expr] ...)
              guard:expr
              ([(inner-id:id ...) inner-expr:expr] ...)
              match-expr:expr
              (loop-arg:expr ...)))))

(define-syntax (from-vector stx)
  (syntax-parse stx
    [(_ (~var v (expr/c #'vector?)))
     #'(([(vect) v.c] [(len) (vector-length vect)])
        #t
        ([pos 0])
        (< pos len)
        ()
        (vector-ref vect pos)
        ((add1 pos)))]))

(define-syntax (from-range stx)
  (syntax-parse stx
    [(_ end)
     #'(from-range 0 end)]
    [(_ start end)
     #'(from-range start end 1)]
    [(_ (~var start (expr/c #'real?))
        (~var end (expr/c #'real?))
        (~var step (expr/c #'real?)))
     #'(()
        #t
        ([n start.c])
        (if (< step.c 0)
            (> n end.c)
            (< n end.c))
        ()
        n
        ((+ n step.c)))]))

(define-syntax (from-list stx)
  (syntax-parse stx
    [(_ (~var l (expr/c #'list?)))
     #'(()
        #t
        ([lst l.c])
        (pair? lst)
        ()
        (car lst)
        ((cdr lst)))]))

(define-syntax (from-naturals stx)
  (syntax-parse stx
    [(_)
     #'(from-naturals 0)]
    [(_ (~var start (expr/c #'exact-nonnegative-integer?)))
     #'(()
        #t
        ([n start.c])
        ()
        ()
        n
        ((add1 n)))]))

(define-syntax (from-hash stx)
  (syntax-parse stx
    [(_ (~var table (expr/c #'hash?)))
     #'(([(ht) table.c])
        #t
        ([index (hash-iterate-first ht)])
        index
        ()
        (values (hash-iterate-key ht index)
                (hash-iterate-value ht index))
        ((hash-iterate-next ht index)))]))
