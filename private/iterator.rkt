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
              (outer-check:expr ...)
              ([loop-id:id loop-expr:expr] ...)
              pos-guard:expr
              ([(inner-id:id ...) inner-expr:expr] ...)
              pre-guard:expr
              match-expr:expr
              post-guard:expr
              (loop-arg:expr ...))
             (local-expand (if (identifier? #'unexpanded)
                               (syntax/loc #'unexpanded
                                 (unexpanded))
                               #'unexpanded)
                           'expression
                           #f)))

  (define-syntax-class expanded-iterator
    (pattern (([(outer-id:id ...) outer-expr:expr] ...)
              (outer-check:expr ...)
              ([loop-id:id loop-expr:expr] ...)
              pos-guard:expr
              ([(inner-id:id ...) inner-expr:expr] ...)
              pre-guard:expr
              match-expr:expr
              post-guard:expr
              (loop-arg:expr ...)))))

(define-syntax (from-vector stx)
  (syntax-parse stx
    [(_ (~var v (expr/c #'vector?)))
     #'(([(vect) v.c] [(len) (vector-length vect)])
        ()
        ([pos 0])
        (< pos len)
        ()
        #t
        (vector-ref vect pos)
        #t
        ((add1 pos)))]))

(define-syntax (from-range stx)
  (syntax-parse stx
    [(_ end)
     #'(from-range 0 end)]
    [(_ start end)
     #'(from-range start end 1)]
    [(_ start
        end
        step)
     #'(()
        ()
        ([n start])
        (if (< step 0)
            (> n end)
            (< n end))
        ()
        #t
        n
        #t
        ((+ n step)))]))

(define-syntax (from-list stx)
  (syntax-parse stx
    [(_ (~var l (expr/c #'list?)))
     #'(()
        ()
        ([lst l.c])
        (pair? lst)
        ()
        #t
        (car lst)
        #t
        ((cdr lst)))]))

(define-syntax (from-naturals stx)
  (syntax-parse stx
    [(_)
     #'(from-naturals 0)]
    [(_ (~var start (expr/c #'exact-nonnegative-integer?)))
     #'(()
        ()
        ([n start.c])
        #t
        ()
        #t
        n
        #t
        ((add1 n)))]))

(define-syntax (from-hash stx)
  (syntax-parse stx
    [(_ (~var table (expr/c #'hash?)))
     #'(([(ht) table.c])
        ()
        ([index (hash-iterate-first ht)])
        index
        ()
        #t
        (values (hash-iterate-key ht index)
                (hash-iterate-value ht index))
        #t
        ((hash-iterate-next ht index)))]))
