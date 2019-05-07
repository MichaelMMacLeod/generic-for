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
    [(_ v:expr)
     #`(([(vect) v] ; make sure we don't inline a vector literal anywhere but here
         [(len)
          #,(syntax/loc #'v
              (vector-length vect))])
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
    [(_ end:expr)
     #'(from-range 0 end)]
    [(_ start:expr end:expr)
     #'(from-range start end 1)]
    [(_ start:expr end:expr step:expr)
     #`(()
        ((unless (real? start)
           #,(syntax/loc #'start
               (raise-argument-error 'from-range "real?" start)))
         (unless (real? end)
           #,(syntax/loc #'end
               (raise-argument-error 'from-range "real?" end)))
         (unless (real? step)
           #,(syntax/loc #'step
               (raise-argument-error 'from-range "real?" step))))
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
    [(_ l:expr)
     #`(([(lst) l]) ; make sure we don't inline a list literal anywhere but here
        ()
        ([iter-lst lst])

        ;; Instead of checking (list? l) at the start of iteration, we check as we
        ;; go. We get a significant performance boost since we only iterate over l once.
        (if (pair? iter-lst)
            #t
            (if (null? iter-lst)
                #f
                #,(syntax/loc #'l
                    (raise-argument-error 'from-list "list?" lst))))
        ()
        #t
        (car iter-lst)
        #t
        ((cdr iter-lst)))]))

(define-syntax (from-naturals stx)
  (syntax-parse stx
    [(_)
     #'(from-naturals 0)]
    [(_ start:expr)
     #`(()
        ((unless (exact-nonnegative-integer? start)
           #,(syntax/loc #'start
               (raise-argument-error 'from-naturals "exact-nonnegative-integer?" start))))
        ([n start])
        #t
        ()
        #t
        n
        #t
        ((add1 n)))]))

(define-syntax (from-hash stx)
  (syntax-parse stx
    [(_ table)
     #`(([(ht) table]) ; make sure we don't inline a hash literal anywhere but here
        ((unless (hash? ht)
           #,(syntax/loc #'table
               (raise-argument-error 'from-hash "hash?" ht))))
        ([index (hash-iterate-first ht)])
        index
        ()
        #t
        (values (hash-iterate-key ht index)
                (hash-iterate-value ht index))
        #t
        ((hash-iterate-next ht index)))]))
