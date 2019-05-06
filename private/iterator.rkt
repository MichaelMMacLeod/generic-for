#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide (for-syntax (all-from-out 'protected))
         from-vector
         from-range
         from-list
         from-naturals
         from-hash)

(module protected racket/base
  (require syntax/parse
           racket/contract/base)

  (provide (contract-out
            [make-iterator
             (-> #:let*-values (listof syntax?)
                 #:loop-bindings (listof syntax?)
                 #:checks (listof syntax?)
                 #:match-expr syntax?
                 #:loop-args (listof syntax?)
                 syntax?)])
           expanded-iterator
           unexpanded-iterator)

  (define-syntax-class unexpanded-iterator
    (pattern unexpanded:expr
             #:with
             (([(outer-id:id ...) outer-expr:expr] ...)
              ([loop-id:id loop-expr:expr] ...)
              (pos-guard:expr ...)
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
              ([loop-id:id loop-expr:expr] ...)
              (pos-guard:expr ...)
              match-expr:expr
              (loop-arg:expr ...))))

  (define (make-iterator
           #:let*-values outer-bindings
           #:loop-bindings loop-bindings
           #:checks checks
           #:match-expr match-expr
           #:loop-args loop-args)
    (syntax-parse (list outer-bindings loop-bindings checks match-expr loop-args)
      [i:expanded-iterator #'i])))

(require (for-syntax 'protected))

(define-syntax (from-vector stx)
  (syntax-parse stx
    [(_ (~var v (expr/c #'vector?)))
     (make-iterator #:let*-values
                    (list #'[(vect) v.c]
                          #'[(len) (vector-length vect)])
                    #:loop-bindings
                    (list #'[pos 0])
                    #:checks
                    (list #'(< pos len))
                    #:match-expr
                    #'(vector-ref vect pos)
                    #:loop-args
                    (list #'(add1 pos)))]))

(define-syntax (from-range stx)
  (syntax-parse stx
    [(_ end)
     #'(from-range 0 end)]
    [(_ start end)
     #'(from-range start end 1)]
    [(_ (~var start (expr/c #'real?))
        (~var end (expr/c #'real?))
        (~var step (expr/c #'real?)))
     (make-iterator #:let*-values
                    '()
                    #:loop-bindings
                    (list #'[n start.c])
                    #:checks
                    (list #'(if (< step.c 0)
                                (> n end.c)
                                (< n end.c)))
                    #:match-expr
                    #'n
                    #:loop-args
                    (list #'(+ n step.c)))]))

(define-syntax (from-list stx)
  (syntax-parse stx
    [(_ l:expr)
     #'(()
        ([lst l])
        ((pair? lst))
        (car lst)
        ((cdr lst)))]))

(define-syntax (from-naturals stx)
  (syntax-parse stx
    [(_)
     #'(from-naturals 0)]
    [(_ start:expr)
     #'(()
        ([n start])
        ()
        n
        ((add1 n)))]))

(define-syntax (from-hash stx)
  (syntax-parse stx
    [(_ table:expr)
     #'(([(ht) table])
        ([index (hash-iterate-first ht)])
        (index)
        (values (hash-iterate-key ht index)
                (hash-iterate-value ht index))
        ((hash-iterate-next ht index)))]))
