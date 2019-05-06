#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide (for-syntax unexpanded-iterator
                     expanded-iterator
                     make-iterator)
         from-vector
         from-range
         from-list
         from-naturals
         from-hash)

(begin-for-syntax
  (define-syntax-class unexpanded-iterator
    (pattern unexpanded:expr
             #:with
             (([(outer-id:id ...) outer-expr:expr] ...)
              ([loop-id:id loop-expr:expr] ...)
              (pos-guard:expr ...)
              match-expr:expr
              (loop-arg:expr ...))
             (local-expand (if (identifier? #'unexpanded)
                               #'(unexpanded)
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

(define-syntax (from-vector stx)
  (syntax-parse stx
    [(_ v:expr)
     (make-iterator #:let*-values
                    #'([(vect) v] [(len) (vector-length vect)])
                    #:loop-bindings
                    #'([pos 0])
                    #:checks
                    #'((< pos len))
                    #:match-expr
                    #'(vector-ref vect pos)
                    #:loop-args
                    #'((add1 pos)))]))

(define-syntax (from-range stx)
  (syntax-parse stx
    [(_ end:expr)
     #'(from-range 0 end)]
    [(_ start:expr end:expr)
     #'(from-range start end 1)]
    [(_ start:expr end:expr step:expr)
     #'(()
        ([n start])
        ((if (< step 0)
             (> n end)
             (< n end)))
        n
        ((+ n step)))]))

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
