#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide from-hash
         from-list
         from-range)

(define-syntax from-range
  (syntax-parser
    [((var0:id var:id ...) end:expr)
     #'(()
        ((var0 0) (var 0) ...)
        ((< var0 end))
        ()
        ((add1 var0) (add1 var) ...))]))

(define-syntax from-list
  (syntax-parser
    [((var:id) lst:expr)
     #'(()
        ([var lst])
        ((pair? var))
        ([var (car var)])
        ((cdr var)))]))

(define-syntax from-hash
  (syntax-parser
    [((key:id value:id) table:expr)
     #'(([ht table])
        ([i (hash-iterate-first ht)])
        (i)
        ([key (hash-iterate-key ht i)]
         [value (hash-iterate-value ht i)])
        ((hash-iterate-next ht i)))]))

#;(define-syntax from-vector
  (syntax-parser
    [(var:id vect:expr)
     #'(((v vect) (len (vector-length v)))
        (var 0)
        (< var len)
        (var (vector-ref v var))
        (add1 var))]))

#;(define-syntax from-naturals
  (syntax-parser
    [(var:id start:expr)
     #'(()
        (var start)
        #t
        (var var)
        (add1 var))]
    [(var:id)
     #'(()
        (var 0)
        #t
        (var var)
        (add1 var))]))
