#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide from-vector
         from-range
         from-list
         from-naturals)

(define-syntax from-vector
  (syntax-parser
    [(v)
     #'(([(vect) v]
         [(len) (vector-length vect)])
        ([pos 0])
        ((< pos len))
        ((vector-ref vect pos))
        ((add1 pos)))]))

(define-syntax from-range
  (syntax-parser
    [(end:expr)
     #'(()
        ([n 0])
        ((< n end))
        (n)
        ((add1 n)))]))

(define-syntax from-list
  (syntax-parser
    [(l:expr)
     #'(()
        ([lst l])
        ((pair? lst))
        ((car lst))
        ((cdr lst)))]))

(define-syntax from-naturals
  (syntax-parser
    [(start:expr)
     #'(()
        ([n start])
        ()
        (n)
        ((add1 n)))]))
