#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide from-vector
         from-range
         from-list
         from-naturals
         from-hash)

(define-syntax (from-vector stx)
  (syntax-parse stx
    [(_ v:expr)
     #'(([(vect) v]
         [(len) (vector-length vect)])
        ([pos 0])
        ((< pos len))
        ((vector-ref vect pos))
        ((add1 pos)))]))

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
        (n)
        ((+ n step)))]))

(define-syntax (from-list stx)
  (syntax-parse stx
    [(_ l:expr)
     #'(()
        ([lst l])
        ((pair? lst))
        ((car lst))
        ((cdr lst)))]))

(define-syntax (from-naturals stx)
  (syntax-parse stx
    [(_)
     #'(from-naturals 0)]
    [(_ start:expr)
     #'(()
        ([n start])
        ()
        (n)
        ((add1 n)))]))

(define-syntax (from-hash stx)
  (syntax-parse stx
    [(_ table:expr)
     #'(([(ht) table])
        ([index (hash-iterate-first ht)])
        (index)
        ((values (hash-iterate-key ht index)
                 (hash-iterate-value ht index)))
        ((hash-iterate-next ht index)))]))
