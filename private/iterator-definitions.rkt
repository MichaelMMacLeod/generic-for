#lang racket/base

;;;
;;; iterator-definitions.rkt
;;;
;;; Provides several iterator implementations.
;;;

(require (for-syntax racket/base
                     syntax/parse
                     "make-iterator.rkt"))

(provide (all-defined-out))

(define-syntax (from-list stx)
  (syntax-parse stx
    [(_ l:expr)
     (make-iterator
      #:outside-bindings #'([(lst) l])
      #:outside-checks
      #`((unless (list? lst)
           #,(syntax/loc #'l
               (raise-argument-error 'from-list "list?" lst))))
      #:loop-bindings #'([iter-lst lst])
      #:inside-guards #'((pair? iter-lst))
      #:body-input #'(car iter-lst)
      #:recurse-arguments #'((cdr iter-lst)))]))

;(define-iterator vector
;  [(_ v:expr)
;   #:outer-bindings
;   ([(vect) v]
;    [(len)
;     #,(syntax/loc #'v
;         (vector-length vect))])
;   #:initial-arguments ([pos 0])
;   #:pos-guard (< pos len)
;   #:bind (vector-ref vect pos)
;   #:loop-arguments ((add1 pos))])
;
;(define-iterator list
;  [(_ l:expr)
;   #:outer-bindings ([(lst) l])
;   #:outer-checks ((list? lst))
;   #:initial-arguments ([iter-lst lst])
;   #:pos-guard (pair? iter-lst)
;   #:bind (car iter-lst)
;   #:loop-arguments ((cdr iter-lst))])
;
;(define-iterator naturals
;  [(_ (~optional start:expr #:defaults ([start #'0])))
;   #:outer-checks
;   ((unless (exact-nonnegative-integer? start)
;      #,(syntax/loc #'start
;          (raise-argument-error 'from-naturals "exact-nonnegative-integer?" start))))
;   #:initial-arguments ([n start])
;   #:bind n
;   #:loop-arguments ((add1 n))])
;
;(define-iterator hash
;  [(_ table:expr)
;   #:outer-bindings ([(ht) table])
;   #:outer-checks
;   ((unless (hash? ht)
;      #,(syntax/loc #'table
;          (raise-argument-error 'from-hash "hash?" ht))))
;   #:initial-arguments ([index (hash-iterate-first ht)])
;   #:pos-guard index
;   #:bind (values (hash-iterate-key ht index)
;                  (hash-iterate-value ht index))
;   #:loop-arguments ((hash-iterate-next ht index))])
