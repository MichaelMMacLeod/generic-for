#lang racket/base

(require racket/match
         (for-syntax racket/base
                     syntax/apply-transformer
                     syntax/parse))

(require "accumulator.rkt"
         "iterator.rkt"
         "void.rkt")

(provide (rename-out [generic-for for]
                     #;[fast-generic-for for/fast]))

(define-syntax (generic-for stx)
  (syntax-parse stx
    [(_ (~optional (~or accumulator:id accumulator:expr)
                   #:defaults ([accumulator #'to-void]))
        ([pattern ... iterator] ...)
        body ...)
     (with-syntax ([(collection ...)
                    (generate-temporaries #'(iterator ...))]
                   [((taken-values ...) ...)
                    (map generate-temporaries (syntax->list #'((pattern ...) ...)))]
                   [(no-match-id ...)
                    (generate-temporaries (syntax->list #'((pattern ...) ...)))]
                   [accumulator
                    (if (identifier? #'accumulator)
                        #'(accumulator)
                        #'accumulator)])
       #'(let loop ([acc (Accumulator-empty accumulator)]
                    [collection (Iterator-collection iterator)] ...)
           (cond [(and ((Iterator-not-empty? iterator) collection) ...)
                  (define-values (taken-values ...)
                    ((Iterator-take iterator) collection))
                  ...
                  (match* (taken-values ... ...)
                    [(pattern ... ...)
                     (loop (call-with-values (lambda () body ...)
                                             (lambda x
                                               (apply (Accumulator-insert accumulator)
                                                      acc x)))
                           ((Iterator-drop iterator) collection) ...)]
                    [(no-match-id ...) ((Accumulator-collect accumulator) acc)])]
                 [else ((Accumulator-collect accumulator) acc)])))]))

#;(for (to-list)
    ([x (from-range 100)])
    (add1 x))

#;(let loop
      ([a 0]                       ; from-range
       [acc null])                 ; to-list
    (cond
      [(< a 100)                   ; from-range
       (loop (add1 a)              ; from-range
             (cons (add1 x) acc))] ; to-list
      [else (reverse acc)]))       ; to-list

(require racket/fixnum
         racket/unsafe/ops)

(define-syntax from-range
  (syntax-parser
    [(var:id end:expr)
     #'(()
        (var 0)
        (< var end)
        (var var)
        (add1 var))]))

(define-syntax from-naturals
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

(define-syntax from-list
  (syntax-parser
    [(var:id lst:expr)
     #'(()
        (var lst)
        (pair? var)
        (var (car var))
        (cdr var))]))

(define-syntax from-vector
  (syntax-parser
    [(var:id vect:expr)
     #'(((v vect) (len (vector-length v)))
        (var 0)
        (< var len)
        (var (vector-ref v var))
        (add1 var))]))

(define-syntax unsafe-from-vector
  (syntax-parser
    [(var:id vect:expr)
     #'(((v vect) (len (unsafe-vector-length vect)))
        (var 0)
        (unsafe-fx< var len)
        (var (unsafe-vector-ref v var))
        (unsafe-fx+ 1 var))]))

(define-syntax to-hash-set
  (syntax-parser
    [(last-body:expr)
     #'(([acc (hash)])
        ((hash-set acc (first last-body) #t))
        (acc))]))

(define-syntax to-fold
  (syntax-parser
    [([fold-var:id start:expr] ...)
     (with-syntax ([(last-body ...) (generate-temporaries #'((fold-var start) ...))])
       #'((last-body ...)
          ([fold-var start] ...)
          (last-body ...)
          (fold-var ...)))]))

(define-syntax to-list
  (syntax-parser
    [()
     (with-syntax ([(last-body) (generate-temporaries #'(tmp))])
       #'((last-body)
          ([acc null])
          ((cons last-body acc))
          ((reverse acc))))]))

(define-syntax (fast-generic-for stx)
  (syntax-parse stx
    [(_ (accumulator accumulator-args ...)
        ([var:id (iterator iterator-args ...)] ...)
        body ...)
     (with-syntax
       ([(((pre-bind ...)
           bind
           test
           post-bind
           step) ...)
         (map (lambda (i i-args)
                (local-apply-transformer (syntax-local-value i)
                                         i-args
                                         'expression))
              (syntax->list #'(iterator ...))
              (syntax->list #`((var . (iterator-args ...)) ...)))])
       (with-syntax
         ([((result ...) (a-bind ...) (a-insert ...) (a-collect ...))
           (local-apply-transformer (syntax-local-value #'accumulator)
                                    #'(accumulator-args ...)
                                    'expression)])
         #`(let* (pre-bind ... ...)
             (let loop (bind ... a-bind ...)
               (cond [(and test ...)
                      (let-values ([(result ...)
                                    (let (post-bind ...)
                                      body ...)])
                        (loop step ... a-insert ...))]
                     [else (values a-collect ...)])))))]))

(require racket/list racket/set)


(define size 10000000)

(collect-garbage)
(time (for/fold ([evens '()]
                 [odds '()])
                ([x (in-range size)])
        (cond [(even? x)
               (values (cons x evens) odds)]
              [else
               (values evens (cons x odds))]))
      #f)

(collect-garbage)
(time (fast-generic-for (to-fold [evens '()]
                                 [odds '()])
                        ([x (from-range size)])
                        (cond [(even? x)
                               (values (cons x evens) odds)]
                              [else
                               (values evens (cons x odds))]))
      #f)

#;(fast-generic-for (to-list)
                  ([x (from-vector #(a b c d e f g h))]
                   [i (from-naturals)])
                  (cons i x))

#;(fast-generic-for (to-hash-set)
                  ([x (from-range 10)])
                  (define y (* 2 x))
                  y)

;(define size 10000000)
;(define lst (make-list size 0))
;(collect-garbage)
;(time (for/set ([x (in-list lst)])
;        x)
;      #f)
;(collect-garbage)
;(time (fast-generic-for (to-hash-set)
;                        ([x (from-list lst)])
;        x)
;      #f)
;
;(fast-generic-for (to-hash-set)
;                  ([x (from-vector #(a b c d e f))])
;                  x)
;#;(let ([vect v] [len (vector-length v)])
;  (let loop ([x 0])
;    (cond [(< x len)
;           (let ([x (vector-ref vect x)])
;             (void))
;           (loop (add1 x))])))
