#lang racket/base

(require (for-syntax racket/base
                     syntax/apply-transformer
                     syntax/parse)
         racket/match
         "accumulator.rkt")

(provide (rename-out [unified-for for]))

(define-syntax (unified-for stx)
  (syntax-parse stx
    [(_ (~optional (~or (accumulator accumulator-args ...) accumulator)
                   #:defaults ([accumulator #'to-void]))
        ([var:id ...+ (iterator:id iterator-args:expr ...)] ...)
        body ...)
     (with-syntax
       ([(accumulator-args ...)
         (if (attribute accumulator-args)
             #'(accumulator-args ...)
             #'())])
       (with-syntax
         ([(((pre-bind ...)
             (bind ...)
             (test ...)
             (post-bind ...)
             (step ...))
            ...)
           (map (lambda (i i-args)
                  (local-apply-transformer (syntax-local-value i)
                                           i-args
                                           'expression))
                (syntax->list #'(iterator ...))
                (syntax->list #'((var ... iterator-args ...) ...)))]
          [((result ...) (a-bind ...) (a-insert ...) a-collect)
           (local-apply-transformer (syntax-local-value #'accumulator)
                                    (if #'(accumulator-args ...)
                                        #'(accumulator-args ...)
                                        #'())
                                    'expression)])
         #`(let* (pre-bind ... ...)
             (let loop (bind ... ... a-bind ...)
               (cond [(and test ... ...)
                      (let-values ([(result ...)
                                    (let (post-bind ... ...)
                                      body ...)])
                        (loop step ... ... a-insert ...))]
                     [else a-collect])))))]))

#;(let-values ([(i-outer-id ...) i-outer-expr] ...)
    (let loop ([i-loop-id i-loop-expr] ...)
      (if i-pos-guard
          (let-values
              ([(i-inner-id ...)
                (match* (i-match-expr ...)
                  [(@arg-patterns ...)
                   @body ...])])
            (loop i-loop-arg ...))
          i-done-expr)))

(define-for-syntax (apply-transformer transformer args)
  (local-apply-transformer (syntax-local-value transformer) args 'expression))

(define-syntax (unified-for~ stx)
  (syntax-parse stx
    [(_ (accumulator:id accumulator-args:expr ...)
        ([pattern:expr ...+ (iterator:id iterator-args:expr ...)] ...)
        body ...+)
     (define accumulator-result
       (apply-transformer #'accumulator #'(accumulator-args ...)))
     (define iterator-results
       (map apply-transformer
            (syntax->list #'(iterator ...))
            (syntax->list #'((iterator-args ...) ...))))
     (syntax-parse accumulator-result
       [(([a-loop-id:id a-loop-expr:expr] ...)
         (a-body-result:id ...)
         (a-loop-arg ...)
         a-done-expr:expr)
        (syntax-parse iterator-results
          [((([(i-outer-id:id ...) i-outer-expr] ...)
             ([i-loop-id:id i-loop-expr:expr] ...)
             (i-pos-guard:expr ...)
             (i-match-expr:expr ...)
             (i-loop-arg ...))
            ...)
           #'(let*-values ([(i-outer-id ...) i-outer-expr] ... ...)
               (let loop ([a-loop-id a-loop-expr]
                          ...
                          [i-loop-id i-loop-expr]
                          ... ...)
                 (if (and i-pos-guard ... ...)
                     (let-values
                         ([(a-body-result ...)
                           (match* (i-match-expr ... ...)
                             [(pattern ... ...)
                              body ...])])
                       (loop a-loop-arg ...
                             i-loop-arg ... ...))
                     a-done-expr)))])])]))

(define-syntax from-vector
  (syntax-parser
    [(v)
     #'(([(vect) v]
         [(len) (vector-length vect)])
        ([pos 0])
        ((< pos len))
        ((vector-ref vect pos))
        ((add1 pos)))]))

(define-syntax to-list
  (syntax-parser
    [()
     #'(([acc '()])
        (body-result)
        ((cons body-result acc))
        (reverse acc))]))

(unified-for~ (to-list)
              ([(cons a b) (from-vector #((1 . 2) (3 . 4) (5 . 6)))]
               [x (from-vector #(1 2 3))])
              (+ a b x))

#;(let*-values ([(i-outer-id ...) i-outer-expr] ...)
  (let loop ([a-loop-id a-loop-expr]
             ...
             [i-loop-id i-loop-expr]
             ...)
    (if (and i-pos-guard ...)
        (let-values
            ([(a-body-result ...)
              (match (i-match-expr ...)
                [(pattern ...)
                 body ...])])
          (loop i-loop-arg ...
                a-loop-arg ...))
        a-done-expr)))

(let*-values ([(vect)                               ;         from-vector
               #(1 2 3 4 5 6 7 8 9)]                ;         from-vector user
              [(len)                                ;         from-vector
               (vector-length vect)])               ;         from-vector
  (let loop ([pos 0]                                ;         from-vector
             [acc '()])                             ; to-list
    (if (and (< pos len))                           ;         from-vector
        (let-values                                 ;
            ([(body-result)                         ; to-list
              (match* ((vector-ref vect pos))       ;         from-vector
                [((? number? n))                    ;                     user
                 (define n*2                        ;                     user
                   (* n 2))                         ;                     user
                 (displayln n*2)                    ;                     user
                 n*2])])                            ;                     user
          (loop (add1 pos)                          ;         from-vector
                (cons body-result acc)))            ; to-list
        (reverse acc))))                            ; to-list
