#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/apply-transformer
                     syntax/parse)
         racket/format
         racket/match
         "accumulator.rkt"
         "iterator.rkt")

(provide (rename-out [ufor for])
         (all-from-out racket/format))

(define-syntax (ufor stx)
  (syntax-parse stx
    [(_ ([pattern:expr ...+ iterator:iterator] ...) body ...+)
     #'(ufor to-void ([pattern ... iterator] ...) body ...)]
    [(_ accumulator:accumulator
        ([pattern:expr ...+ iterator:iterator] ...)
        body ...+)
     #'(let*-values ([(accumulator.outer-id ...) accumulator.outer-expr]
                     ...
                     [(iterator.outer-id ...) iterator.outer-expr]
                     ... ...)
         accumulator.outer-check ...
         iterator.outer-check ... ...
         (let loop ([accumulator.loop-id accumulator.loop-expr]
                    ...
                    [iterator.loop-id iterator.loop-expr]
                    ... ...)
           (if (and accumulator.pos-guard iterator.pos-guard ...)
               (let*-values ([(accumulator.inner-id ...) accumulator.inner-expr]
                             ...
                             [(iterator.inner-id ...) iterator.inner-expr]
                             ... ...)
                 (if (and accumulator.pre-guard iterator.pre-guard ...)
                     (let-values ([(accumulator.body-result ...)
                                   (match-let*-values
                                       ([(pattern ...) iterator.match-expr] ...)
                                     body ...)])
                       (if (and accumulator.post-guard iterator.post-guard ...)
                           (loop accumulator.loop-arg ... iterator.loop-arg ... ...)
                           accumulator.done-expr))
                     accumulator.done-expr))
               accumulator.done-expr)))]
    [(_ accumulator:accumulator
        ([pattern:expr ...+ iterator:iterator]
         ...
         #:when when-expr:expr
         [nested-pattern:expr ...+ nested-iterator:iterator]
         ...)
        body ...+)
     #'(let*-values ([(accumulator.outer-id ...) accumulator.outer-expr]
                     ...
                     [(iterator.outer-id ...) iterator.outer-expr]
                     ... ...)
         accumulator.outer-check ...
         iterator.outer-check ... ...
         (let loop ([accumulator.loop-id accumulator.loop-expr]
                    ...
                    [iterator.loop-id iterator.loop-expr]
                    ... ...)
           (if (and accumulator.pos-guard iterator.pos-guard ...)
               (let*-values ([(accumulator.inner-id ...) accumulator.inner-expr]
                             ...
                             [(iterator.inner-id ...) iterator.inner-expr]
                             ... ...)
                 (if (and accumulator.pre-guard iterator.pre-guard ...)
                     (match-let*-values
                         ([(pattern ...) iterator.match-expr] ...)
                       (if when-expr
                           (let-syntax
                               ([new-acc
                                 (λ (stx)
                                   #'(()
                                      ()
                                      ([accumulator.loop-id
                                        accumulator.loop-id] ...)
                                      accumulator.pos-guard
                                      ([(accumulator.inner-id ...)
                                        accumulator.inner-expr]
                                       ...)
                                      accumulator.pre-guard
                                      (accumulator.body-result ...)
                                      accumulator.post-guard
                                      (accumulator.loop-arg ...)
                                      accumulator.nested-done-expr
                                      accumulator.nested-done-expr))])
                             (let-values ([(accumulator.loop-id ...)
                                           (ufor new-acc
                                                 ([nested-pattern ... nested-iterator] ...)
                                                 body ...)])
                               (loop accumulator.loop-id ...
                                     iterator.loop-arg ... ...)))
                           (loop accumulator.loop-id ...
                                 iterator.loop-arg ... ...)))
                     (if (and accumulator.post-guard iterator.post-guard ...)
                         (loop accumulator.loop-id ... iterator.loop-arg ... ...)
                         accumulator.done-expr)))
               accumulator.done-expr)))]))

#;(define-for-syntax (build-body stx)
  (syntax-parse stx
    [(accumulator:accumulator iterator:iterator ... body-form)
     ]))

#;(begin-for-syntax
  (define-splicing-syntax-class when-clause
    (pattern [~seq #:when when-expr:expr]))
  (define-syntax-class iterator-clause
    (pattern [pattern:expr ...+ iterator:iterator]))
  (define-splicing-syntax-class loop-clause
    (pattern (~or when-clause:when-clause iterator-clause:iterator-clause)))
  (define-syntax-class expanded-unified-for
    (pattern (accumulator:accumulator (loop-clause:loop-clause ...) body:expr ...+))))

#;(define-for-syntax (expand-loop-clauses stx)
  (syntax-parse stx
    [()
     (displayln 'empty)
     #'()]
    [(clause:when-clause others ...)
     (displayln 'when-clause)
     (with-syntax
       ([(expanded-others ...)
         (expand-loop-clauses #'(others ...))])
       #'(clause expanded-others ...))]
    [([pattern:expr ...+ clause:iterator] others ...)
     (displayln 'iterator-clause)
     (with-syntax
       ([(expanded-others ...)
         (expand-loop-clauses #'(others ...))])
       #'([pattern
           ...
           (([(clause.outer-id ...) clause.outer-expr] ...)
            (clause.outer-check ...)
            ([clause.loop-id clause.loop-expr] ...)
            clause.pos-guard
            ([(clause.inner-id ...) clause.inner-expr] ...)
            clause.pre-guard
            clause.match-expr
            clause.post-guard
            (clause.loop-arg ...))]
          others ...))]))

#;(define-for-syntax (expand-accumulator stx)
  (syntax-parse stx
    [a:accumulator
     #'(([(a.outer-id ...) a.outer-expr] ...)
        (a.outer-check ...)
        ([a.loop-id a.loop-expr] ...)
        a.pos-guard
        ([(a.inner-id ...) a.inner-expr] ...)
        a.pre-guard
        (a.body-result ...)
        a.post-guard
        (a.loop-arg ...)
        a.done-expr)]))



#;(define-for-syntax (expand-unified-for stx)
  (syntax-parse stx
    [(accumulator:accumulator
      (loop-clause:loop-clause ...)
      body ...+)
     (with-syntax
       ([expanded-acc (expand-accumulator #'accumulator)]
        [(expanded-clause ...)
         (expand-loop-clauses #'(loop-clause ...))])
       #'(expanded-acc (expanded-clause ...) body ...))]))

#;(define-syntax (unified-for-unoptimized stx)
  (syntax-parse stx
    [(_ (~optional accumulator #:defaults ([accumulator #'to-void]))
        (loop-clause:loop-clause ...) body ...+)
     (displayln #'accumulator)
     (with-syntax
       ([(expanded-accumulator (expanded-clause ...) body ...)
         (expand-unified-for #'(accumulator (loop-clause ...) body ...))])
       #'(unified-for-unoptimized expanded-accumulator
                                  (expanded-clause ...)
                                  body ...))]
    [(_ accumulator:expanded-accumulator
        ([pattern:expr ...+ iterator:expanded-iterator]
         ...
         #:when when-expr:expr
         loop-clause ...+)
        body ...+)
     (displayln 2)
     (with-syntax
       ([body
         #'(match-let*-values
               ([(pattern ...) iterator.match-expr] ...)
             (if (and accumulator.post-guard iterator.post-guard ...)
                 (if when-expr
                     (let-values ([(accumulator.loop-id ...)
                                   (unified-for-unoptimized
                                    (()
                                     ()
                                     ([accumulator.loop-id accumulator.loop-id] ...)
                                     accumulator.pos-guard
                                     ([(accumulator.inner-id ...)
                                       accumulator.inner-expr]
                                      ...)
                                     accumulator.pre-guard
                                     (accumulator.body-result ...)
                                     accumulator.post-guard
                                     (accumulator.loop-arg ...)
                                     accumulator.loop-id ...)
                                    (loop-clause ...)
                                    body ...)])
                       (loop accumulator.loop-id ...
                             iterator.loop-arg ... ...))
                     (loop accumulator.loop-id ...
                           iterator.loop-arg ... ...))
                 accumulator.done-expr))])
       (build-body #'(accumulator iterator ... body)))]
    [(_ accumulator:expanded-accumulator
        ([pattern:expr ...+ iterator:expanded-iterator] ...)
        body ...+)
     (displayln 3)
     (with-syntax
       ([body
         #'(let-values ([(accumulator.body-result ...)
                         (match-let*-values
                             ([(pattern ...) iterator.match-expr] ...)
                           body ...)])
             (if (and accumulator.post-guard iterator.post-guard ...)
                 (loop accumulator.loop-arg ... iterator.loop-arg ... ...)
                 accumulator.done-expr))])
       (build-body #'(accumulator iterator ... body)))]))

#;(define-syntax (unified-for stx)
  (syntax-parse stx
    [(_ ([pattern:expr ...+ iterator:iterator] ...) body ...+)
     #'(unified-for to-void
                    ([pattern ... iterator] ...)
                    body ...)]
    [(_ accumulator:accumulator
        ([(~describe "match pattern" pattern:expr)
          ...+
          iterator:iterator] ...)
        body ...+)
     (with-syntax*
       ([(accumulator-pos-guards ...)
         (if (equal? #t (syntax-e #'accumulator.pos-guard))
             #'()
             #'(accumulator.pos-guard))]
        [(iterator-pos-guards ...)
         (foldl (λ (check useful-checks)
                  (if (equal? #t (syntax-e check))
                      useful-checks
                      (cons check useful-checks)))
                '()
                (syntax->list #'(iterator.pos-guard ...)))]
        [(accumulator-pre-guards ...)
         (if (equal? #t (syntax-e #'accumulator.pre-guard))
             #'()
             #'(accumulator.pre-guard))]
        [(iterator-pre-guards ...)
         (foldl (λ (check useful-checks)
                  (if (equal? #t (syntax-e check))
                      useful-checks
                      (cons check useful-checks)))
                '()
                (syntax->list #'(iterator.pre-guard ...)))]
        [(accumulator-post-guards ...)
         (if (equal? #t (syntax-e #'accumulator.post-guard))
             #'()
             #'(accumulator.post-guard))]
        [(iterator-post-guards ...)
         (foldl (λ (check useful-checks)
                  (if (equal? #t (syntax-e check))
                      useful-checks
                      (cons check useful-checks)))
                '()
                (syntax->list #'(iterator.post-guard ...)))]
        [(match-body ...)
         (if (empty? (syntax->list #'(pattern ... ...)))
             #'(body ...)
             (if (andmap identifier? (syntax->list #'(pattern ... ...)))
                 #'((let*-values
                        ([(pattern ...) iterator.match-expr] ...)
                      body ...))
                 #'((match-let*-values
                        ([(pattern ...) iterator.match-expr] ...)
                      body ...))))]
        [post-guard-form
         (with-syntax
           ([(guards ...)
             (append (syntax->list #'(accumulator-post-guards ...))
                     (syntax->list #'(iterator-post-guards ...)))])
           (if (empty? (syntax->list #'(guards ...)))
               #'(loop accumulator.loop-arg ... iterator.loop-arg ... ...)
               (if (= 1 (length (syntax->list #'(guards ...))))
                   #'(if guards ...
                         (loop accumulator.loop-arg ... iterator.loop-arg ... ...)
                         accumulator.done-expr)
                   #'(if (and guards ...)
                         (loop accumulator.loop-arg ... iterator.loop-arg ... ...)
                         accumulator.done-expr))))]
        [(body-result-form ...)
         (if (empty? (syntax->list #'(accumulator.body-result ...)))
             #'(match-body ... post-guard-form)
             #'((let-values ([(accumulator.body-result ...) match-body ...])
                  post-guard-form)))]
        [(pre-guard-form ...)
         (with-syntax
           ([(guards ...)
             (append (syntax->list #'(accumulator-pre-guards ...))
                     (syntax->list #'(iterator-pre-guards ...)))])
           (if (empty? (syntax->list #'(guards ...)))
               #'(body-result-form ...)
               (if (= 1 (length (syntax->list #'(guards ...))))
                   #'((if guards ...
                          (begin body-result-form ...)
                          accumulator.done-expr))
                   #'((if (and guards ...)
                          (begin body-result-form ...)
                          accumulator.done-expr)))))]
        [(inner-form ...)
         (if (and (empty? (syntax->list #'(accumulator.inner-id ... ...)))
                  (empty? (syntax->list #'(iterator.inner-id ... ... ...))))
             #'(pre-guard-form ...)
             #'((let*-values ([(accumulator.inner-id ...) accumulator.inner-expr]
                              ...
                              [(iterator.inner-id ...) iterator.inner-expr]
                              ... ...)
                  pre-guard-form ...)))]
        [(pos-guard-form ...)
         (with-syntax
           ([(guards ...)
             (append (syntax->list #'(accumulator-pos-guards ...))
                     (syntax->list #'(iterator-pos-guards ...)))])
           (if (empty? (syntax->list #'(guards ...)))
               #'(inner-form ...)
               (if (= 1 (length (syntax->list #'(inner-form ...))))
                   (if (= 1 (length (syntax->list #'(guards ...))))
                       #'((if guards ...
                              inner-form ...
                              accumulator.done-expr))
                       #'((if (and guards ...)
                              inner-form ...
                              accumulator.done-expr)))
                   (if (= 1 (length (syntax->list #'(guards ...))))
                       #'((if guards ...
                              (begin inner-form ...)
                              accumulator.done-expr))
                       #'((if (and guards ...)
                              (begin inner-form ...)
                              accumulator.done-expr))))))]
        [loop-form
         #'(let loop ([accumulator.loop-id accumulator.loop-expr]
                      ...
                      [iterator.loop-id iterator.loop-expr]
                      ... ...)
             pos-guard-form ...)]
        [outer-form
         (if (and (empty? (syntax->list #'(accumulator.outer-id ... ...)))
                  (empty? (syntax->list #'(iterator.outer-id ... ... ...))))
             (if (and (empty? (syntax->list #'(accumulator.outer-check ...)))
                      (empty? (syntax->list #'(iterator.outer-check ... ...))))
                 #'loop-form
                 #'(begin
                     accumulator.outer-check ...
                     iterator.outer-check ... ...
                     loop-form))
             #'(let*-values ([(accumulator.outer-id ...) accumulator.outer-expr]
                             ...
                             [(iterator.outer-id ...) iterator.outer-expr]
                             ... ...)
                 accumulator.outer-check ...
                 iterator.outer-check ... ...
                 loop-form))])
       #'outer-form)]))
