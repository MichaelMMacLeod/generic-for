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

(provide (rename-out [unified-for-unoptimized for])
         (all-from-out racket/format))

(begin-for-syntax
  (define-syntax-class when-clause
    (pattern [#:when when-expr:expr]))
  (define-syntax-class iterator-clause
    (pattern [pattern:expr ...+ iterator:iterator]))
  (define-syntax-class loop-clause
    (pattern (~or when-clause:when-clause iterator-clause:iterator-clause))))

(define-for-syntax (expand-loop-clause stx)
  (syntax-parse stx
    [clause:when-clause
     #'clause]
    [(pattern:expr ...+ clause:iterator)
     #'[pattern
        ...
        (([(clause.outer-id ...) clause.outer-expr] ...)
         (clause.outer-check ...)
         ([clause.loop-id clause.loop-expr] ...)
         clause.pos-guard
         ([(clause.inner-id ...) clause.inner-expr] ...)
         clause.pre-guard
         clause.match-expr
         clause.post-guard
         (clause.loop-arg ...))]]))

(define-for-syntax (expand-accumulator stx)0
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

(define-syntax (unified-for-unoptimized stx)
  (syntax-parse stx
    [(_ ([pattern:expr ...+ iterator:iterator] ...) body ...+)
     #'(unified-for-unoptimized to-void
                                ([pattern ... iterator] ...)
                                body ...)]
    [(_ accumulator:expanded-accumulator
        ([pattern:expr ...+ iterator:expanded-iterator]
         ...
         [#:when when-expr:expr]
         loop-clause ...+)
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
                           accumulator.done-expr))
                     accumulator.done-expr))
               accumulator.done-expr)))]
    [(_ accumulator:expanded-accumulator
        ([pattern:expr ...+ iterator:expanded-iterator] ...)
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
        (loop-clause:loop-clause ...)
        body ...+)
     (with-syntax
       ([expanded-acc (expand-accumulator #'accumulator)]
        [(expanded-clause ...)
         (map expand-loop-clause (syntax->list #'(loop-clause ...)))])
       #'(unified-for-unoptimized expanded-acc
                                  (expanded-clause ...)
                                  body ...))]))

(define-syntax (unified-for stx)
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
