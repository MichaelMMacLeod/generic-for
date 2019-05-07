#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/apply-transformer
                     syntax/parse)
         racket/match
         "accumulator.rkt"
         "iterator.rkt")

(provide (rename-out [unified-for for]))

#;(define-syntax (unified-for-unoptimized stx)
  (syntax-parse stx
    [(_ ([pattern:expr ...+ iterator:iterator] ...) body ...+)
     #'(unified-for-unoptimized to-void
                                ([pattern ... iterator] ...)
                                body ...)]
    [(_ accumulator:accumulator
        ([]))]))

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
