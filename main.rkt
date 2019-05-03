#lang racket/base

(require racket/match
         (for-syntax racket/base
                     syntax/apply-transformer
                     syntax/parse))

(provide (rename-out [generic-for for]))

(define-syntax (generic-for stx)
  (syntax-case stx ()
    [(_ (to-transformer to-transformer-args ...)
        ([(pattern ...) (from-transformer from-transformer-args ...)] ...)
        body ...)
     (with-syntax ([(tmps ...)
                    (generate-temporaries #'(from-transformer ...))]
                   [(to-empty to-insert to-collect)
                    (local-apply-transformer
                     (syntax-local-value #'to-transformer)
                     #'(to-transformer-args ...)
                     'expression)]
                   [((from-take from-drop from-empty? from-collection) ...)
                    (map (lambda (a b)
                           (local-apply-transformer
                            (syntax-local-value a)
                            b
                            'expression))
                         (syntax->list #'(from-transformer ...))
                         (syntax->list #'((from-transformer-args ...) ...)))])
       #'(let loop ([acc to-empty]
                    [tmps from-collection] ...)
           (cond [(and (from-empty? tmps) ...)
                  (match-define-values (pattern ...) (from-take tmps)) ...
                  (loop (call-with-values (lambda () body ...)
                                          (lambda x (apply to-insert acc x)))
                        (from-drop tmps) ...)]
                 [else (to-collect acc)])))]))

(define-syntax (to-list stx)
  (syntax-parse stx
    #:track-literals
    [()
     #'(to-list #:reverse? #f)]
    [(#:reverse? #t)
     #'(null (lambda (acc x) (cons x acc)) values)]
    [(#:reverse? #f)
     #'(null (lambda (acc x) (cons x acc)) reverse)]))

(define-syntax (from-list stx)
  (syntax-parse stx
    #:track-literals
    [(#:reverse? #t collection)
     #'(car cdr pair? (reverse collection))]
    [(collection)
     #'(car cdr pair? collection)]))

(generic-for (to-list #:reverse? #f)
             ([(i) (from-list #:reverse? #t '(1 2 3 4 5))])
  (+ i 2))
