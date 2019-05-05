#lang racket/base

(require racket/contract/base
         "private/accumulator.rkt"
         "private/iterator.rkt"
         "private/for.rkt")

(provide (all-from-out "private/accumulator.rkt"
                       "private/iterator.rkt"
                       "private/for.rkt"))



(module+ test
  (require racket/set
           rackunit)

  (check-equal? (for to-list
                  ([x from-naturals]
                   [y (from-list '(a b c d e))])
                  (cons x y))
                '((0 . a) (1 . b) (2 . c) (3 . d) (4 . e)))

  (check-equal? (for (to-list)
                  ([x (from-list '(1 2 3 4 5))])
                  x)
                '(1 2 3 4 5))

  (check-equal? (for (to-list #:reverse? #f)
                  ([x (from-list '(1 2 3 4 5))])
                  x)
                '(5 4 3 2 1))

  (check-equal? (for (to-list #:reverse? #t)
                  ([x (from-list '(1 2 3 4 5))])
                  x)
                '(1 2 3 4 5))

  (check-equal? (for to-list
                  ([x (from-list '(1 2 3 4 5))])
                  x)
                (for (to-list #:reverse? #t)
                  ([x (from-list '(1 2 3 4 5))])
                  x))

  (check-equal? (for (to-list)
                  ([x (from-list '())])
                  x)
                '())

  (check-equal? (for (to-list)
                  ([x (from-range 5)])
                  x)
                '(0 1 2 3 4))

  (check-equal? (for (to-list)
                  ([x (from-range 3)])
                  (for (to-list)
                    ([y (from-range 3)])
                    (* x y)))
                '((0 0 0)
                  (0 1 2)
                  (0 2 4)))

  (check-equal? (for to-list
                  ([x from-naturals]
                   [y (from-list '(1 2 3 4 5))])
                  (cons x y))
                '((0 . 1) (1 . 2) (2 . 3) (3 . 4) (4 . 5)))

  (check-equal? (for (to-vector #:length 5 #:fill 0)
                  ([x (from-list '(1 2 3 4 5))])
                  x)
                #(1 2 3 4 5))

  (check-equal? (for (to-vector #:length 5 #:fill #f)
                  ([x (from-list '())])
                  x)
                #(#f #f #f #f #f))

  (check-equal? (for (to-vector #:grow-from 1)
                  ([x (from-list '(1 2 3))])
                  x)
                #(1 2 3))

  (check-equal? (for (to-vector)
                  ([x (from-list '(1 2 3))])
                  x)
                (for (to-vector #:grow-from 16 #:by 2)
                  ([x (from-list '(1 2 3))])
                  x))

  (check-equal? (for (to-vector #:length 5)
                  ([x (from-range 3)])
                  x)
                #(0 1 2 0 0))

  (check-true (set=?
               (for (to-list)
                 ([k v (from-hash #hash((k1 . v1) (k2 . v2) (k3 . v3)))])
                 (cons v k))
               '((v1 . k1) (v2 . k2) (v3 . k3))))

  (check-equal? (for (to-hash-set)
                  ([x (from-range 3)])
                  (* 3 x))
                (hash 0 #t 3 #t 6 #t))

  (let-values
      ([(evens odds)
        (for (to-fold [evens '()]
                      [odds '()])
          ([x (from-range 9)])
          (if (even? x)
              (values (cons x evens) odds)
              (values evens (cons x odds))))])
    (check-equal? evens (reverse '(0 2 4 6 8)))
    (check-equal? odds (reverse '(1 3 5 7))))

  (let-values
      ([(evens odds)
        (for (to-fold [evens '()]
                      [odds '()]
                      #:result (values (reverse evens)
                                       (reverse odds)))
          ([x (from-range 9)])
          (if (even? x)
              (values (cons x evens) odds)
              (values evens (cons x odds))))])
    (check-equal? evens '(0 2 4 6 8))
    (check-equal? odds '(1 3 5 7)))

  (check-equal? (for (to-void)
                  ([x (from-range 10)])
                  x)
                (void))

  (check-equal? (for to-void
                  ([x (from-range 10)])
                  x)
                (void))

  (check-equal? (for ([x (from-range 10)]) x)
                (for to-void
                  ([x (from-range 10)])
                  x))

  (check-equal? (for to-list
                  ([x (from-range 0 10 2)])
                  x)
                '(0 2 4 6 8))

  (check-equal? (for to-list
                  ([x (from-range 10 0 -2)])
                  x)
                '(10 8 6 4 2))
  )
