#lang racket/base

(require racket/contract/base
         "private/accumulator.rkt"
         "private/iterator.rkt"
         "private/for.rkt")

(provide (all-from-out "private/accumulator.rkt"
                       "private/iterator.rkt"
                       "private/for.rkt"))

(module+ test
  (require expect
           expect/rackunit
           racket/list
           racket/port
           racket/set))

(module+ test ;; #:when
  (check-equal?
   (for (to-list)
     ([x (from-range 3)]
      #:when #t
      [y (from-range 3)])
     (cons x y))
   '((0 . 0) (0 . 1) (0 . 2) (1 . 0) (1 . 1) (1 . 2) (2 . 0) (2 . 1) (2 . 2)))
  (check-equal?
   (for (to-list)
     ([x (from-range 3)]
      #:when #f
      [y (from-range 3)])
     (cons x y))
   '())
  (check-equal?
   (for to-list
     ([x (from-range 3)]
      #:when (even? x)
      [y (from-range 5)])
     (cons x y))
   '((0 . 0) (0 . 1) (0 . 2) (0 . 3) (0 . 4) (2 . 0) (2 . 1) (2 . 2) (2 . 3) (2 . 4)))
  #;(check-equal? (with-output-to-string
                  (λ ()
                    (for ([x (from-list '(0 1 2))]
                          [#:when #t]
                          [y (from-vector #(0 1 2))])
                      (display (~a x y)))))
                "000102101112202122")
  )

(module+ test ;; to-list
  (check-expect (for to-list
                  ([x (from-range 3)]) x)
                (expect-pred list?))
  (check-equal? (for to-list
                  ([x (from-range 3)]) x)
                '(0 1 2))
  (check-equal? (for (to-list #:reverse? #t)
                  ([x (from-range 3)]) x)
                (for to-list
                  ([x (from-range 3)]) x))
  (check-equal? (for (to-list #:reverse? #f)
                  ([x (from-range 3)])
                  x)
                (reverse (for to-list
                           ([x (from-range 3)])
                           x)))
  (check-equal? (for (to-list #:reverse? #f)
                  ([x (from-range 3)])
                  x)
                '(2 1 0))
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
  )

(module+ test ;; to-vector
  (check-exn exn:fail?
             (λ ()
               (for (to-vector #:grow-from 'a)
                 ([x (from-range 10)])
                 x)))
  (check-exn exn:fail?
             (λ ()
               (for (to-vector #:length -1)
                 ([x (from-range 10)])
                 x)))
  (check-equal? (with-output-to-string
                  (λ ()
                    (for (to-vector #:length 3)
                      ([x (from-list '(1 2 3 4 5 6))])
                      (display x)
                      x)))
                "123")
  (check-exn exn:fail?
             (λ ()
               (for (to-vector #:length 5
                               #:fill (values 'not 'okay))
                 ([x (from-range 5)])
                 x)))
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
  )

(module+ test ;; for's match patterns
  (let* ([xs (range 10)]
         [ys (range 10 0 -1)]
         [zipped-1 (map (λ (x y) (cons x y)) xs ys)]
         [zipped-2 (map (λ (x y) (cons y x)) xs ys)])
    (check-equal? (for to-list
                    ([(cons x y) (from-list zipped-1)])
                    (cons y x))
                  zipped-2)))

(module+ test ; to-fold
  (check-exn exn:fail?
             (λ ()
               (for (to-fold [x (values 'too 'many)])
                 ([y (from-range 10)])
                 x)))
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
  (check-equal? (for (to-fold [x 2] #:result 'hello-world)
                  ([y (from-range 7)])
                  y)
                'hello-world)
  )

(module+ test ; to-void
  (check-equal? (with-output-to-string
                  (λ ()
                    (for ([x (from-range 5)])
                      (display x))))
                "01234")
  (check-equal? (with-output-to-string
                  (λ ()
                    (for ([x (from-range 5)])
                      (display x))))
                (with-output-to-string
                  (λ ()
                    (for to-void
                      ([x (from-range 5)])
                      (display x)))))
  (check-expect (for ([x (from-range 10)])
                  x)
                (expect-pred void?))
  )

(module+ test ; to-hash-set
  (check-equal? (for (to-hash-set)
                  ([x (from-range 3)])
                  (* 3 x))
                (hash 0 #t 3 #t 6 #t))
  )

(module+ test ; from-hash
  (check-true (set=?
               (for (to-list)
                 ([k v (from-hash #hash((k1 . v1) (k2 . v2) (k3 . v3)))])
                 (cons v k))
               '((v1 . k1) (v2 . k2) (v3 . k3))))
  )
