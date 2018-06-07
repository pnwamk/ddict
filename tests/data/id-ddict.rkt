#lang racket/base

(require (for-syntax racket/base)
         rackunit
         racket/syntax
         "../../data/id-ddict.rkt")

(define failsym (gensym 'fail))

;; - - - - - - - - - - - -
;; constructors
;; - - - - - - - - - - - -
(define empty-dd (free-id-ddict))

(define i0 #'i0)
(define i1 #'i1)
(define i2 #'i2)
(define i3 #'i3)
(define i4 #'i4)

(define dd5 (free-id-ddict i0 "0" i1 "1" i2 "2" i3 "3" i4 "4"))
(check-exn #rx"free-id-ddict: contract violation"
           (λ () (free-id-ddict #'i0 "0" #'i1 "1" #'i2)))
(define (empty-mdd) (mutable-free-id-ddict))
(define (mdd5) (mutable-free-id-ddict i0 "0" i1 "1" i2 "2" i3 "3" i4 "4"))
(check-exn #rx"free-id-ddict: contract violation"
           (λ () (mutable-free-id-ddict i0 "0" i1 "1" i2)))

;; - - - - - - - - - - - -
;; free-id-ddict?
;; - - - - - - - - - - - -
(check-false (free-id-ddict? #hash()))
(check-false (free-id-ddict? 42))
(check-true (free-id-ddict? empty-dd))
(check-true (free-id-ddict? dd5))
(check-true (free-id-ddict? (empty-mdd)))
(check-true (free-id-ddict? (mdd5)))

;; - - - - - - - - - - - -
;; immutable-free-id-ddict?
;; - - - - - - - - - - - -
(check-true (immutable-free-id-ddict? empty-dd))
(check-true (immutable-free-id-ddict? dd5))
(check-false (immutable-free-id-ddict? (empty-mdd)))
(check-false (immutable-free-id-ddict? (mdd5)))

;; - - - - - - - - - - - -
;; mutable-free-id-ddict?
;; - - - - - - - - - - - -
(check-false (mutable-free-id-ddict? empty-dd))
(check-false (mutable-free-id-ddict? dd5))
(check-true (mutable-free-id-ddict? (empty-mdd)))
(check-true (mutable-free-id-ddict? (mdd5)))

;; - - - - - - - - - - - -
;; equality
;; - - - - - - - - - - - -
(check-equal? (free-id-ddict i1 2 i3 4)
              (free-id-ddict #'i3 4 #'i1 2))
(check-equal? (mutable-free-id-ddict i1 2 #'i3 4)
              (mutable-free-id-ddict #'i3 4 i1 2))

;; - - - - - - - - - - - -
;; free-id-ddict-count
;; - - - - - - - - - - - -
(check-equal? (free-id-ddict-count empty-dd) 0)
(check-equal? (free-id-ddict-count dd5) 5)
(check-equal? (free-id-ddict-count (empty-mdd)) 0)
(check-equal? (free-id-ddict-count (mdd5)) 5)
(check-exn #rx"free-id-ddict-count: contract violation"
           (λ () (free-id-ddict-count "not a free-id-ddict")))

;; - - - - - - - - - - - -
;; free-id-ddict-ref (immutable)
;; - - - - - - - - - - - -
(for ([k (in-list (list i4 i3 i2 i1 i0))]
      [v (in-list '("4" "3" "2" "1" "0"))])
  (check-equal? (free-id-ddict-ref dd5 k) v))
(check-equal? (free-id-ddict-ref dd5 #'i42 failsym) failsym)

;; - - - - - - - - - - - -
;; free-id-ddict-ref (mutable)
;; - - - - - - - - - - - -
(let ([dd (mdd5)])
  (for ([k (in-list (list i4 i3 i2 i1 i0))]
        [v (in-list '("4" "3" "2" "1" "0"))])
    (check-equal? (free-id-ddict-ref dd k) v))
  (check-equal? (free-id-ddict-ref dd5 #'i42 failsym) failsym))


;; - - - - - - - - - - - -
;; free-id-ddict-set
;; - - - - - - - - - - - -
(check-equal? (for/fold ([dd empty-dd])
                        ([i (in-list '(4 3 2 1 0))]
                         [k (in-list (list #'i4 #'i3 #'i2 #'i1 #'i0))]
                         [v (in-list '("4" "3" "2" "1" "0"))])
                (free-id-ddict-set dd k v))
              dd5)
(check-exn #rx"free-id-ddict-set: contract violation"
           (λ () (free-id-ddict-set 42 #'i42 42)))
(check-exn #rx"free-id-ddict-set: contract violation"
           (λ () (free-id-ddict-set (mdd5) 42 42)))

(define id-list1 (list i4 i3 i2 i1 i0))
(define id-list2 (list #'i42 #'i32 #'i22 #'i12 #'i02))

(let ([dd (for/fold ([dd dd5])
                    ([k (in-list id-list2)]
                     [v (in-list '("4" "3" "2" "1" "0"))])
            (free-id-ddict-set dd k v))])
  (check-equal? (free-id-ddict-count dd) 10)
  (for ([k (in-list (list #'i4 #'i3 #'i2 #'i1 #'i0))]
        [v (in-list '("4" "3" "2" "1" "0"))])
    (check-equal? (free-id-ddict-ref dd k) v))
  (check-equal? (free-id-ddict-keys dd)
                (append (reverse id-list2) id-list1)) 
  (check-equal? (free-id-ddict-values dd)
                (append (reverse '("4" "3" "2" "1" "0")) '("4" "3" "2" "1" "0"))) 
  (check-equal? (free-id-ddict->list dd)
                (map cons
                     (append (reverse id-list2) id-list1)
                     (append (reverse '("4" "3" "2" "1" "0")) '("4" "3" "2" "1" "0"))))
  (let ([dd (for/fold ([dd dd])
                      ([k (in-list id-list1)])
              (free-id-ddict-set dd k k))])
    (check-equal? (free-id-ddict-count dd) 10)
    (for ([k (in-list id-list1)]
          [v (in-list '("4" "3" "2" "1" "0"))])
      (check-equal? (free-id-ddict-ref dd k) k)
      #;(check-equal? (free-id-ddict-ref dd v) v)
      )
    (check-equal? (free-id-ddict-keys dd)
                  (append (reverse id-list2) id-list1))
    (check-equal? (free-id-ddict-values dd)
                  (append (reverse '("4" "3" "2" "1" "0")) id-list1))
    (check-equal? (free-id-ddict->list dd)
                  (map cons
                       (append (reverse id-list2) id-list1)
                       (append (reverse '("4" "3" "2" "1" "0")) id-list1)))))


;; - - - - - - - - - - - -
;; free-id-ddict-remove
;; - - - - - - - - - - - -
(let* ([dd dd5]
       [dd (free-id-ddict-remove dd i3)]
       [_ (begin (check-equal? (free-id-ddict-count dd) 4)
                 (check-false (free-id-ddict-compact? dd)))]
       [dd (free-id-ddict-remove dd i1)]
       [_ (begin (check-equal? (free-id-ddict-count dd) 3)
                 (check-false (free-id-ddict-compact? dd)))]
       [dd (free-id-ddict-remove dd i0)]
       [_ (begin (check-equal? (free-id-ddict-count dd) 2)
                 (check-true (free-id-ddict-compact? dd)))])
  (check-exn #rx"free-id-ddict-remove: contract violation"
             (λ () (free-id-ddict-remove 42 42)))
  (check-exn #rx"free-id-ddict-remove: contract violation"
             (λ () (free-id-ddict-remove (mdd5) 42)))
  (check-equal? (free-id-ddict-keys dd) (list i4 i2))
  (check-equal? (free-id-ddict-values dd) '("4" "2"))
  (check-equal? dd (free-id-ddict i4 "4" i2 "2"))
  (check-equal? (free-id-ddict-remove (free-id-ddict-remove dd i4) i2) empty-dd))


;; - - - - - - - - - - - -
;; free-id-ddict-set!
;; - - - - - - - - - - - -
(let ([dd (mdd5)])
  (for ([v (in-list id-list2)])
    (check-equal? (free-id-ddict-set! dd v v) (void)))
  (check-equal? (free-id-ddict-count dd) 10)
  (check-exn #rx"free-id-ddict-set!: contract violation"
             (λ () (free-id-ddict-set! 42 #'i42 42)))
  (check-exn #rx"free-id-ddict-set!: contract violation"
             (λ () (free-id-ddict-set! dd5 42 42)))
  (for ([k (in-list id-list1)]
        [v (in-list '("4" "3" "2" "1" "0"))])
    (check-equal? (free-id-ddict-ref dd k) v)))
(let ([dd (mdd5)])
  (for ([v (in-list id-list2)])
    (free-id-ddict-set! dd v v))
  (check-equal? (free-id-ddict-count dd) 10)
  (for ([k (in-list id-list1)]
        [v (in-list '("4" "3" "2" "1" "0"))])
    (check-equal? (free-id-ddict-ref dd k) v))
  (for ([k (in-list id-list2)])
    (check-equal? (free-id-ddict-ref dd k) k))
  (check-equal? (free-id-ddict-keys dd)
                (append (reverse id-list2) id-list1))
  (check-equal? (free-id-ddict-values dd)
                (append (reverse id-list2) '("4" "3" "2" "1" "0")))
  (check-equal? (free-id-ddict->list dd)
                (map cons
                     (append (reverse id-list2) id-list1)
                     (append (reverse id-list2) '("4" "3" "2" "1" "0"))))
  (for ([k (in-list id-list1)])
    (free-id-ddict-set! dd k k))
  (check-equal? (free-id-ddict-count dd) 10)
  (for ([k (in-list id-list1)]
        [v (in-list id-list2)])
    (check-equal? (free-id-ddict-ref dd k) k)
    (check-equal? (free-id-ddict-ref dd v) v))
  (check-equal? (free-id-ddict-keys dd)
                (append (reverse id-list2) id-list1))
  (check-equal? (free-id-ddict-values dd)
                (append (reverse id-list2) id-list1))
  (check-equal? (free-id-ddict->list dd)
                (map cons
                     (append (reverse id-list2) id-list1)
                     (append (reverse id-list2) id-list1))))

;; - - - - - - - - - - - -
;free-id-ddict-remove!
;; - - - - - - - - - - - -
(let ([dd (mdd5)])
  (check-equal? (free-id-ddict-remove! dd i3) (void))
  (check-equal? (free-id-ddict-count dd) 4)
  (check-false (free-id-ddict-compact? dd))
  (free-id-ddict-remove! dd i1)
  (free-id-ddict-remove! dd i1)
  (check-equal? (free-id-ddict-count dd) 3)
  (check-false (free-id-ddict-compact? dd))
  (free-id-ddict-remove! dd i0)
  (check-equal? (free-id-ddict-count dd) 2)
  (check-true (free-id-ddict-compact? dd))
  (check-equal? (free-id-ddict-keys dd) (list i4 i2))
  (check-equal? (free-id-ddict-values dd) '("4" "2"))
  (check-equal? dd (mutable-free-id-ddict i4 "4" i2 "2"))
  (free-id-ddict-remove! dd i4)
  (free-id-ddict-remove! dd i2)
  (check-true (free-id-ddict-empty? dd))
  (check-exn #rx"free-id-ddict-remove!: contract violation"
             (λ () (free-id-ddict-remove! 42 #'i42)))
  (check-exn #rx"free-id-ddict-remove!: contract violation"
             (λ () (free-id-ddict-remove! dd5 42))))


;; - - - - - - - - - - - -
;; free-id-ddict-keys
;; - - - - - - - - - - - -
(check-equal? (free-id-ddict-keys empty-dd) '())
(check-equal? (free-id-ddict-keys dd5) id-list1)
(check-equal? (free-id-ddict-keys (empty-mdd)) '())
(check-equal? (free-id-ddict-keys (mdd5)) id-list1)
(check-exn #rx"free-id-ddict-keys: contract violation"
           (λ () (free-id-ddict-keys 42)))

;; - - - - - - - - - - - -
;; free-id-ddict-values
;; - - - - - - - - - - - -
(check-equal? (free-id-ddict-values empty-dd) '())
(check-equal? (free-id-ddict-values dd5) '("4" "3" "2" "1" "0"))
(check-equal? (free-id-ddict-values (empty-mdd)) '())
(check-equal? (free-id-ddict-values (mdd5)) '("4" "3" "2" "1" "0"))
(check-exn #rx"free-id-ddict-values: contract violation"
           (λ () (free-id-ddict-values 42)))

;; - - - - - - - - - - - -
;; free-id-ddict->list
;; - - - - - - - - - - - -
(check-equal? (free-id-ddict->list empty-dd) '())
(check-equal? (free-id-ddict->list dd5) (map cons id-list1 '("4" "3" "2" "1" "0")))
(check-equal? (free-id-ddict->list (empty-mdd)) '())
(check-equal? (free-id-ddict->list (mdd5)) (map cons id-list1 '("4" "3" "2" "1" "0")))
(check-exn #rx"free-id-ddict->list: contract violation"
           (λ () (free-id-ddict->list 42)))

;; - - - - - - - - - - - -
;; free-id-ddict-ref!
;; - - - - - - - - - - - -
(define i5 #'i5)
(define i6 #'i6)
(let ([dd (mdd5)])
  (for ([k (in-list id-list1)]
        [v (in-list '("4" "3" "2" "1" "0"))])
    (check-equal? (free-id-ddict-ref! dd k (λ () (error 'oh-no!))) v))
  (check-equal? (free-id-ddict-ref! dd i5 i5) i5)
  (define b (box #f))
  (check-equal? (free-id-ddict-ref! dd i6 (λ () (set-box! b #t) i6)) i6)
  (check-equal? (unbox b) #t)
  (check-equal? (free-id-ddict-count dd) 7)
  (check-equal? (free-id-ddict->list dd) (map cons
                                              (list* i6 i5 id-list1)
                                              (list i6 i5 "4" "3" "2" "1" "0"))))
(check-exn #rx"free-id-ddict-ref!: contract violation"
           (λ () (free-id-ddict-ref! 42 #'i42 42)))
(check-exn #rx"free-id-ddict-ref!: contract violation"
           (λ () (free-id-ddict-ref! dd5 42 42)))

;; - - - - - - - - - - - -
;; free-id-ddict-has-key?
;; - - - - - - - - - - - -
(check-equal? (free-id-ddict-has-key? empty-dd i1) #f)
(check-equal? (free-id-ddict-has-key? dd5 i0) #t)
(check-equal? (free-id-ddict-has-key? dd5 i5) #f)
(check-equal? (free-id-ddict-has-key? (empty-mdd) i0) #f)
(check-equal? (free-id-ddict-has-key? (mdd5) i0) #t)
(check-equal? (free-id-ddict-has-key? (mdd5) i5) #f)
(check-exn #rx"free-id-ddict-has-key\\?: contract violation"
           (λ () (free-id-ddict-has-key? 42 42)))
(check-exn #rx"free-id-ddict-has-key\\?: contract violation"
           (λ () (free-id-ddict-has-key? (mdd5) 42)))

;; - - - - - - - - - - - -
;; free-id-ddict-empty?
;; - - - - - - - - - - - -
(check-true (free-id-ddict-empty? empty-dd))
(check-false (free-id-ddict-empty? dd5))
(check-true (free-id-ddict-empty? (empty-mdd)))
(check-false (free-id-ddict-empty? (mdd5)))
(check-exn #rx"free-id-ddict-empty\\?: contract violation"
           (λ () (free-id-ddict-empty? 42)))

;; - - - - - - - - - - - -
;; free-id-ddict-set*
;; - - - - - - - - - - - -
(check-equal? (free-id-ddict-set* empty-dd) empty-dd)
(check-equal? (free-id-ddict-set* empty-dd i0 "0" i1 "1" i2 "2" i3 "3" i4 "4")
              dd5)
(check-equal? (free-id-ddict-set* dd5 i4 i4 i3 i3 i0 i0 i1 i1 i2 i2)
              (free-id-ddict i0 i0 i1 i1 i2 i2 i3 i3 i4 i4))
(check-equal? (free-id-ddict-keys
               (free-id-ddict-set* empty-dd i0 "0" i1 "1" i2 "2" i3 "3" i4 "4"))
              id-list1)
(check-equal? (free-id-ddict-keys
               (free-id-ddict-set* dd5 i4 i4 i3 i3 i0 i0 i1 i1 i2 i2))
              id-list1)
(check-equal? (free-id-ddict-keys
               (free-id-ddict-set* dd5 i4 4 i3 3 i0 0 i1 1 i2 2 i5 42))
              (list* i5 id-list1))
(check-equal? (free-id-ddict-values
               (free-id-ddict-set* dd5 i4 4 i3 3 i0 0 i1 1 i2 2 i5 42))
              '(42 4 3 2 1 0))
(check-equal? (free-id-ddict-set* empty-dd i0 "0" i1 "1" i0 0 i1 1)
              (free-id-ddict i0 0 i1 1))
(check-exn #rx"free-id-ddict-set\\*: contract violation"
           (λ () (free-id-ddict-set* 42)))
(check-exn #rx"free-id-ddict-set\\*: contract violation"
           (λ () (free-id-ddict-set* (mdd5))))
(check-exn #rx"free-id-ddict-set\\*: contract violation"
           (λ () (free-id-ddict-set* empty-dd 1 1 2 2 3)))

;; - - - - - - - - - - - -
;; free-id-ddict-set*!
;; - - - - - - - - - - - -
(check-equal? (free-id-ddict-set*! (empty-mdd)) (void))
(let ([dd (empty-mdd)])
  (check-equal? (free-id-ddict-set*! dd i0 "0" i1 "1" i2 "2" i3 "3" i4 "4") (void))
  (check-equal? dd (mdd5)))
(let ([dd (mdd5)])
  (free-id-ddict-set*! dd i4 4 i3 3 i0 0 i1 1 i2 2)
  (check-equal? dd (mutable-free-id-ddict i0 0 i1 1 i2 2 i3 3 i4 4)))
(let ([dd (mdd5)])
  (free-id-ddict-set*! dd i4 i4 i3 i3 i0 i0 i2 i2 i1 i1)
  (check-equal? dd (mutable-free-id-ddict i0 i0 i1 i1 i2 i2 i3 i3 i4 i4))
  (check-equal? (free-id-ddict-keys dd) id-list1)
  (check-equal? (free-id-ddict-values dd) id-list1)
  (free-id-ddict-set*! dd i5 i5 i6 i6)
  (check-equal? (free-id-ddict-keys dd) (list* i6 i5 id-list1))
  (check-equal? (free-id-ddict-values dd) (list* i6 i5 id-list1)))
(check-exn #rx"free-id-ddict-set\\*!: contract violation"
           (λ () (free-id-ddict-set*! 42)))
(check-exn #rx"free-id-ddict-set\\*!: contract violation"
           (λ () (free-id-ddict-set*! dd5)))
(check-exn #rx"free-id-ddict-set\\*!: contract violation"
           (λ () (free-id-ddict-set*! (empty-mdd) 1 1 2 2 3)))

;; - - - - - - - - - - - -
;; free-id-ddict-update
;; - - - - - - - - - - - -
(check-equal? (free-id-ddict-update (free-id-ddict i5 41) i5 add1) (free-id-ddict i5 42))
(let ([dd (free-id-ddict-update (free-id-ddict-update (free-id-ddict i5 41) i5 add1)
                        i6 add1 40)])
  (check-equal? (free-id-ddict-keys dd) (list i6 i5)))
(check-equal? (free-id-ddict-update empty-dd i5 add1 41) (free-id-ddict i5 42))
(check-exn #rx"free-id-ddict-update: contract violation"
           (λ () (free-id-ddict-update 42 #'i42 add1)))
(check-exn #rx"free-id-ddict-update: contract violation"
           (λ () (free-id-ddict-update (free-id-ddict i5 41) 42 add1)))
(check-exn #rx"free-id-ddict-update: contract violation"
           (λ () (free-id-ddict-update (free-id-ddict i5 41) i5 42)))

;; - - - - - - - - - - - -
;; free-id-ddict-update!
;; - - - - - - - - - - - -
(let ([dd (mutable-free-id-ddict i5 41)])
  (check-equal? (free-id-ddict-ref dd i5) 41)
  (check-equal? (free-id-ddict-update! dd i5 add1) (void))
  (check-equal? (free-id-ddict-ref dd i5) 42)
  (check-equal? (free-id-ddict-count dd) 1)
  (check-equal? dd (mutable-free-id-ddict i5 42))
  (check-equal? (free-id-ddict-update! dd i6 add1 40) (void))
  (check-equal? (free-id-ddict-ref dd i6) 41)
  (check-equal? (free-id-ddict-ref dd i5) 42)
  (check-equal? (free-id-ddict-count dd) 2)
  (check-equal? dd (mutable-free-id-ddict i6 41 i5 42))
  (check-equal? (free-id-ddict-keys dd) (list i6 i5))
  (check-exn #rx"free-id-ddict-update!: contract violation"
             (λ () (free-id-ddict-update! 42 #'i42 add1)))
  (check-exn #rx"free-id-ddict-update!: contract violation"
             (λ () (free-id-ddict-update! (mutable-free-id-ddict #'i42 41) 42 add1)))
  (check-exn #rx"free-id-ddict-update!: contract violation"
             (λ () (free-id-ddict-update! (mutable-free-id-ddict #'i42 41) #'i42 42))))

;; - - - - - - - - - - - -
;; free-id-ddict-clear
;; - - - - - - - - - - - -
(let ([dd (free-id-ddict-clear (free-id-ddict i1 i1 i2 i2))])
  (check-true (free-id-ddict-empty? dd))
  (check-true (free-id-ddict? dd))
  (check-true (immutable-free-id-ddict? dd)))

;; - - - - - - - - - - - -
;; free-id-ddict-clear!
;; - - - - - - - - - - - -
(let ([dd (empty-mdd)])
  (check-equal? (free-id-ddict-clear! dd) (void))
  (check-equal? dd (empty-mdd)) 
  (check-true (free-id-ddict? dd))
  (check-true (mutable-free-id-ddict? dd))
  (free-id-ddict-set*! dd i1 i1 i2 i2 i3 i3)
  (check-equal? dd (mutable-free-id-ddict i1 i1 i2 i2 i3 i3))
  (check-equal? (free-id-ddict-clear! dd) (void))
  (check-equal? dd (empty-mdd)))
(check-exn #rx"free-id-ddict-clear!: contract violation"
           (λ () (free-id-ddict-clear! 42)))
(check-exn #rx"free-id-ddict-clear!: contract violation"
           (λ () (free-id-ddict-clear! dd5)))

;; - - - - - - - - - - - -
;; free-id-ddict-copy
;; - - - - - - - - - - - -
(let* ([dd0 (mutable-free-id-ddict i1 i1 i2 i2)]
       [dd1 (mutable-free-id-ddict i1 i1)]
       [dd2 (free-id-ddict-copy dd1)]
       [_ (free-id-ddict-remove! dd0 i2)]
       [dd3 (free-id-ddict-copy dd0)])
  (check-equal? dd1 dd2)
  (check-equal? dd1 dd3)
  (check-false (eq? dd1 dd2))
  (free-id-ddict-remove! dd1 i1)
  (check-false (equal? dd1 dd2)))
(check-exn #rx"free-id-ddict-copy: contract violation"
           (λ () (free-id-ddict-copy dd5)))

;; - - - - - - - - - - - -
;; free-id-ddict-copy-clear
;; - - - - - - - - - - - -
(let* ([dd0 (free-id-ddict-copy-clear
             (free-id-ddict-remove (free-id-ddict i1 i1 i2 i2) i2))]
       [dd1 (free-id-ddict i1 i1)]
       [dd2 (free-id-ddict-copy-clear dd1)])
  (check-true (free-id-ddict? dd0))
  (check-true (free-id-ddict? dd2))
  (check-true (immutable-free-id-ddict? dd0))
  (check-true (immutable-free-id-ddict? dd2))
  (check-true (free-id-ddict-empty? dd0))
  (check-true (free-id-ddict-empty? dd2))
  (check-false (free-id-ddict-empty? dd1)))

(let* ([dd1 (mutable-free-id-ddict i1 i1)]
       [dd2 (free-id-ddict-copy-clear dd1)])
  (check-true (mutable-free-id-ddict? dd2))
  (check-true (free-id-ddict-empty? dd2))
  (check-false (free-id-ddict-empty? dd1)))

;; - - - - - - - - - - - - - - - -
;; free-id-ddict-compact? / free-id-ddict-compact!
;; - - - - - - - - - - - - - - - -
(let ([dd (mutable-free-id-ddict i1 i1 i2 i2 i3 i3 i4 i4)])
  (free-id-ddict-remove! dd i1)
  (free-id-ddict-remove! dd i2)
  (check-false (free-id-ddict-compact? dd))
  (check-equal? dd (mutable-free-id-ddict i3 i3 i4 i4))
  (free-id-ddict-compact! dd)
  (check-true (free-id-ddict-compact? dd))
  (check-equal? dd (mutable-free-id-ddict i3 i3 i4 i4)))
(let ([dd (free-id-ddict-remove
           (free-id-ddict-remove
            (free-id-ddict i1 i1 i2 i2 i3 i3 i4 i4)
            i1)
           i2)])
  (check-equal? dd (free-id-ddict i3 i3 i4 i4))
  (check-false (free-id-ddict-compact? dd))
  (let ([dd (free-id-ddict-compact dd)])
    (check-equal? dd (free-id-ddict i3 i3 i4 i4))
    (check-true (free-id-ddict-compact? dd))))

;; - - - - - - - - - - - -
;; free-id-ddict-map
;; - - - - - - - - - - - -
(check-equal? (free-id-ddict-map dd5 cons)
              `((,i4 . "4")
                (,i3 . "3")
                (,i2 . "2")
                (,i1 . "1")
                (,i0 . "0")))
(check-equal? (free-id-ddict-map (mdd5) cons)
              `((,i4 . "4")
                (,i3 . "3")
                (,i2 . "2")
                (,i1 . "1")
                (,i0 . "0")))
(check-exn #rx"free-id-ddict-map: contract violation"
           (λ () (free-id-ddict-map 42 cons)))
(check-exn #rx"free-id-ddict-map: contract violation"
           (λ () (free-id-ddict-map dd5 42)))
(check-exn #rx"free-id-ddict-map: contract violation"
           (λ () (free-id-ddict-map dd5 add1)))

;; - - - - - - - - - - - -
;; free-id-ddict-for-each
;; - - - - - - - - - - - -
(let* ([b (box 0)]
       [add1! (λ _ (set-box! b (add1 (unbox b))))])
  (check-equal? (free-id-ddict-for-each dd5 add1!) (void))
  (check-equal? (unbox b) 5))
(let* ([b (box 0)]
       [add1! (λ _ (set-box! b (add1 (unbox b))))])
  (check-equal? (free-id-ddict-for-each (mdd5) add1!) (void))
  (check-equal? (unbox b) 5))
(check-exn #rx"free-id-ddict-for-each: contract violation"
           (λ () (free-id-ddict-for-each 42 cons)))
(check-exn #rx"free-id-ddict-for-each: contract violation"
           (λ () (free-id-ddict-for-each dd5 42)))
(check-exn #rx"free-id-ddict-for-each: contract violation"
           (λ () (free-id-ddict-for-each dd5 add1)))

;in-free-id-ddict // in-dict
(check-equal? (for/list ([(k v) (in-free-id-ddict dd5)])
                (cons k v))
              (free-id-ddict->list dd5))
(check-equal? (for/list ([(k v) (in-free-id-ddict (mdd5))])
                (cons k v))
              (free-id-ddict->list dd5))
(check-equal? (for/list ([(k v) (in-free-id-ddict
                                 (free-id-ddict-remove
                                  (free-id-ddict-remove dd5 i0) i1))])
                (cons k v))
              (free-id-ddict->list (free-id-ddict-remove
                                    (free-id-ddict-remove dd5 i0) i1)))
(check-equal? (for/list ([(k v) (in-free-id-ddict
                                 (free-id-ddict-remove
                                  (free-id-ddict-remove dd5 i4) i3))])
                (cons k v))
              (free-id-ddict->list (free-id-ddict-remove
                                    (free-id-ddict-remove dd5 i4) i3)))
(check-equal? (for/list ([(k v) ((λ () (in-free-id-ddict dd5)))])
                (cons k v))
              (free-id-ddict->list dd5))
(check-exn #rx"in-free-id-ddict: contract violation"
           (λ () (check-equal? (for/list ([(k v) (in-free-id-ddict "foo")])
                                 (cons k v))
                               (free-id-ddict->list dd5))))
(check-exn #rx"in-free-id-ddict: contract violation"
           (λ () (check-equal? (for/list ([(k v) ((λ () (in-free-id-ddict "foo")))])
                                 (cons k v))
                               (free-id-ddict->list dd5))))

;in-free-id-ddict-keys // in-dict-keys
(check-equal? (for/list ([k (in-free-id-ddict-keys dd5)])
                k)
              (free-id-ddict-keys dd5))
(check-equal? (for/list ([k (in-free-id-ddict-keys (mdd5))])
                k)
              (free-id-ddict-keys dd5))
(check-equal? (for/list ([k (in-free-id-ddict-keys
                             (free-id-ddict-remove
                              (free-id-ddict-remove dd5 i0) i1))])
                k)
              (free-id-ddict-keys
               (free-id-ddict-remove
                (free-id-ddict-remove dd5 i0) i1)))
(check-equal? (for/list ([k (in-free-id-ddict-keys
                             (free-id-ddict-remove
                              (free-id-ddict-remove dd5 i4) i3))])
                k)
              (free-id-ddict-keys
               (free-id-ddict-remove
                (free-id-ddict-remove dd5 i4) i3)))
(check-equal? (for/list ([k ((λ () (in-free-id-ddict-keys dd5)))])
                k)
              (free-id-ddict-keys dd5))
(check-exn #rx"in-free-id-ddict-keys: contract violation"
           (λ () (check-equal? (for/list ([k (in-free-id-ddict-keys '())])
                                 k)
                               (free-id-ddict->list dd5))))
(check-exn #rx"in-free-id-ddict-keys: contract violation"
           (λ () (check-equal? (for/list ([k ((λ () (in-free-id-ddict-keys '())))])
                                 k)
                               (free-id-ddict->list dd5))))


;in-free-id-ddict-values // in-dict-values
(check-equal? (for/list ([v (in-free-id-ddict-values dd5)])
                v)
              (free-id-ddict-values dd5))
(check-equal? (for/list ([v (in-free-id-ddict-values (mdd5))])
                v)
              (free-id-ddict-values dd5))
(check-equal? (for/list ([k (in-free-id-ddict-values
                             (free-id-ddict-remove
                              (free-id-ddict-remove dd5 i4) i3))])
                k)
              (free-id-ddict-values
               (free-id-ddict-remove
                (free-id-ddict-remove dd5 i4) i3)))
(check-equal? (for/list ([k (in-free-id-ddict-values
                             (free-id-ddict-remove
                              (free-id-ddict-remove dd5 i0) i1))])
                k)
              (free-id-ddict-values
               (free-id-ddict-remove
                (free-id-ddict-remove dd5 i0) i1)))
(check-equal? (for/list ([v ((λ () (in-free-id-ddict-values dd5)))])
                v)
              (free-id-ddict-values dd5))
(check-exn #rx"in-free-id-ddict-values: contract violation"
           (λ () (check-equal? (for/list ([v (in-free-id-ddict-values '())])
                                 v)
                               (free-id-ddict->list dd5))))
(check-exn #rx"in-free-id-ddict-values: contract violation"
           (λ () (check-equal? (for/list ([v ((λ () (in-free-id-ddict-values '())))])
                                 v)
                               (free-id-ddict->list dd5))))

(define-syntax-rule (for-tests for expected)
  (begin
    (check-exn (regexp (format "~a: contract violation" 'for))
               (lambda () (for ([k '(1 2 3)]) (values k '()))))
    (check-equal? (for ([n (in-range (length id-list1))]
                        [i (reverse id-list1)])
                    (values i (number->string n)))
                  expected)
    (check-equal? (for ([n (in-range (length id-list1))]
                        [i (reverse id-list1)])
                    #:break (> n 4)
                    (values i (number->string n)))
                  expected)
    (check-equal? (for ([n (in-range (* 2 (length id-list1)))]
                        [v (reverse (append id-list1 id-list1))])
                    (values v (number->string (modulo n 5)))) expected)))

(for-tests for/free-id-ddict dd5)
(for-tests for/mutable-free-id-ddict (mdd5))


;; check hash code
(check-equal? (equal-hash-code (free-id-ddict i1 i1 i2 i2 i3 i3))
              (equal-hash-code (free-id-ddict i3 i3 i1 i1 i2 i2)))
(check-equal? (equal-hash-code (mutable-free-id-ddict i1 i1 i2 i2 i3 i3))
              (equal-hash-code (mutable-free-id-ddict i3 i3 i1 i1 i2 i2)))


