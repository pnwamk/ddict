#lang racket/base

(require (for-syntax racket/base)
         rackunit
         racket/dict
         "../../data/ddict.rkt")

(define failsym (gensym 'fail))

;; - - - - - - - - - - - -
;; constructors
;; - - - - - - - - - - - -
(define empty-dd (ddict))
(define dd5 (ddict 0 "0" 1 "1" 2 "2" 3 "3" 4 "4"))
(check-true (ddict-equal? dd5))
(define dd5eqv (ddicteqv 0 "0" 1 "1" 2 "2" 3 "3" 4 "4"))
(check-true (ddict-eqv? dd5eqv))
(define dd5eq (ddicteq 0 "0" 1 "1" 2 "2" 3 "3" 4 "4"))
(check-true (ddict-eq? dd5eq))
(check-exn #rx"ddict: contract violation"
           (λ () (ddict 0 "0" 1 "1" 2)))
(define (empty-mdd) (mutable-ddict))
(define (mdd5) (mutable-ddict 0 "0" 1 "1" 2 "2" 3 "3" 4 "4"))
(check-true (ddict-equal? (mdd5)))
(define (mdd5eqv) (mutable-ddicteqv 0 "0" 1 "1" 2 "2" 3 "3" 4 "4"))
(check-true (ddict-eqv? (mdd5eqv)))
(define (mdd5eq) (mutable-ddicteq 0 "0" 1 "1" 2 "2" 3 "3" 4 "4"))
(check-true (ddict-eq? (mdd5eq)))
(check-exn #rx"ddict: contract violation"
           (λ () (mutable-ddict 0 "0" 1 "1" 2)))

;; - - - - - - - - - - - -
;; ddict?
;; - - - - - - - - - - - -
(check-false (ddict? #hash()))
(check-false (ddict? 42))
(check-true (ddict? empty-dd))
(check-true (ddict? dd5))
(check-true (ddict? (empty-mdd)))
(check-true (ddict? (mdd5)))

;; - - - - - - - - - - - -
;; immutable-ddict?
;; - - - - - - - - - - - -
(check-true (immutable-ddict? empty-dd))
(check-true (immutable-ddict? dd5))
(check-false (immutable-ddict? (empty-mdd)))
(check-false (immutable-ddict? (mdd5)))

;; - - - - - - - - - - - -
;; mutable-ddict?
;; - - - - - - - - - - - -
(check-false (mutable-ddict? empty-dd))
(check-false (mutable-ddict? dd5))
(check-true (mutable-ddict? (empty-mdd)))
(check-true (mutable-ddict? (mdd5)))

;; - - - - - - - - - - - -
;; ddict-equal?
;; - - - - - - - - - - - -
(check-true (ddict-equal? (ddict)))
(check-true (ddict-equal? (mutable-ddict)))
(check-false (ddict-equal? (ddicteqv)))
(check-false (ddict-equal? (mutable-ddicteqv)))
(check-false (ddict-equal? (ddicteq)))
(check-false (ddict-equal? (mutable-ddicteq)))
(check-exn #rx"ddict-equal\\?: contract violation"
           (λ () (ddict-equal? 42)))

;; - - - - - - - - - - - -
;; ddict-eqv?
;; - - - - - - - - - - - -
(check-false (ddict-eqv? (ddict)))
(check-false (ddict-eqv? (mutable-ddict)))
(check-true (ddict-eqv? (ddicteqv)))
(check-true (ddict-eqv? (mutable-ddicteqv)))
(check-false (ddict-eqv? (ddicteq)))
(check-false (ddict-eqv? (mutable-ddicteq)))
(check-exn #rx"ddict-eqv\\?: contract violation"
           (λ () (ddict-eqv? 42)))

;; - - - - - - - - - - - -
;; ddict-eq?
;; - - - - - - - - - - - -
(check-false (ddict-eq? (ddict)))
(check-false (ddict-eq? (mutable-ddict)))
(check-false (ddict-eq? (ddicteqv)))
(check-false (ddict-eq? (mutable-ddicteqv)))
(check-true (ddict-eq? (ddicteq)))
(check-true (ddict-eq? (mutable-ddicteq)))
(check-exn #rx"ddict-eq\\?: contract violation"
           (λ () (ddict-eq? 42)))

;; - - - - - - - - - - - -
;; equality
;; - - - - - - - - - - - -
(check-equal? (ddict 1 2 3 4) (ddict 3 4 1 2))
(check-equal? (ddicteqv 1 2 3 4) (ddicteqv 3 4 1 2))
(check-equal? (ddicteq 1 2 3 4) (ddicteq 3 4 1 2))
(check-false (equal? (ddict 1 2 3 4) (ddicteqv 3 4 1 2)))
(check-false (equal? (ddicteqv 1 2 3 4) (ddicteq 3 4 1 2)))
(check-false (equal? (ddict 1 2 3 4) (ddicteq 3 4 1 2)))
(check-equal? (mutable-ddict 1 2 3 4) (mutable-ddict 3 4 1 2))
(check-equal? (mutable-ddicteqv 1 2 3 4) (mutable-ddicteqv 3 4 1 2))
(check-equal? (mutable-ddicteq 1 2 3 4) (mutable-ddicteq 3 4 1 2))
(check-false (equal? (mutable-ddict 1 2 3 4) (mutable-ddicteqv 3 4 1 2)))
(check-false (equal? (mutable-ddicteqv 1 2 3 4) (mutable-ddicteq 3 4 1 2)))
(check-false (equal? (mutable-ddict 1 2 3 4) (mutable-ddicteq 3 4 1 2)))

;; - - - - - - - - - - - -
;; ddict-count
;; - - - - - - - - - - - -
(check-equal? (ddict-count empty-dd) 0)
(check-equal? (dict-count empty-dd) 0)
(check-equal? (ddict-count dd5) 5)
(check-equal? (dict-count dd5) 5)
(check-equal? (ddict-count (empty-mdd)) 0)
(check-equal? (ddict-count (mdd5)) 5)
(check-equal? (dict-count (empty-mdd)) 0)
(check-equal? (dict-count (mdd5)) 5)
(check-exn #rx"ddict-count: contract violation"
           (λ () (ddict-count "not a ddict")))

;; - - - - - - - - - - - -
;; ddict-ref (immutable)
;; - - - - - - - - - - - -
(for ([k (in-list '(4 3 2 1 0))]
      [v (in-list '("4" "3" "2" "1" "0"))])
  (check-equal? (ddict-ref dd5 k) v)
  (check-equal? (dict-ref dd5 k) v))
(check-equal? (ddict-ref dd5 42 failsym) failsym)

;; - - - - - - - - - - - -
;; ddict-ref (mutable)
;; - - - - - - - - - - - -
(let ([dd (mdd5)])
  (for ([k (in-list '(4 3 2 1 0))]
        [v (in-list '("4" "3" "2" "1" "0"))])
    (check-equal? (ddict-ref dd k) v)
    (check-equal? (dict-ref dd k) v))
  (check-equal? (ddict-ref dd5 42 failsym) failsym))


;; - - - - - - - - - - - -
;; ddict-set
;; - - - - - - - - - - - -
(check-equal? (for/fold ([dd empty-dd])
                        ([k (in-list '(4 3 2 1 0))]
                         [v (in-list '("4" "3" "2" "1" "0"))])
                (if (even? k)
                    (ddict-set dd k v)
                    (dict-set dd k v)))
              dd5)
(check-exn #rx"ddict-set: contract violation"
           (λ () (ddict-set 42 42 42)))
(check-exn #rx"ddict-set: contract violation"
           (λ () (ddict-set (mdd5) 42 42)))
(let ([dd (for/fold ([dd dd5])
                    ([v (in-list '("4" "3" "2" "1" "0"))])
            (ddict-set dd v v))])
  (check-equal? (ddict-count dd) 10)
  (for ([k (in-list '(4 3 2 1 0))]
        [v (in-list '("4" "3" "2" "1" "0"))])
    (check-equal? (ddict-ref dd k) v)
    (check-equal? (ddict-ref dd v) v))
  (check-equal? (ddict-keys dd)
                (append (reverse '("4" "3" "2" "1" "0")) '(4 3 2 1 0)))
  (check-equal? (dict-keys dd)
                (append (reverse '("4" "3" "2" "1" "0")) '(4 3 2 1 0)))
  (check-equal? (ddict-values dd)
                (append (reverse '("4" "3" "2" "1" "0")) '("4" "3" "2" "1" "0")))
  (check-equal? (dict-values dd)
                (append (reverse '("4" "3" "2" "1" "0")) '("4" "3" "2" "1" "0")))
  (check-equal? (ddict->list dd)
                (map cons
                     (append (reverse '("4" "3" "2" "1" "0")) '(4 3 2 1 0))
                     (append (reverse '("4" "3" "2" "1" "0")) '("4" "3" "2" "1" "0"))))
  (let ([dd (for/fold ([dd dd])
                      ([k (in-list '(4 3 2 1 0))])
              (ddict-set dd k k))])
    (check-equal? (ddict-count dd) 10)
    (for ([k (in-list '(4 3 2 1 0))]
          [v (in-list '("4" "3" "2" "1" "0"))])
      (check-equal? (ddict-ref dd k) k)
      (check-equal? (ddict-ref dd v) v))
    (check-equal? (ddict-keys dd)
                  (append (reverse '("4" "3" "2" "1" "0")) '(4 3 2 1 0)))
    (check-equal? (ddict-values dd)
                  (append (reverse '("4" "3" "2" "1" "0")) '(4 3 2 1 0)))
    (check-equal? (ddict->list dd)
                  (map cons
                       (append (reverse '("4" "3" "2" "1" "0")) '(4 3 2 1 0))
                       (append (reverse '("4" "3" "2" "1" "0")) '(4 3 2 1 0))))))


;; - - - - - - - - - - - -
;; ddict-remove
;; - - - - - - - - - - - -
(let* ([dd dd5]
       [dd (ddict-remove dd 3)]
       [_ (begin (check-equal? (ddict-count dd) 4)
                 (check-false (ddict-compact? dd)))]
       [dd (begin (ddict-remove dd 1)
                  (dict-remove dd 1))]
       [_ (begin (check-equal? (ddict-count dd) 3)
                 (check-false (ddict-compact? dd)))]
       [dd (dict-remove dd 0)]
       [_ (begin (check-equal? (ddict-count dd) 2)
                 (check-true (ddict-compact? dd)))])
  (check-exn #rx"ddict-remove: contract violation"
             (λ () (ddict-remove 42 42)))
  (check-exn #rx"ddict-remove: contract violation"
             (λ () (ddict-remove (mdd5) 42)))
  (check-equal? (ddict-keys dd) '(4 2))
  (check-equal? (ddict-values dd) '("4" "2"))
  (check-equal? dd (ddict 4 "4" 2 "2"))
  (check-equal? (ddict-remove (ddict-remove dd 4) 2) empty-dd))


;; - - - - - - - - - - - -
;; ddict-set!
;; - - - - - - - - - - - -
(let ([dd (mdd5)])
  (for ([v (in-list '("4" "3" "2" "1" "0"))])
    (if (even? (string->number v))
        (check-equal? (ddict-set! dd v v) (void))
        (check-equal? (dict-set! dd v v) (void))))
  (check-equal? (ddict-count dd) 10)
  (check-exn #rx"ddict-set!: contract violation"
             (λ () (ddict-set! 42 42 42)))
  (check-exn #rx"ddict-set!: contract violation"
             (λ () (ddict-set! dd5 42 42)))
  (for ([k (in-list '(4 3 2 1 0))]
        [v (in-list '("4" "3" "2" "1" "0"))])
    (check-equal? (ddict-ref dd k) v)
    (check-equal? (ddict-ref dd v) v)))
(let ([dd (mdd5)])
  (for ([v (in-list '("4" "3" "2" "1" "0"))])
    (ddict-set! dd v v))
  (check-equal? (ddict-count dd) 10)
  (for ([k (in-list '(4 3 2 1 0))]
        [v (in-list '("4" "3" "2" "1" "0"))])
    (check-equal? (ddict-ref dd k) v)
    (check-equal? (ddict-ref dd v) v))
  (check-equal? (ddict-keys dd)
                (append (reverse '("4" "3" "2" "1" "0")) '(4 3 2 1 0)))
  (check-equal? (ddict-values dd)
                (append (reverse '("4" "3" "2" "1" "0")) '("4" "3" "2" "1" "0")))
  (check-equal? (ddict->list dd)
                (map cons
                     (append (reverse '("4" "3" "2" "1" "0")) '(4 3 2 1 0))
                     (append (reverse '("4" "3" "2" "1" "0")) '("4" "3" "2" "1" "0"))))
  (for ([k (in-list '(4 3 2 1 0))])
    (ddict-set! dd k k))
  (check-equal? (ddict-count dd) 10)
  (for ([k (in-list '(4 3 2 1 0))]
        [v (in-list '("4" "3" "2" "1" "0"))])
    (check-equal? (ddict-ref dd k) k)
    (check-equal? (ddict-ref dd v) v))
  (check-equal? (ddict-keys dd)
                (append (reverse '("4" "3" "2" "1" "0")) '(4 3 2 1 0)))
  (check-equal? (ddict-values dd)
                (append (reverse '("4" "3" "2" "1" "0")) '(4 3 2 1 0)))
  (check-equal? (ddict->list dd)
                (map cons
                     (append (reverse '("4" "3" "2" "1" "0")) '(4 3 2 1 0))
                     (append (reverse '("4" "3" "2" "1" "0")) '(4 3 2 1 0)))))

;; - - - - - - - - - - - -
;ddict-remove!
;; - - - - - - - - - - - -
(let ([dd (mdd5)])
  (check-equal? (ddict-remove! dd 3) (void))
  (check-equal? (ddict-count dd) 4)
  (check-false (ddict-compact? dd))
  (ddict-remove! dd 1)
  (ddict-remove! dd 1)
  (check-equal? (ddict-count dd) 3)
  (check-false (ddict-compact? dd))
  (dict-remove! dd 0)
  (check-equal? (ddict-count dd) 2)
  (check-true (ddict-compact? dd))
  (check-equal? (ddict-keys dd) '(4 2))
  (check-equal? (ddict-values dd) '("4" "2"))
  (check-equal? dd (mutable-ddict 4 "4" 2 "2"))
  (ddict-remove! dd 4)
  (ddict-remove! dd 2)
  (check-true (ddict-empty? dd))
  (check-exn #rx"ddict-remove!: contract violation"
             (λ () (ddict-remove! 42 42)))
  (check-exn #rx"ddict-remove!: contract violation"
             (λ () (ddict-remove! dd5 42))))


;; - - - - - - - - - - - -
;; ddict-keys
;; - - - - - - - - - - - -
(check-equal? (ddict-keys empty-dd) '())
(check-equal? (ddict-keys dd5) '(4 3 2 1 0))
(check-equal? (ddict-keys (empty-mdd)) '())
(check-equal? (ddict-keys (mdd5)) '(4 3 2 1 0))
(check-exn #rx"ddict-keys: contract violation"
           (λ () (ddict-keys 42)))

;; - - - - - - - - - - - -
;; ddict-values
;; - - - - - - - - - - - -
(check-equal? (ddict-values empty-dd) '())
(check-equal? (ddict-values dd5) '("4" "3" "2" "1" "0"))
(check-equal? (ddict-values (empty-mdd)) '())
(check-equal? (ddict-values (mdd5)) '("4" "3" "2" "1" "0"))
(check-exn #rx"ddict-values: contract violation"
           (λ () (ddict-values 42)))

;; - - - - - - - - - - - -
;; ddict->list
;; - - - - - - - - - - - -
(check-equal? (ddict->list empty-dd) '())
(check-equal? (ddict->list dd5) (map cons '(4 3 2 1 0) '("4" "3" "2" "1" "0")))
(check-equal? (ddict->list (empty-mdd)) '())
(check-equal? (ddict->list (mdd5)) (map cons '(4 3 2 1 0) '("4" "3" "2" "1" "0")))
(check-exn #rx"ddict->list: contract violation"
           (λ () (ddict->list 42)))

;; - - - - - - - - - - - -
;; ddict-ref!
;; - - - - - - - - - - - -
(let ([dd (mdd5)])
  (for ([k (in-list '(4 3 2 1 0))]
        [v (in-list '("4" "3" "2" "1" "0"))])
    (check-equal? (ddict-ref! dd k (λ () (error 'oh-no!))) v))
  (check-equal? (ddict-ref! dd 5 "5") "5")
  (define b (box #f))
  (check-equal? (ddict-ref! dd 6 (λ () (set-box! b #t) "6")) "6")
  (check-equal? (unbox b) #t)
  (check-equal? (ddict-count dd) 7)
  (check-equal? (ddict->list dd) (map cons
                                      '(6 5 4 3 2 1 0)
                                      '("6" "5" "4" "3" "2" "1" "0"))))
(check-exn #rx"ddict-ref!: contract violation"
           (λ () (ddict-ref! 42 42 42)))
(check-exn #rx"ddict-ref!: contract violation"
           (λ () (ddict-ref! dd5 42 42)))

;; - - - - - - - - - - - -
;; ddict-has-key?
;; - - - - - - - - - - - -
(check-equal? (ddict-has-key? empty-dd #t) #f)
(check-equal? (ddict-has-key? dd5 0) #t)
(check-equal? (ddict-has-key? dd5 5) #f)
(check-equal? (ddict-has-key? (empty-mdd) #t) #f)
(check-equal? (ddict-has-key? (mdd5) 0) #t)
(check-equal? (ddict-has-key? (mdd5) 5) #f)
(check-exn #rx"ddict-has-key\\?: contract violation"
           (λ () (ddict-has-key? 42 42)))


;; - - - - - - - - - - - -
;; ddict-empty?
;; - - - - - - - - - - - -
(check-true (ddict-empty? empty-dd))
(check-false (ddict-empty? dd5))
(check-true (ddict-empty? (empty-mdd)))
(check-false (ddict-empty? (mdd5)))
(check-exn #rx"ddict-empty\\?: contract violation"
           (λ () (ddict-empty? 42)))


;; - - - - - - - - - - - - - - - - - - - - - - - -
;; constructor tests (e.g. ddict, ddicteqv, etc)
;; - - - - - - - - - - - - - - - - - - - - - - - -
(define-syntax (constructor-test stx)
  (syntax-case stx ()
    [(_ constructor comparison-pred? pred?)
     (syntax/loc stx
       (let ([mt (constructor)]
             [dd (constructor 1 "1" 2 "2" 3 "3")])
         (check-true (pred? mt))
         (check-true (comparison-pred? mt))
         (check-true (pred? dd))
         (check-true (comparison-pred? dd))
         (check-true (ddict-empty? mt))
         (check-equal? (ddict-count dd) 3)
         (check-equal? (ddict-ref dd 1) "1")
         (check-equal? (ddict-ref dd 2) "2")
         (check-equal? (ddict-ref dd 3) "3")))]))


(constructor-test ddict ddict-equal? immutable-ddict?)
(constructor-test ddicteqv ddict-eqv? immutable-ddict?)
(constructor-test ddicteq ddict-eq? immutable-ddict?)
(constructor-test mutable-ddict ddict-equal? mutable-ddict?)
(constructor-test mutable-ddicteqv ddict-eqv? mutable-ddict?)
(constructor-test mutable-ddicteq ddict-eq? mutable-ddict?)


;; - - - - - - - - - - - - - - - - - - - - - - - -
;; make-* tests (e.g. ddict, ddicteqv, etc)
;; - - - - - - - - - - - - - - - - - - - - - - - -
(define-syntax (make/alist-test stx)
  (syntax-case stx ()
    [(_ make comparison-pred? pred?)
     (syntax/loc stx
       (let ([_1 (check-exn #rx"contract violation" (λ () (make 42)))]
             [_2 (check-exn #rx"contract violation" (λ () (make '((1 . 2) (3 . 4) . (5 . 6)))))]
             [mt (make)]
             [dd (make '((1 . "1") (2 . "2") (3 . "3") (3 . "3")))])
         (check-true (pred? mt))
         (check-true (comparison-pred? mt))
         (check-true (pred? dd))
         (check-true (comparison-pred? dd))
         (check-true (ddict-empty? mt))
         (check-equal? (ddict-count dd) 3)
         (check-equal? (ddict-ref dd 1) "1")
         (check-equal? (ddict-ref dd 2) "2")
         (check-equal? (ddict-ref dd 3) "3")))]))

(make/alist-test make-ddict ddict-equal? immutable-ddict?)
(make/alist-test make-ddicteqv ddict-eqv? immutable-ddict?)
(make/alist-test make-ddicteq ddict-eq? immutable-ddict?)
(make/alist-test make-mutable-ddict ddict-equal? mutable-ddict?)
(make/alist-test make-mutable-ddicteqv ddict-eqv? mutable-ddict?)
(make/alist-test make-mutable-ddicteq ddict-eq? mutable-ddict?)


;; - - - - - - - - - - - -
;; ddict-set*
;; - - - - - - - - - - - -
(check-equal? (ddict-set* empty-dd) empty-dd)
(check-equal? (ddict-set* empty-dd 0 "0" 1 "1" 2 "2" 3 "3" 4 "4")
              dd5)
(check-equal? (ddict-set* dd5 4 4 3 3 0 0 1 1 2 2)
              (ddict 0 0 1 1 2 2 3 3 4 4))
(check-equal? (ddict-keys (ddict-set* empty-dd 0 "0" 1 "1" 2 "2" 3 "3" 4 "4"))
              '(4 3 2 1 0))
(check-equal? (ddict-keys (ddict-set* dd5 4 4 3 3 0 0 1 1 2 2))
              '(4 3 2 1 0))
(check-equal? (ddict-keys (ddict-set* dd5 4 4 3 3 0 0 1 1 2 2 42 42))
              '(42 4 3 2 1 0))
(check-equal? (ddict-values (ddict-set* dd5 4 4 3 3 0 0 1 1 2 2 42 42))
              '(42 4 3 2 1 0))
(check-equal? (ddict-set* empty-dd 0 "0" 1 "1" 0 0 1 1)
              (ddict 0 0 1 1))
(check-exn #rx"ddict-set\\*: contract violation"
           (λ () (ddict-set* 42)))
(check-exn #rx"ddict-set\\*: contract violation"
           (λ () (ddict-set* (mdd5))))
(check-exn #rx"ddict-set\\*: contract violation"
           (λ () (ddict-set* empty-dd 1 1 2 2 3)))

;; - - - - - - - - - - - -
;; ddict-set*!
;; - - - - - - - - - - - -
(check-equal? (ddict-set*! (empty-mdd)) (void))
(let ([dd (empty-mdd)])
  (check-equal? (ddict-set*! dd 0 "0" 1 "1" 2 "2" 3 "3" 4 "4") (void))
  (check-equal? dd (mdd5)))
(let ([dd (mdd5)])
  (ddict-set*! dd 4 4 3 3 0 0 1 1 2 2)
  (check-equal? dd (mutable-ddict 0 0 1 1 2 2 3 3 4 4)))
(let ([dd (mdd5)])
  (ddict-set*! dd 4 4 3 3 0 0 2 2 1 1)
  (check-equal? dd (mutable-ddict 0 0 1 1 2 2 3 3 4 4))
  (check-equal? (ddict-keys dd) '(4 3 2 1 0))
  (check-equal? (ddict-values dd) '(4 3 2 1 0))
  (ddict-set*! dd 42 42 43 43)
  (check-equal? (ddict-keys dd) '(43 42 4 3 2 1 0))
  (check-equal? (ddict-values dd) '(43 42 4 3 2 1 0)))
(check-exn #rx"ddict-set\\*!: contract violation"
           (λ () (ddict-set*! 42)))
(check-exn #rx"ddict-set\\*!: contract violation"
           (λ () (ddict-set*! dd5)))
(check-exn #rx"ddict-set\\*!: contract violation"
           (λ () (ddict-set*! (empty-mdd) 1 1 2 2 3)))

;; - - - - - - - - - - - -
;; ddict-update
;; - - - - - - - - - - - -
(check-equal? (ddict-update (ddict 42 41) 42 add1) (ddict 42 42))
(let ([dd (ddict-update (ddict-update (ddict 42 41) 42 add1)
                        41 add1 40)])
  (check-equal? (ddict-keys dd) '(41 42)))
(check-equal? (ddict-update empty-dd 42 add1 41) (ddict 42 42))
(check-exn #rx"ddict-update: contract violation"
           (λ () (ddict-update 42 42 add1)))
(check-exn #rx"ddict-update: contract violation"
           (λ () (ddict-update (ddict 42 41) 42 42)))

;; - - - - - - - - - - - -
;; ddict-update!
;; - - - - - - - - - - - -
(let ([dd (mutable-ddict 42 41)])
  (check-equal? (ddict-ref dd 42) 41)
  (check-equal? (ddict-update! dd 42 add1) (void))
  (check-equal? (ddict-ref dd 42) 42)
  (check-equal? (ddict-count dd) 1)
  (check-equal? dd (mutable-ddict 42 42))
  (check-equal? (ddict-update! dd 41 add1 40) (void))
  (check-equal? (ddict-ref dd 41) 41)
  (check-equal? (ddict-ref dd 42) 42)
  (check-equal? (ddict-count dd) 2)
  (check-equal? dd (mutable-ddict 41 41 42 42))
  (check-equal? (ddict-keys dd) '(41 42))
  (check-exn #rx"ddict-update!: contract violation"
             (λ () (ddict-update! 42 42 add1)))
  (check-exn #rx"ddict-update!: contract violation"
             (λ () (ddict-update! (mutable-ddict 42 41) 42 42))))

;; - - - - - - - - - - - -
;; ddict-clear
;; - - - - - - - - - - - -
(let ([dd (ddict-clear (ddict 1 1 2 2))])
  (check-true (ddict-empty? dd))
  (check-true (ddict-equal? dd))
  (check-true (immutable-ddict? dd)))
(let ([dd (ddict-clear (ddicteqv 1 1 2 2))])
  (check-true (ddict-empty? dd))
  (check-true (ddict-eqv? dd))
  (check-true (immutable-ddict? dd)))
(let ([dd (ddict-clear (ddicteq 1 1 2 2))])
  (check-true (ddict-empty? dd))
  (check-true (ddict-eq? dd))
  (check-true (immutable-ddict? dd)))

;; - - - - - - - - - - - -
;; ddict-clear!
;; - - - - - - - - - - - -
(let ([dd (empty-mdd)])
  (check-equal? (ddict-clear! dd) (void))
  (check-equal? dd (empty-mdd))
  (check-true (ddict-equal? dd))
  (check-true (mutable-ddict? dd))
  (ddict-set*! dd 1 1 2 2 3 3)
  (check-equal? dd (mutable-ddict 1 1 2 2 3 3))
  (check-equal? (ddict-clear! dd) (void))
  (check-equal? dd (empty-mdd)))
(let ([dd (mutable-ddicteqv 1 2 3 4)])
  (check-equal? (ddict-clear! dd) (void))
  (check-true (ddict-empty? dd))
  (check-true (ddict-eqv? dd))
  (check-true (mutable-ddict? dd)))
(let ([dd (mutable-ddicteq 1 2 3 4)])
  (check-equal? (ddict-clear! dd) (void))
  (check-true (ddict-empty? dd))
  (check-true (ddict-eq? dd))
  (check-true (mutable-ddict? dd)))
(check-exn #rx"ddict-clear!: contract violation"
           (λ () (ddict-clear! 42)))
(check-exn #rx"ddict-clear!: contract violation"
           (λ () (ddict-clear! dd5)))

;; - - - - - - - - - - - -
;; ddict-copy
;; - - - - - - - - - - - -
(let* ([dd0 (mutable-ddict 1 1 2 2)]
       [dd1 (mutable-ddict 1 1)]
       [dd2 (ddict-copy dd1)]
       [_ (ddict-remove! dd0 2)]
       [dd3 (ddict-copy dd0)])
  (check-equal? dd1 dd2)
  (check-equal? dd1 dd3)
  (check-false (eq? dd1 dd2))
  (ddict-remove! dd1 1)
  (check-false (equal? dd1 dd2)))
(check-exn #rx"ddict-copy: contract violation"
           (λ () (ddict-copy dd5)))

;; - - - - - - - - - - - -
;; ddict-copy-clear
;; - - - - - - - - - - - -
(let* ([dd0 (ddict-copy-clear (ddict-remove (ddict 1 1 2 2) 2))]
       [dd1 (ddict 1 1)]
       [dd2 (ddict-copy-clear dd1)])
  (check-true (ddict? dd0))
  (check-true (ddict? dd2))
  (check-true (ddict-equal? dd0))
  (check-true (ddict-equal? dd2))
  (check-true (ddict-empty? dd0))
  (check-true (ddict-empty? dd2))
  (check-false (ddict-empty? dd1)))
(let* ([dd1 (ddicteqv 1 1)]
       [dd2 (ddict-copy-clear dd1)])
  (check-true (ddict? dd2))
  (check-true (ddict-eqv? dd2))
  (check-true (ddict-empty? dd2))
  (check-false (ddict-empty? dd1)))
(let* ([dd1 (mutable-ddict 1 1)]
       [dd2 (ddict-copy-clear dd1)])
  (check-true (mutable-ddict? dd2))
  (check-true (ddict-equal? dd2))
  (check-true (ddict-empty? dd2))
  (check-false (ddict-empty? dd1)))
(let* ([dd1 (mutable-ddicteqv 1 1)]
       [dd2 (ddict-copy-clear dd1)])
  (check-true (mutable-ddict? dd2))
  (check-true (ddict-eqv? dd2))
  (check-true (ddict-empty? dd2))
  (check-false (ddict-empty? dd1)))


;; - - - - - - - - - - - - - - - -
;; ddict-keys-subset?
;; - - - - - - - - - - - - - - - -
(define (immutable-ddict-tests dd other-dd1 other-dd2)
  (check-true (ddict-keys-subset? dd dd))
  (check-true (ddict-keys-subset? (ddict-remove dd 0) dd))
  (check-false (ddict-keys-subset? dd (ddict-remove dd 0))))

(immutable-ddict-tests dd5 dd5eqv dd5eq)
(immutable-ddict-tests dd5eqv dd5eq dd5)
(immutable-ddict-tests dd5eq dd5 dd5eqv)

(define (mutable-ddict-tests mkdd mkother-dd1 mkother-dd2)
  (define dd1 (mkdd))
  (define dd2 (mkdd))
  (define other-dd1 (mkother-dd1))
  (define other-dd2 (mkother-dd2))
  (check-true (ddict-keys-subset? dd1 dd2))
  (ddict-remove! dd1 0)
  (check-true (ddict-keys-subset? dd1 dd2))
  (check-false (ddict-keys-subset? dd2 dd1)))

(mutable-ddict-tests mdd5 mdd5eqv mdd5eq)
(mutable-ddict-tests mdd5eqv mdd5eq mdd5)
(mutable-ddict-tests mdd5eq mdd5 mdd5eqv)


;; - - - - - - - - - - - - - - - -
;; ddict-compact? / ddict-compact!
;; - - - - - - - - - - - - - - - -
(let ([dd (mutable-ddict 1 1 2 2 3 3 4 4)])
  (ddict-remove! dd 1)
  (ddict-remove! dd 2)
  (check-false (ddict-compact? dd))
  (check-equal? dd (mutable-ddict 3 3 4 4))
  (ddict-compact! dd)
  (check-true (ddict-compact? dd))
  (check-equal? dd (mutable-ddict 3 3 4 4)))
(let ([dd (ddict-remove
           (ddict-remove
            (ddict 1 1 2 2 3 3 4 4)
            1)
           2)])
  (check-equal? dd (ddict 3 3 4 4))
  (check-false (ddict-compact? dd))
  (let ([dd (ddict-compact dd)])
    (check-equal? dd (ddict 3 3 4 4))
    (check-true (ddict-compact? dd))))

;; - - - - - - - - - - - -
;; ddict-map
;; - - - - - - - - - - - -
(check-equal? (ddict-map dd5 cons) '((4 . "4")
                                     (3 . "3")
                                     (2 . "2")
                                     (1 . "1")
                                     (0 . "0")))
(check-equal? (ddict-map (mdd5) cons) '((4 . "4")
                                        (3 . "3")
                                        (2 . "2")
                                        (1 . "1")
                                        (0 . "0")))
(check-exn #rx"ddict-map: contract violation"
           (λ () (ddict-map 42 cons)))
(check-exn #rx"ddict-map: contract violation"
           (λ () (ddict-map dd5 42)))
(check-exn #rx"ddict-map: contract violation"
           (λ () (ddict-map dd5 add1)))

;; - - - - - - - - - - - -
;; ddict-for-each
;; - - - - - - - - - - - -
(let* ([b (box 0)]
       [add1! (λ _ (set-box! b (add1 (unbox b))))])
  (check-equal? (ddict-for-each dd5 add1!) (void))
  (check-equal? (unbox b) 5))
(let* ([b (box 0)]
       [add1! (λ _ (set-box! b (add1 (unbox b))))])
  (check-equal? (ddict-for-each (mdd5) add1!) (void))
  (check-equal? (unbox b) 5))
(check-exn #rx"ddict-for-each: contract violation"
           (λ () (ddict-for-each 42 cons)))
(check-exn #rx"ddict-for-each: contract violation"
           (λ () (ddict-for-each dd5 42)))
(check-exn #rx"ddict-for-each: contract violation"
           (λ () (ddict-for-each dd5 add1)))

;in-ddict // in-dict
(check-equal? (for/list ([(k v) (in-ddict dd5)])
                (cons k v))
              (ddict->list dd5))
(check-equal? (for/list ([(k v) (in-ddict (mdd5))])
                (cons k v))
              (ddict->list dd5))
(check-equal? (for/list ([(k v) (in-dict dd5)])
                (cons k v))
              (ddict->list dd5))
(check-equal? (for/list ([(k v) (in-dict (mdd5))])
                (cons k v))
              (ddict->list dd5))
(check-equal? (for/list ([(k v) (in-ddict (ddict-remove (ddict-remove dd5 0) 1))])
                (cons k v))
              (ddict->list (ddict-remove (ddict-remove dd5 0) 1)))
(check-equal? (for/list ([(k v) (in-ddict (ddict-remove (ddict-remove dd5 4) 3))])
                (cons k v))
              (ddict->list (ddict-remove (ddict-remove dd5 4) 3)))
(check-equal? (for/list ([(k v) ((λ () (in-ddict dd5)))])
                (cons k v))
              (ddict->list dd5))
(check-exn #rx"in-ddict: contract violation"
           (λ () (check-equal? (for/list ([(k v) (in-ddict "foo")])
                                 (cons k v))
                               (ddict->list dd5))))
(check-exn #rx"in-ddict: contract violation"
           (λ () (check-equal? (for/list ([(k v) ((λ () (in-ddict "foo")))])
                                 (cons k v))
                               (ddict->list dd5))))

;in-ddict-keys // in-dict-keys
(check-equal? (for/list ([k (in-ddict-keys dd5)])
                k)
              (ddict-keys dd5))
(check-equal? (for/list ([k (in-ddict-keys (mdd5))])
                k)
              (ddict-keys dd5))
(check-equal? (for/list ([k (in-dict-keys dd5)])
                k)
              (ddict-keys dd5))
(check-equal? (for/list ([k (in-dict-keys (mdd5))])
                k)
              (ddict-keys dd5))
(check-equal? (for/list ([k (in-ddict-keys (ddict-remove (ddict-remove dd5 0) 1))])
                k)
              (ddict-keys (ddict-remove (ddict-remove dd5 0) 1)))
(check-equal? (for/list ([k (in-ddict-keys (ddict-remove (ddict-remove dd5 4) 3))])
                k)
              (ddict-keys (ddict-remove (ddict-remove dd5 4) 3)))
(check-equal? (for/list ([k ((λ () (in-ddict-keys dd5)))])
                k)
              (ddict-keys dd5))
(check-exn #rx"in-ddict-keys: contract violation"
           (λ () (check-equal? (for/list ([k (in-ddict-keys '())])
                                 k)
                               (ddict->list dd5))))
(check-exn #rx"in-ddict-keys: contract violation"
           (λ () (check-equal? (for/list ([k ((λ () (in-ddict-keys '())))])
                                 k)
                               (ddict->list dd5))))


;in-ddict-values // in-dict-values
(check-equal? (for/list ([v (in-ddict-values dd5)])
                v)
              (ddict-values dd5))
(check-equal? (for/list ([v (in-ddict-values (mdd5))])
                v)
              (ddict-values dd5))
(check-equal? (for/list ([v (in-dict-values dd5)])
                v)
              (ddict-values dd5))
(check-equal? (for/list ([v (in-dict-values (mdd5))])
                v)
              (ddict-values dd5))
(check-equal? (for/list ([k (in-ddict-values (ddict-remove (ddict-remove dd5 4) 3))])
                k)
              (ddict-values (ddict-remove (ddict-remove dd5 4) 3)))
(check-equal? (for/list ([k (in-ddict-values (ddict-remove (ddict-remove dd5 0) 1))])
                k)
              (ddict-values (ddict-remove (ddict-remove dd5 0) 1)))
(check-equal? (for/list ([v ((λ () (in-ddict-values dd5)))])
                v)
              (ddict-values dd5))
(check-exn #rx"in-ddict-values: contract violation"
           (λ () (check-equal? (for/list ([v (in-ddict-values '())])
                                 v)
                               (ddict->list dd5))))
(check-exn #rx"in-ddict-values: contract violation"
           (λ () (check-equal? (for/list ([v ((λ () (in-ddict-values '())))])
                                 v)
                               (ddict->list dd5))))



(define-syntax-rule (for-tests for expected)
  (begin (check-equal? (for ([n (in-range 5)])
                         (values n (number->string n)))
                       expected)
         (check-equal? (for ([n (in-range 5)])
                         #:break (> n 4)
                         (values n (number->string n)))
                       expected)
         (check-equal? (for ([n (in-range 10)])
                         (values (modulo n 5)
                                 (number->string (modulo n 5))))
                       expected)))

(for-tests for/ddict dd5)
(for-tests for/ddicteqv dd5eqv)
(for-tests for/ddicteq dd5eq)
(for-tests for/mutable-ddict (mdd5))
(for-tests for/mutable-ddicteqv (mdd5eqv))
(for-tests for/mutable-ddicteq (mdd5eq))

(define-syntax-rule (for*-tests for* expected)
  (begin (check-equal? (for* ([n (in-range 5)]
                              [n (in-value n)])
                         (values n (number->string n)))
                       expected)
         (check-equal? (for* ([n (in-range 5)]
                              [n (in-value n)])
                         #:break (> n 4)
                         (values n (number->string n)))
                       expected)
         (check-equal? (for* ([n (in-range 10)]
                              [n (in-value n)])
                         (values (modulo n 5)
                                 (number->string (modulo n 5))))
                       expected)))

(for*-tests for*/ddict dd5)
(for*-tests for*/ddicteqv dd5eqv)
(for*-tests for*/ddicteq dd5eq)
(for*-tests for*/mutable-ddict (mdd5))
(for*-tests for*/mutable-ddicteqv (mdd5eqv))
(for*-tests for*/mutable-ddicteq (mdd5eq))



;; printing
(check-equal? (format "~a" dd5)
                       "(ddict ((4 . 4) (3 . 3) (2 . 2) (1 . 1) (0 . 0)))")
(check-equal? (format "~a" (mdd5))
                       "(mutable-ddict ((4 . 4) (3 . 3) (2 . 2) (1 . 1) (0 . 0)))")

;; check hash code
(check-equal? (equal-hash-code (ddict 1 1 2 2 3 3))
              (equal-hash-code (ddict 3 3 1 1 2 2)))
(check-equal? (equal-hash-code (mutable-ddict 1 1 2 2 3 3))
              (equal-hash-code (mutable-ddict 3 3 1 1 2 2)))




;; tests w/ concurrent threads operating on the
;; same mutable-ddict
(let*-values
    ([(N) 1000000]
     [(mdd) (mutable-ddict)]
     [(l1 l2 l3 l4)
      (for/lists (l1 l2 l3 l4)
        ([n (in-range 0 N 4)])
        (values n (+ 1 n) (+ 2 n) (+ 3 n)))])
  ;; concurrent ddict-set!, ddict-ref!,
  ;;  ddict-update!, and ddict-remove!
  (define t1 (thread (λ () (for ([n (in-list l1)])
                             (ddict-set! mdd n n)))))
  (define t2 (thread (λ () (for ([n (in-list l2)])
                             (ddict-ref! mdd n n)))))
  (define t3 (thread (λ () (for ([n (in-list l3)])
                             (ddict-update! mdd n add1 (sub1 n))))))
  (define t4 (thread (λ () (for ([n (in-list l4)])
                             (ddict-set*! mdd n n)))))
  (thread-wait t1)
  (thread-wait t2)
  (thread-wait t3)
  (thread-wait t4)
  (check-equal? (ddict-count mdd) N)
  (check-true (for/and ([n (in-range N)])
                (equal? (ddict-ref mdd n #f)  n)))
  ;; concurrent ddict-remove!
  (set! t1 (thread (λ () (for ([n (in-list l1)])
                           (ddict-remove! mdd n)))))
  (set! t2 (thread (λ () (for ([n (in-list l2)])
                           (ddict-remove! mdd n)))))
  (set! t3 (thread (λ () (for ([n (in-list l3)])
                           (ddict-remove! mdd n)))))
  (set! t4 (thread (λ () (for ([n (in-list l4)])
                           (ddict-remove! mdd n)))))
  (thread-wait t1)
  (thread-wait t2)
  (thread-wait t3)
  (thread-wait t4)
  (check-equal? (ddict-count mdd) 0)
  (check-true (ddict-empty? mdd))
  (check-equal? (ddict-keys mdd) '())
  (check-equal? (ddict-values mdd) '()))

