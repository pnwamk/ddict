#lang racket/base

(require "../../data/ddict.rkt"
         racket/syntax
         (for-syntax racket/base))

(define (random-key)
  (list->string
   (map integer->char
        (for/list ([i (in-range 1 (add1 (random 20)))])
          (random 256)))))


(define N 500000)


(define (gc)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage))

(define-syntax-rule (run/immutable num-keys dict set ref remove)
  (let ([keys (for/list ([i (in-range num-keys)]) (random-key))]
        [iterations (quotient N num-keys)])
    (printf "\n")
    (printf " - ~a (~a keys, ~a iterations)\n" 'dict num-keys iterations)
    (printf " -- insertion\n") (gc)  
    (let ([h (time (let () (for ([_ (in-range (sub1 iterations))])
                             (for/fold ([h (dict)])
                                       ([k (in-list keys)])
                               (set h k #t)))
                     (for/fold ([h (dict)])
                               ([k (in-list keys)])
                       (set h k #t))))])
      (printf " -- lookup\n") (gc)
      (time (for* ([_ (in-range iterations)]
                   [k (in-list keys)])
              (ref h k)))
      (printf " -- removal\n") (gc)
      (void (time (for ([_ (in-range iterations)])
                    (for/fold ([h h]) ([k (in-list keys)])
                      (remove h k)))))
      (printf " -- thrash (add,remove,add,remove,...)\n") (gc)
      (void (time (for ([_ (in-range iterations)])
                    (for/fold ([h h]) ([k (in-list keys)])
                      (remove (set h k #t) k))))))))

(printf "1. random string keys [immutable] [equal?]\n")
(let ([keys (for/list ([i (in-range N)]) (random-key))])
  (run/immutable 10 hash hash-set hash-ref hash-remove)
  (run/immutable 10 ddict ddict-set ddict-ref ddict-remove)
  (run/immutable 100 hash hash-set hash-ref hash-remove)
  (run/immutable 100 ddict ddict-set ddict-ref ddict-remove)
  (run/immutable 1000 hash hash-set hash-ref hash-remove)
  (run/immutable 1000 ddict ddict-set ddict-ref ddict-remove)
  (run/immutable 10000 hash hash-set hash-ref hash-remove)
  (run/immutable 10000 ddict ddict-set ddict-ref ddict-remove))


(define-syntax-rule (run/mutable name num-keys make-dict set! ref remove!)
  (let ([keys (for/list ([i (in-range num-keys)]) (random-key))]
        [iterations (quotient N num-keys)])
    (printf "\n")
    (printf " - ~a (~a keys, ~a iterations)\n" 'name num-keys iterations)
    (printf " -- insertion!\n") (gc)
    (time (for ([_ (in-range iterations)])
            (let ([h (make-dict)])
              (for ([k (in-list keys)])
                (set! h k #t)))))
    (printf " -- lookup\n") (gc)
    (let ([h (make-dict)])
      (for ([k (in-list keys)])
        (set! h k #t))
      (time (for* ([_ (in-range iterations)]
                   [k (in-list keys)])
              (ref h k))))
    (printf " -- insert!, insert!, ..., remove!, remove!, ....\n") (gc)
    (void (time (for ([_ (in-range iterations)])
                  (define h (make-dict))
                  (for ([k (in-list keys)])
                    (set! h k #t))
                  (for ([k (in-list keys)])
                    (remove! h k)))))))

(printf "2. random string keys [mutable] [equal?]\n")
(let ([keys (for/list ([i (in-range N)]) (random-key))])
  (run/mutable mutable-hash 10 make-hash hash-set! hash-ref hash-remove!)
  (run/mutable mutable-ddict 10 mutable-ddict ddict-set! ddict-ref ddict-remove!)
  (run/mutable mutable-hash 100 make-hash hash-set! hash-ref hash-remove!)
  (run/mutable mutable-ddict 100 mutable-ddict ddict-set! ddict-ref ddict-remove!)
  (run/mutable mutable-hash 1000 make-hash hash-set! hash-ref hash-remove!)
  (run/mutable mutable-ddict 1000 mutable-ddict ddict-set! ddict-ref ddict-remove!)
  (run/mutable mutable-hash 10000 make-hash hash-set! hash-ref hash-remove!)
  (run/mutable mutable-ddict 10000 mutable-ddict ddict-set! ddict-ref ddict-remove!))
