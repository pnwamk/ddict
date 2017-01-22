#lang racket/base

(require "private/ddict.rkt"
         racket/contract/base)

(provide
 in-ddict
 in-ddict-keys
 in-ddict-values
 for/ddict
 for/ddicteqv
 for/ddicteq
 for*/ddict
 for*/ddicteqv
 for*/ddicteq
 for/mutable-ddict
 for/mutable-ddicteqv
 for/mutable-ddicteq
 for*/mutable-ddict
 for*/mutable-ddicteqv
 for*/mutable-ddicteq
 ddict?
 immutable-ddict?
 mutable-ddict?
 ddict-position?

 (contract-out
  [ddict-equal? (-> ddict? boolean?)]
  [ddict-eqv? (-> ddict? boolean?)]
  [ddict-eq? (-> ddict? boolean?)]
  [ddict (->* () #:rest (and/c list? (λ (l) (even? (length l))))
              (and/c immutable-ddict? ddict-equal?))]
  [ddicteqv (->* () #:rest (and/c list? (λ (l) (even? (length l))))
                 (and/c immutable-ddict? ddict-eqv?))]
  [ddicteq (->* () #:rest (and/c list? (λ (l) (even? (length l))))
                (and/c immutable-ddict? ddict-eq?))]
  [mutable-ddict (->* () #:rest (and/c list? (λ (l) (even? (length l))))
                      (and/c mutable-ddict? ddict-equal?))]
  [mutable-ddicteqv (->* () #:rest (and/c list? (λ (l) (even? (length l))))
                         (and/c mutable-ddict? ddict-eqv?))]
  [mutable-ddicteq (->* () #:rest (and/c list? (λ (l) (even? (length l))))
                        (and/c mutable-ddict? ddict-eq?))]
  [make-ddict (-> (listof pair?) (and/c immutable-ddict? ddict-equal?))]
  [make-ddicteqv (-> (listof pair?) (and/c immutable-ddict? ddict-eqv?))]
  [make-ddicteq (-> (listof pair?) (and/c immutable-ddict? ddict-eq?))]
  [make-mutable-ddict (-> (listof pair?) (and/c mutable-ddict? ddict-equal?))]
  [make-mutable-ddicteqv (-> (listof pair?) (and/c mutable-ddict? ddict-eqv?))]
  [make-mutable-ddicteq (-> (listof pair?) (and/c mutable-ddict? ddict-eq?))]
  [ddict-set (-> immutable-ddict? any/c any/c immutable-ddict?)]
  [ddict-set* (->* (immutable-ddict?) #:rest (and/c list? (λ (l) (even? (length l))))
                   immutable-ddict?)]
  [ddict-set! (-> mutable-ddict? any/c any/c void?)]
  [ddict-set*! (->* (mutable-ddict?) #:rest (and/c list? (λ (l) (even? (length l))))
                    void?)]
  [ddict-ref (->* (ddict? any/c) (any/c) any)]
  [ddict-ref! (-> mutable-ddict? any/c any/c any)]
  [ddict-update (->* (immutable-ddict? any/c (any/c . -> . any/c))
                     (any/c)
                     immutable-ddict?)]
  [ddict-update! (->* (mutable-ddict? any/c (any/c . -> . any/c))
                      (any/c)
                      void?)]
  [ddict-remove (-> immutable-ddict? any/c immutable-ddict?)]
  [ddict-remove! (-> mutable-ddict? any/c void?)]
  [ddict-clear! (-> mutable-ddict? void?)]
  [ddict-clear (-> immutable-ddict? immutable-ddict?)]
  [ddict-copy (-> ddict? ddict?)]
  [ddict-copy-clear (-> ddict? ddict?)]
  [ddict-has-key? (-> ddict? any/c boolean?)]
  [ddict-empty? (-> ddict? boolean?)]
  [ddict-count (-> ddict? exact-nonnegative-integer?)]
  [ddict-keys (-> ddict? list?)]
  [ddict-values (-> ddict? list?)]
  [ddict->list (-> ddict? (listof pair?))]
  [ddict-map (-> ddict? (-> any/c any/c any/c) list?)]
  [ddict-for-each (-> ddict? (-> any/c any/c any) void?)]
  [ddict-keys-subset? (-> ddict? ddict? boolean?)]
  [ddict-iterate-first (-> ddict? ddict-position?)]
  [ddict-iterate-next (-> ddict? ddict-position? ddict-position?)]
  [ddict-iterate-key (-> ddict? ddict-position? any/c)]
  [ddict-iterate-value (-> ddict? ddict-position? any/c)]
  [ddict-compact? (-> ddict? boolean?)]
  [ddict-compact (-> immutable-ddict? immutable-ddict?)]
  [ddict-compact! (-> mutable-ddict? void?)]))
