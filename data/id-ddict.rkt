#lang racket/base

(require "private/id-ddict.rkt"
         racket/contract/base)

(provide
 in-free-id-ddict
 in-free-id-ddict-keys
 in-free-id-ddict-values
 for/free-id-ddict
 for*/free-id-ddict
 for/mutable-free-id-ddict
 for*/mutable-free-id-ddict
 free-id-ddict?
 immutable-free-id-ddict?
 mutable-free-id-ddict?
 free-id-ddict-position?
 (contract-out
  [free-id-ddict
   (->* ()
        #:rest
        (flat-rec-contract
         identifier-key-value-pairs
         (or/c null? (cons/c identifier? (cons/c any/c identifier-key-value-pairs))))
        immutable-free-id-ddict?)]
  [mutable-free-id-ddict
   (->* ()
        #:rest
        (flat-rec-contract
         identifier-key-value-pairs
         (or/c null? (cons/c identifier? (cons/c any/c identifier-key-value-pairs))))
        mutable-free-id-ddict?)]
  [make-free-id-ddict
   (->* () ((listof (cons/c identifier? any/c)))
        immutable-free-id-ddict?)]
  [make-mutable-free-id-ddict
   (->* () ((listof (cons/c identifier? any/c))) mutable-free-id-ddict?)]
  [free-id-ddict-set
   (-> immutable-free-id-ddict? identifier? any/c immutable-free-id-ddict?)]
  [free-id-ddict-set*
   (->* (immutable-free-id-ddict?)
        #:rest
        (flat-rec-contract
         identifier-key-value-pairs
         (or/c null?
               (cons/c identifier? (cons/c any/c identifier-key-value-pairs))))
        immutable-free-id-ddict?)]
  [free-id-ddict-set!
   (-> mutable-free-id-ddict? identifier? any/c void?)]
  [free-id-ddict-set*!
   (->* (mutable-free-id-ddict?)
        #:rest
        (flat-rec-contract
         identifier-key-value-pairs
         (or/c null?
               (cons/c identifier? (cons/c any/c identifier-key-value-pairs))))
        void?)]
  [free-id-ddict-ref
   (->* (free-id-ddict? identifier?) (any/c) any)]
  [free-id-ddict-ref!
   (-> mutable-free-id-ddict? identifier? any/c any)]
  [free-id-ddict-update
   (->* (immutable-free-id-ddict? identifier? (any/c . -> . any/c))
        (any/c)
        immutable-free-id-ddict?)]
  [free-id-ddict-update!
   (->* (mutable-free-id-ddict? identifier? (any/c . -> . any/c))
        (any/c)
        void?)]
  [free-id-ddict-remove
   (-> immutable-free-id-ddict? identifier? immutable-free-id-ddict?)]
  [free-id-ddict-remove!
   (-> mutable-free-id-ddict? identifier? void?)]
  [free-id-ddict-clear!
   (-> mutable-free-id-ddict? void?)]
  [free-id-ddict-clear
   (-> immutable-free-id-ddict? immutable-free-id-ddict?)]
  [free-id-ddict-copy
   (-> free-id-ddict? free-id-ddict?)]
  [free-id-ddict-copy-clear
   (-> free-id-ddict? free-id-ddict?)]
  [free-id-ddict-has-key?
   (-> free-id-ddict? identifier? boolean?)]
  [free-id-ddict-empty?
   (-> free-id-ddict? boolean?)]
  [free-id-ddict-count
   (-> free-id-ddict? exact-nonnegative-integer?)]
  [free-id-ddict-keys
   (-> free-id-ddict? (listof identifier?))]
  [free-id-ddict-values
   (-> free-id-ddict? list?)]
  [free-id-ddict->list
   (-> free-id-ddict? (listof (cons/c identifier? any/c)))]
  [free-id-ddict-map
   (-> free-id-ddict? (-> identifier? any/c any/c) list?)]
  [free-id-ddict-for-each
   (-> free-id-ddict? (-> identifier? any/c any) void?)]
  [free-id-ddict-keys-subset?
   (-> free-id-ddict? free-id-ddict? boolean?)]
  [free-id-ddict-iterate-first
   (-> free-id-ddict? free-id-ddict-position?)]
  [free-id-ddict-iterate-next
   (-> free-id-ddict? free-id-ddict-position? free-id-ddict-position?)]
  [free-id-ddict-iterate-key
   (-> free-id-ddict? free-id-ddict-position? identifier?)]
  [free-id-ddict-iterate-value
   (-> free-id-ddict? free-id-ddict-position? any/c)]
  [free-id-ddict-compact?
   (-> free-id-ddict? boolean?)]
  [free-id-ddict-compact
   (-> immutable-free-id-ddict? immutable-free-id-ddict?)]
  [free-id-ddict-compact!
   (-> mutable-free-id-ddict? void?)]))
