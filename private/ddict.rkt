#lang racket/base

(require (only-in racket/unsafe/ops unsafe-struct-ref)
         (for-syntax racket/base))




(provide ddict?
         immutable-ddict?
         mutable-ddict?
         (rename-out [ddict* ddict]
                     [ddicteqv* ddicteqv]
                     [ddicteq* ddicteq]
                     [mutable-ddict* mutable-ddict]
                     [mutable-ddicteqv* mutable-ddicteqv]
                     [mutable-ddicteq* mutable-ddicteq])
         make-ddict
         make-ddicteqv
         make-ddicteq
         make-mutable-ddict
         make-mutable-ddicteqv
         make-mutable-ddicteq
         ddict-set
         ddict-set*
         ddict-set!
         ddict-set*!
         ddict-ref
         ddict-ref!
         ddict-update
         ddict-update!
         ddict-remove
         ddict-remove!
         ddict-copy
         ddict-has-key?
         ddict-empty?
         ddict-clean?
         ddict-clean
         ddict-clean!
         ddict-empty?
         ddict-count
         ddict-keys
         ddict-values
         ddict->list)



(define-syntax-rule (in? elems)
  (λ (x) (hash-has-key? elems x)))

(define-syntax-rule (need-to-flush? elems del)
  (> del (* .5 (hash-count elems))))

;; - - - - - -
;; ddict-print
;; - - - - - -
(define (ddict-print dd port mode)
  (define elems (ddict-elems dd))
  (if mode
      (if (immutable-ddict? dd)
          (write-string "#<ddict:" port)
          (write-string "#<mutable-ddict:" port))
      (if (immutable-ddict?)
          (write-string "(ddict" port)
          (write-string "(mutable-ddict" port)))
  (let ([l (filter (in? elems) (ddict-seq dd))]
        [recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (λ (p port) (print p port mode))])])
    (when (not (null? l))
      (for* ([key (in-list l)]
             [val (in-value (hash-ref elems key))])
        (write-string " " port)
        (recur (cons key val) port))))
  (if mode
      (write-string ">" port)
      (write-string ")" port)))



;; elems - hash? - the actual dictionary data structure -- used for
;; both mappings and equality checks
;; del - exact-nonnegative-integer? - how many deleted items are stored in 'seq'
;; seq - list? - the sequence of keys in LIFO order
;; NOTE - fields are mutable, but only the mutable-ddict
;; functions should mutate the fields, and only for
;; mutable-ddicts
(struct ddict (elems [del #:mutable] [seq #:mutable])
  #:constructor-name do-not-use-me-ever)

;; NOTE: keep these in sync w/ above def!!!!!!
(define-syntax-rule (unsafe-ddict-elems dd) (unsafe-struct-ref dd 0))
(define-syntax-rule (unsafe-ddict-del dd)   (unsafe-struct-ref dd 1))
(define-syntax-rule (unsafe-ddict-seq dd)   (unsafe-struct-ref dd 2))

(struct immutable-ddict ddict ()
  #:methods gen:equal+hash
  [(define (equal-proc dd1 dd2 rec-equal?)
     (rec-equal? (unsafe-ddict-elems dd1)
                 (unsafe-ddict-elems dd2)))
   (define (hash-proc dd rec-hc)
     (rec-hc (unsafe-ddict-elems dd)))
   (define (hash2-proc dd rec-hc)
     (rec-hc (unsafe-ddict-elems dd)))]
  #:methods gen:custom-write
  [(define write-proc ddict-print)])

(struct mutable-ddict ddict ()
  #:methods gen:equal+hash
  [(define (equal-proc dd1 dd2 rec-equal?)
     (rec-equal? (unsafe-ddict-elems dd1)
                 (unsafe-ddict-elems dd2)))
   (define (hash-proc dd rec-hc)
     (rec-hc (unsafe-ddict-elems dd)))
   (define (hash2-proc dd rec-hc)
     (rec-hc (unsafe-ddict-elems dd)))]
  #:methods gen:custom-write
  [(define write-proc ddict-print)])

(define empty-ddict (immutable-ddict #hash() 0 '()))
(define empty-ddicteqv (immutable-ddict #hasheqv() 0 '()))
(define empty-ddicteq (immutable-ddict #hasheq() 0 '()))

(define-syntax-rule (ddict/template name empty init-hash)
  (case-lambda
    [() empty]
    [initial-args
     (let loop ([args initial-args]
                [elems init-hash]
                [seq '()])
       (cond
         [(null? args) (immutable-ddict elems 0 seq)]
         [(null? (cdr args))
          (raise-argument-error
           (quote name)
           "an even number of arguments"
           initial-args)]
         [else
          (define key (car args))
          (let ([seq (if (hash-has-key? elems key)
                         seq
                         (cons key seq))])
            (loop (cddr args)
                  (hash-set elems key (cadr args))
                  seq))]))]))

(define ddict*    (ddict/template ddict empty-ddict #hash()))
(define ddicteqv* (ddict/template ddicteqv empty-ddicteqv #hasheqv()))
(define ddicteq*   (ddict/template ddicteq empty-ddicteq #hasheq()))

(define-syntax-rule (mutable-ddict/template name make-init-hash)
  (case-lambda
    [() (mutable-ddict (make-init-hash) 0 '())]
    [initial-args
     (define elems (make-init-hash))
     (let loop ([args initial-args]
                [seq '()])
       (cond
         [(null? args) (mutable-ddict elems 0 seq)]
         [(null? (cdr args))
          (raise-argument-error
           'name
           "an even number of arguments"
           initial-args)]
         [else
          (define key (car args))
          (let ([seq (if (hash-has-key? elems key)
                         seq
                         (cons key seq))])
            (loop (cddr args)
                  (hash-set! elems key (cadr args))
                  seq))]))]))

(define mutable-ddict* (mutable-ddict/template mutable-ddict
                                               make-hash))
(define mutable-ddicteqv* (mutable-ddict/template mutable-ddicteqv
                                                  make-hasheqv))
(define mutable-ddicteq* (mutable-ddict/template mutable-ddicteq
                                                 make-hasheq))



(define-syntax-rule (make-ddict/template name init-hash)
  (λ (alist)
    (unless (and (list? alist) (andmap pair? alist))
      (raise-argument-error (quote name)
                            "a list of pairs"
                            alist))
    (define-values (elems seq)
      (for*/fold ([elems init-hash]
                  [seq '()])
                 ([p (in-list alist)]
                  [key (in-value (car p))]
                  [val (in-value (cdr p))]
                  [seq (in-value (if (hash-has-key? elems key)
                                     seq
                                     (cons key seq)))])
        (values (hash-set elems key val)
                seq)))
    (immutable-ddict elems 0 seq)))

(define make-ddict (make-ddict/template make-ddict #hash()))
(define make-ddicteqv (make-ddict/template make-ddicteqv #hasheqv()))
(define make-ddicteq (make-ddict/template make-ddicteq #hasheq()))

(define-syntax-rule (make-mutable-ddict/template name make-init-hash)
  (λ (alist)
    (define elems (make-init-hash))
    (define seq
      (for*/fold ([seq '()])
                 ([p (in-list alist)]
                  [key (in-value (car p))]
                  [val (in-value (cdr p))]
                  [seq (in-value (if (hash-has-key? elems key)
                                     seq
                                     (cons key seq)))])
        (hash-set! elems key val)
        seq))
    (mutable-ddict elems 0 seq)))

(define make-mutable-ddict (make-mutable-ddict/template make-ddict make-hash))
(define make-mutable-ddicteqv (make-mutable-ddict/template make-ddicteqv make-hasheqv))
(define make-mutable-ddicteq (make-mutable-ddict/template make-ddicteq make-hasheq))

(define-syntax (define/dd stx)
  (syntax-case stx ()
    [(_ (name [(ddict-spec elems del seq) dd] . other-args) . body)
     (memq (syntax->datum #'ddict-spec) '(ddict iddict mddict))
     (with-syntax
         ;; bind all non-wildcard fields
         ([ddict-pred (case (syntax->datum #'ddict-spec)
                        [(ddict) #'ddict?]
                        [(iddict) #'immutable-ddict?]
                        [(mddict) #'mutable-ddict?])]
          [bindings (append
                     (if (eq? '_ (syntax->datum #'elems))
                         (list)
                         (list #'[elems (unsafe-ddict-elems dd)]))
                     (if (eq? '_ (syntax->datum #'del))
                         (list)
                         (list #'[del (unsafe-ddict-del dd)]))
                     (if (eq? '_ (syntax->datum #'elems))
                         (list)
                         (list #'[seq (unsafe-ddict-seq dd)])))]
          ;; build a reasonable error message if not given a ddict
          ;; as the 1st argument
          [error-expr
           (if (identifier? #'other-args)
               ;; rest args
               (syntax/loc stx
                 (raise-argument-error
                  (quote name) "ddict?" 0 dd other-args))
               ;; no rest args
               (quasisyntax/loc stx
                 (raise-argument-error
                  (quote name) "ddict?" 0 dd
                  ;; grab argument ids to report as other args
                  . #,(for/fold ([others #'()])
                                ([arg (in-list (reverse (syntax->list #'other-args)))])
                        (syntax-case arg ()
                          [[id def-val] (identifier? #'id) #`(id . #,others)]
                          [id (identifier? #'id) #`(id . #,others)])))))])
       #'(define (name dd . other-args)
           (cond
             [(ddict? dd)
              (let bindings . body)]
             [else error-expr])))]))


;;
;; ddict-set
;;
(define/dd (ddict-set [(iddict elems del seq) dd] key val)
  (immutable-ddict
   (hash-set elems key val)
   del
   (if (hash-has-key? elems key)
       seq
       (cons key seq))))

;;
;; ddict-set!
;;
(define/dd (ddict-set! [(mddict elems del seq) dd] key val)
  (unless (hash-has-key? elems key)
    (set-ddict-seq! dd (cons key seq)))
  (hash-set! elems key val))

;;
;; ddict-update
;;
(define/dd (ddict-update [(iddict elems del seq) dd]
                         key
                         updater
                         [failure (λ () (raise-argument-error
                                         'ddict-update
                                         "a key with an entry in the ddict"
                                         key))])
  (unless (and (procedure? updater)
               (procedure-arity-includes? updater 1))
    (raise-argument-error 'ddict-update "procedure with arity 1" updater))
  (immutable-ddict
   (hash-update elems key updater failure)
   del
   (if (hash-has-key? elems key)
       seq
       (cons key seq))))


;;
;; ddict-update!
;;
(define/dd (ddict-update! [(mddict elems del seq) dd]
                          key
                          updater
                          [failure (λ () (raise-argument-error
                                          'ddict-update
                                          "a key with an entry in the ddict"
                                          key))])
  (unless (and (procedure? updater)
               (procedure-arity-includes? updater 1))
    (raise-argument-error 'ddict-update! "procedure with arity 1" updater))
  (unless (hash-has-key? elems key)
    (set-ddict-seq! dd (cons key seq)))
  (hash-update! elems key updater failure))


;;
;; ddict-set*
;;
(define/dd (ddict-set* [(iddict elems del seq) dd] . initial-args)
  (let loop ([args initial-args]
             [elems elems]
             [seq seq])
    (cond
      [(null? args) (immutable-ddict elems del seq)]
      [(null? (cdr args))
       (raise-argument-error
        'ddict-set*
        "an even number of arguments"
        initial-args)]
      [else
       (define key (car args))
       (loop (cddr args)
             (hash-set elems key (cadr args))
             (if (hash-has-key? elems key)
                 seq
                 (cons key seq)))])))

;;
;; ddict-set*!
;;
(define/dd (ddict-set*! [(mddict elems del seq) dd] . initial-args)
  (let loop ([args initial-args]
             [seq seq])
    (cond
      [(null? args) (set-ddict-seq! dd seq)]
      [(null? (cdr args))
       (raise-argument-error
        'ddict-set*
        "an even number of arguments"
        initial-args)]
      [else
       (define key (car args))
       (define seq* (if (hash-has-key? elems key)
                        seq
                        (cons key seq)))
       (hash-set! elems key (cadr args))
       (loop (cddr args) seq*)])))

;;
;; ddict-remove
;;
(define/dd (ddict-remove [(iddict elems del seq) dd] key)
  (let* ([elems (hash-remove elems key)]
         [del (add1 del)])
    (if (need-to-flush? elems del)
        (immutable-ddict elems 0 (filter (in? elems) seq))
        (immutable-ddict elems del seq))))
;;
;; ddict-remove!
;;
(define/dd (ddict-remove! [(mddict elems del seq) dd] key)
  (hash-remove! elems key)
  (let ([del (add1 del)])
    (set-ddict-del! dd del)
    (when (need-to-flush? elems del)
      (set-ddict-seq! (filter (in? elems) seq)))))

;;
;; ddict-ref
;;
(define/dd (ddict-ref [(ddict elems _ _) dd]
                      key
                      [failure (λ () (raise-argument-error
                                      'ddict-ref
                                      "a key with an entry in the ddict"
                                      key))])
  (hash-ref elems key failure))

;;
;; ddict-ref!
;;
(define/dd (ddict-ref! [(mddict elems _ seq) dd]
                       key
                       to-set)
  (cond
    [(hash-has-key? elems key)
     (hash-ref elems key)]
    [else
     (set-ddict-seq! dd (cons key seq))
     (hash-set! elems key to-set)
     to-set]))

;;
;; ddict-copy
;;
(define/dd (ddict-copy [(mddict elems del seq) dd])
  (let ([elems (hash-copy elems)])
    (mutable-ddict elems
                   0
                   (if (zero? del)
                       seq
                       (filter (in? elems) seq)))))

;;
;; ddict-has-key?
;;
(define/dd (ddict-has-key? [(ddict elems _ _) dd] key)
  (hash-has-key? elems key))

;;
;; ddict-empty?
;;
(define/dd (ddict-empty? [(ddict elems _ _) dd])
  (hash-empty? elems))

;;
;; ddict-count
;;
(define/dd (ddict-count [(ddict elems _ _) dd])
  (hash-count elems))

;;
;; ddict-clean
;;
(define/dd (ddict-clean [(iddict elems del seq) dd])
  (if (zero? del)
      dd
      (immutable-ddict elems 0 (filter (in? elems) seq))))

;;
;; ddict-clean!
;;
(define/dd (ddict-clean! [(mddict elems del seq) dd])
  (unless (zero? del)
    (set-ddict-del! dd 0)
    (set-ddict-seq! dd (filter (in? elems) seq))))

;;
;; ddict-flushed?
;;
(define/dd (ddict-clean? [(ddict _ del _) dd])
  (zero? del))

;;
;; ddict-keys
;;
(define/dd (ddict-keys [(ddict elems del seq) dd])
  (cond
    [(zero? del) seq]
    [else
     (filter (in? elems) seq)]))

;; 
;; ddict-values
;; 
(define/dd (ddict-values [(ddict elems del seq) dd])
  (cond
    [(zero? del)
     (for/list ([key (in-list seq)])
       (hash-ref elems key))]
    [else
     (for/list ([key (in-list seq)]
                #:when (hash-has-key? elems key))
       (hash-ref elems key))]))

;;
;; ddict->list
;;
(define/dd (ddict->list [(ddict elems del seq) dd])
  (cond
    [(zero? del)
     (for/list ([key (in-list seq)])
       (cons key (hash-ref elems key)))]
    [else
     (for/list ([key (in-list seq)]
                #:when (hash-has-key? elems key))
       (cons key (hash-ref elems key)))]))
