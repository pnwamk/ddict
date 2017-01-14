#lang racket/base

(require (rename-in racket/unsafe/ops
                    [unsafe-struct*-ref struct-ref]
                    [unsafe-struct*-set! struct-set!])
         (for-syntax racket/base))




(provide ddict?
         immutable-ddict?
         mutable-ddict?
         ddict-equal?
         ddict-eqv?
         ddict-eq?
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
         ddict-clear!
         ddict-clear
         ddict-copy
         ddict-copy-clear
         ddict-has-key?
         ddict-empty?
         ddict-compact?
         ddict-compact!
         ddict-count
         ddict-keys
         ddict-values
         ddict->list
         in-ddict
         in-ddict-keys
         in-ddict-values
         ddict-map
         ddict-for-each
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
         for*/mutable-ddicteq)

(define-syntax-rule (in? elems)
  (λ (x) (hash-has-key? elems x)))

(define-syntax-rule (too-fragmented? elems del)
  (> del (hash-count elems)))

(define-syntax-rule (no-key-err-thunk fun-name key)
  (λ () (raise (make-exn:fail:contract
                (format "~a: no value found for key\n key: ~a" (quote fun-name) key)
                (current-continuation-marks)))))


(define *missing* (let () (struct missing ()) (missing)))

;; elems - hash? - the actual dictionary data structure -- used for
;; both mappings and equality checks
;; del - exact-nonnegative-integer? - how many deleted items are stored in 'seq'
;; seq - list? - the sequence of keys in LIFO order
;; NOTE - fields are mutable, but this is (theoretically)
;; unobservable to users, since it only deals w/ how we're
;; organizing our internal state. The immutable versions
;; will still have persistent observable behavior for users.
(struct ddict (elems [del #:mutable] [seq #:mutable])
  #:constructor-name do-not-use-me-ever)

;; NOTE: keep these in sync w/ above def!!!!!!
(define-syntax-rule (unsafe-ddict-elems dd) (struct-ref dd 0))
;(define-syntax-rule (unsafe-ddict-elems dd) (ddict-elems dd))

(define-syntax-rule (unsafe-ddict-del dd)          (unsafe-struct-ref  dd 1))
;(define-syntax-rule (unsafe-ddict-del dd)          (ddict-del  dd))

(define-syntax-rule (unsafe-set-ddict-del! dd val) (unsafe-struct-set! dd 1 val))
;(define-syntax-rule (unsafe-set-ddict-del! dd val) (set-ddict-del! dd val))

(define-syntax-rule (unsafe-ddict-seq dd)          (unsafe-struct-ref  dd 2))
;(define-syntax-rule (unsafe-ddict-seq dd)          (ddict-seq  dd))

(define-syntax-rule (unsafe-set-ddict-seq! dd val) (unsafe-struct-set! dd 2 val))
;(define-syntax-rule (unsafe-set-ddict-seq! dd val) (set-ddict-seq! dd val))

;; 
;; ddict-print
;; 
(define (ddict-print dd port mode)
  (define elems (ddict-elems dd))
  (if mode
      (if (immutable-ddict? dd)
          (write-string "#<ddict:" port)
          (write-string "#<mutable-ddict:" port))
      (if (immutable-ddict? dd)
          (write-string "(ddict" port)
          (write-string "(mutable-ddict" port)))
  (let ([recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (λ (p port) (print p port mode))])])
    (recur (ddict->list dd) port))
  (if mode
      (write-string ">" port)
      (write-string ")" port)))

;; 
;; ddict=?
;; 
(define (ddict=? dd1 dd2 rec-equal?)
  (rec-equal? (unsafe-ddict-elems dd1)
              (unsafe-ddict-elems dd2)))

;; 
;; ddict-hash-code
;; 
(define (ddict-hash-code dd rec-hc)
  (rec-hc (unsafe-ddict-elems dd)))

(struct immutable-ddict ddict ()
  #:methods gen:equal+hash
  [(define equal-proc ddict=?)
   (define hash-proc ddict-hash-code)
   (define hash2-proc ddict-hash-code)]
  #:methods gen:custom-write
  [(define write-proc ddict-print)])

(struct mutable-ddict ddict ()
  #:methods gen:equal+hash
  [(define equal-proc ddict=?)
   (define hash-proc ddict-hash-code)
   (define hash2-proc ddict-hash-code)]
  #:methods gen:custom-write
  [(define write-proc ddict-print)])

(define empty-ddict (immutable-ddict #hash() 0 '()))
(define empty-ddicteqv (immutable-ddict #hasheqv() 0 '()))
(define empty-ddicteq (immutable-ddict #hasheq() 0 '()))

;; constructor template for immutable ddicts
(define-syntax-rule (immutable-ddict-constructor name empty init-hash)
  (case-lambda
    [() empty]
    [args (build-immutable-ddict (quote name) init-hash 0 '() args)]))

(define (build-immutable-ddict name init-hash del init-seq initial-args)
  (let loop ([args initial-args]
             [elems init-hash]
             [count (hash-count init-hash)]
             [seq init-seq])
    (cond
      [(pair? args)
       (cond
         [(pair? (cdr args))
          (let* ([key (car args)]
                 [elems (hash-set elems key (cadr args))])
            (cond
              [(eqv? (hash-count elems) count)
               (loop (cddr args) elems count seq)]
              [else (loop (cddr args) elems (add1 count) (cons key seq))]))]
         [else
          (raise-argument-error
           name
           "an even number of arguments"
           initial-args)])]
      [(null? args) (immutable-ddict elems del seq)]
      [else
       (raise-argument-error
        name
        "a list of keys and values"
        initial-args)])))

(define ddict*    (immutable-ddict-constructor ddict empty-ddict #hash()))
(define ddicteqv* (immutable-ddict-constructor ddicteqv empty-ddicteqv #hasheqv()))
(define ddicteq*   (immutable-ddict-constructor ddicteq empty-ddicteq #hasheq()))

;; constructor template for mutable ddicts
(define-syntax-rule (mutable-ddict-constructor name make-init-hash)
  (case-lambda
    [() (mutable-ddict (make-init-hash) 0 '())]
    [args (define elems (make-init-hash))
          (define dd (mutable-ddict elems 0 '()))
          (add-to-mutable-dict! (quote name) dd elems 0 '() args)
          dd]))

(define (add-to-mutable-dict! name dd elems init-count init-seq initial-args)
  (let loop ([args initial-args]
             [count init-count]
             [seq init-seq])
    (cond
      [(pair? args)
       (cond
         [(pair? (cdr args))
          (define key (car args))
          (hash-set! elems key (cadr args))
          (cond
            [(eqv? (hash-count elems) count)
             (loop (cddr args) count seq)]
            [else (loop (cddr args) (add1 count) (cons key seq))])]
         [else
          (raise-argument-error
           name
           "an even number of arguments"
           initial-args)])]
      [(null? args) (unsafe-set-ddict-seq! dd seq)]
      [else
       (raise-argument-error
        name
        "a list of keys and values"
        initial-args)])))

(define mutable-ddict* (mutable-ddict-constructor mutable-ddict make-hash))
(define mutable-ddicteqv* (mutable-ddict-constructor mutable-ddicteqv make-hasheqv))
(define mutable-ddicteq* (mutable-ddict-constructor mutable-ddicteq make-hasheq))


;; "make-" constructor template for immutable ddicts
(define-syntax-rule (make-ddict/template name init-hash)
  (λ (initial-alist)
    (let loop ([alist initial-alist]
               [elems init-hash]
               [seq '()])
      (cond
        [(pair? alist)
         (define p (car alist))
         (define rst (cdr alist))
         (cond
           [(pair? p)
            (define key (car p))
            (define count (hash-count elems))
            (let ([elems (hash-set elems key (cdr p))])
              (if (eqv? (hash-count elems) count)
                  (loop rst elems seq)
                  (loop rst elems (cons key seq))))]
           [else (raise-argument-error (quote name)
                                       "(listof pair?)"
                                       initial-alist)])]
        [(null? alist) (immutable-ddict elems 0 seq)]
        [else (raise-argument-error (quote name)
                                    "(listof pair?)"
                                    initial-alist)]))))

(define make-ddict (make-ddict/template make-ddict #hash()))
(define make-ddicteqv (make-ddict/template make-ddicteqv #hasheqv()))
(define make-ddicteq (make-ddict/template make-ddicteq #hasheq()))

;; "make-" constructor template for mutable ddicts
(define-syntax-rule (make-mutable-ddict/template name make-init-hash)
  (λ (initial-alist)
    (define elems (make-init-hash))
    (let loop ([alist initial-alist]
               [seq '()])
      (cond
        [(pair? alist)
         (define p (car alist))
         (define rst (cdr alist))
         (cond
           [(pair? p)
            (define key (car p))
            (define count (hash-count elems))
            (hash-set! elems key (cdr p))
            (if (eqv? (hash-count elems) count)
                (loop rst seq)
                (loop rst (cons key seq)))]
           [else (raise-argument-error (quote name)
                                       "(listof pair?)"
                                       initial-alist)])]
        [(null? alist) (mutable-ddict elems 0 seq)]
        [else (raise-argument-error (quote name)
                                    "(listof pair?)"
                                    initial-alist)]))))

(define make-mutable-ddict (make-mutable-ddict/template make-ddict make-hash))
(define make-mutable-ddicteqv (make-mutable-ddict/template make-ddicteqv make-hasheqv))
(define make-mutable-ddicteq (make-mutable-ddict/template make-ddicteq make-hasheq))

;; macro for defining functions whose first argument is a ddict
;;
;; This automatically inserts a ddict? check (or immutable/mutable-ddict?)
;; and raise-argument-error for failure, as well as pattern matching
;; out the ddict's fields quickly after the check is complete
(define-syntax (define/dd stx)
  (syntax-case stx ()
    [(_ (name [(ddict-spec elems del seq) dd] . other-args) . body)
     (memq (syntax->datum #'ddict-spec) '(ddict iddict mddict))
     (with-syntax
         ;; bind all non-wildcard fields
         ([ddict-pred
           (case (syntax->datum #'ddict-spec)
             [(ddict) #'ddict?]
             [(iddict) #'immutable-ddict?]
             [(mddict) #'mutable-ddict?])]
          [ddict-pred-str
           (case (syntax->datum #'ddict-spec)
             [(ddict) #'"ddict?"]
             [(iddict) #'"immutable-ddict?"]
             [(mddict) #'"mutable-ddict?"])])
       (with-syntax
           ([bindings (append
                       (if (eq? '_ (syntax->datum #'elems))
                           (list)
                           (list #'[elems (unsafe-ddict-elems dd)]))
                       (if (eq? '_ (syntax->datum #'del))
                           (list)
                           (list #'[del (unsafe-ddict-del dd)]))
                       (if (eq? '_ (syntax->datum #'seq))
                           (list)
                           (list #'[seq (unsafe-ddict-seq dd)])))]
            ;; build a reasonable error message if not given a ddict
            ;; as the 1st argument
            [error-expr
             (if (identifier? #'other-args)
                 ;; rest args
                 (syntax/loc stx
                   (raise-argument-error
                    (quote name) ddict-pred-str 0 dd other-args))
                 ;; no rest args
                 (quasisyntax/loc stx
                   (raise-argument-error
                    (quote name) ddict-pred-str 0 dd
                    ;; grab argument ids to report as other args
                    . #,(for/fold ([others #'()])
                                  ([arg (in-list (reverse (syntax->list #'other-args)))])
                          (syntax-case arg ()
                            [[id def-val] (identifier? #'id) #`(id . #,others)]
                            [id (identifier? #'id) #`(id . #,others)])))))])
         (syntax/loc stx
           (define (name dd . other-args)
             (cond
               [(ddict-pred dd)
                (let bindings . body)]
               [else error-expr])))))]))


(define/dd (ddict-equal? [(ddict elems _ _) dd])
  (hash-equal? elems))
(define/dd (ddict-eqv? [(ddict elems _ _) dd])
  (hash-eqv? elems))
(define/dd (ddict-eq? [(ddict elems _ _) dd])
  (hash-eq? elems))

;;
;; ddict-set
;;
(define/dd (ddict-set [(iddict elems del seq) dd] key val)
  (define prev-count (hash-count elems))
  (let ([elems (hash-set elems key val)])
    (immutable-ddict elems del (if (eqv? prev-count (hash-count elems))
                                   seq
                                   (cons key seq)))))

;;
;; ddict-set!
;;
(define/dd (ddict-set! [(mddict elems del seq) dd] key val)
  (define prev-count (hash-count elems))
  (hash-set! elems key val)
  (unless (eqv? prev-count (hash-count elems))
    (unsafe-set-ddict-seq! dd (cons key seq))))

;;
;; ddict-update
;;
(define/dd (ddict-update [(iddict elems del seq) dd]
                         key
                         updater
                         [failure (no-key-err-thunk ddict-update key)])
  (unless (and (procedure? updater)
               (procedure-arity-includes? updater 1))
    (raise-argument-error 'ddict-update "(any/c . -> . any/c)" updater))
  (define prev-count (hash-count elems))
  (let ([elems (hash-update elems key updater failure)])
    (immutable-ddict elems del (if (eqv? (hash-count elems) prev-count)
                                   seq
                                   (cons key seq)))))


;;
;; ddict-update!
;;
(define/dd (ddict-update! [(mddict elems del seq) dd]
                          key
                          updater
                          [failure (no-key-err-thunk ddict-update! key)])
  (unless (and (procedure? updater)
               (procedure-arity-includes? updater 1))
    (raise-argument-error 'ddict-update! "(any/c . -> . any/c)" updater))
  (define prev-count (hash-count elems))
  (hash-update! elems key updater failure)
  (unless (eqv? (hash-count elems) prev-count)
    (unsafe-set-ddict-seq! dd (cons key seq))))

;;
;; ddict-set*
;;
(define/dd (ddict-set* [(iddict elems del seq) dd] . args)
  (build-immutable-ddict 'ddict-set* elems (hash-count elems) seq args))

;;
;; ddict-set*!
;;
(define/dd (ddict-set*! [(mddict elems del seq) dd] . args)
  (add-to-mutable-dict! 'ddict-set*! dd elems (hash-count elems) seq args))

;;
;; ddict-remove
;;
(define/dd (ddict-remove [(iddict elems del seq) dd] key)
  (let* ([elems (hash-remove elems key)]
         [del (add1 del)])
    (if (too-fragmented? elems del)
        (immutable-ddict elems 0 (filter (in? elems) seq))
        (immutable-ddict elems del seq))))
;;
;; ddict-remove!
;;
(define/dd (ddict-remove! [(mddict elems del seq) dd] key)
  (hash-remove! elems key)
  (let ([del (add1 del)])
    (cond
      [(too-fragmented? elems del)
       (unsafe-set-ddict-seq! dd (filter (in? elems) seq))
       (unsafe-set-ddict-del! dd 0)]
      [else
       (unsafe-set-ddict-del! dd del)])))

;;
;; ddict-ref
;;
(define/dd (ddict-ref [(ddict elems _ _) dd]
                      key
                      [failure (no-key-err-thunk ddict-ref key)])
  (hash-ref elems key failure))

;;
;; ddict-ref!
;;
(define/dd (ddict-ref! [(mddict elems _ seq) dd]
                       key
                       to-set)
  (define initial-count (hash-count elems))
  (define val (hash-ref! elems key to-set))
  (unless (eqv? initial-count (hash-count elems))
    (unsafe-set-ddict-seq! dd (cons key seq)))
  val)

;;
;; ddict-clear!
;;
(define/dd (ddict-clear! [(mddict elems _ _) dd])
  (hash-clear! elems)
  (unsafe-set-ddict-del! dd 0)
  (unsafe-set-ddict-seq! dd '()))

;;
;; ddict-clear
;;
(define/dd (ddict-clear [(iddict elems _ _) dd])
  (immutable-ddict (hash-clear elems) 0 '()))

;;
;; ddict-copy-clear
;;
(define/dd (ddict-copy-clear [(ddict elems _ _) dd])
  (cond
    [(immutable-ddict? dd)
     (immutable-ddict (hash-copy-clear elems) 0 '())]
    [(mutable-ddict? dd)
     (mutable-ddict (hash-copy-clear elems) 0 '())]
    [else (error 'ddict-copy-clear "internal bug! impossible ddict: ~a" dd)]))

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
;; ddict-compact!
;;
(define/dd (ddict-compact! [(ddict elems del seq) dd])
  (unless (zero? del)
    (define seq* (filter (in? elems) seq))
    (unsafe-set-ddict-seq! dd seq*)
    (unsafe-set-ddict-del! dd 0)))

;;
;; ddict-compact?
;;
(define/dd (ddict-compact? [(ddict _ del _) dd])
  (zero? del))

;;
;; ddict-keys
;;
(define/dd (ddict-keys [(ddict elems del seq) dd])
  (cond
    [(and (zero? del) (immutable? elems)) seq]
    [else (for*/list ([key (in-list seq)]
                      #:when (hash-has-key? elems key))
            key)]))

;; 
;; ddict-values
;; 
(define/dd (ddict-values [(ddict elems del seq) dd])
  (cond
    [(and (zero? del) (immutable? elems))
     (map (λ (key) (hash-ref elems key)) seq)]
    [else (for*/list ([key (in-list seq)]
                      [val (in-value (hash-ref elems key *missing*))]
                      #:unless (eq? val *missing*))
            val)]))

;;
;; ddict->list
;;
(define/dd (ddict->list [(ddict elems del seq) dd])
  (cond
    [(and (zero? del) (immutable? elems))
     (map (λ (key) (cons key (hash-ref elems key))) seq)]
    [else (for*/list ([key (in-list seq)]
                      [val (in-value (hash-ref elems key *missing*))]
                      #:unless (eq? val *missing*))
            (cons key val))]))

;;
;; ddict-map
;;
(define/dd (ddict-map [(ddict elems del seq) dd] f)
  (unless (and (procedure? f)
               (procedure-arity-includes? f 2))
    (raise-argument-error 'ddict-map "(any/c any/c . -> . any/c)" f))
  (cond
    [(and (zero? del) (immutable? elems))
     (map (λ (key) (f key (hash-ref elems key))) seq)]
    [else (for*/list ([key (in-list seq)]
                      [val (in-value (hash-ref elems key *missing*))]
                      #:unless (eq? val *missing*))
            (f key val))]))

;;
;; ddict-for-each
;;
(define/dd (ddict-for-each [(ddict elems del seq) dd] f)
  (unless (and (procedure? f)
               (procedure-arity-includes? f 2))
    (raise-argument-error 'ddict-for-each "(any/c any/c . -> . any/c)" f))
  (cond
    [(and (zero? del) (immutable? elems))
     (for-each (λ (key) (f key (hash-ref elems key))) seq)]
    [else (for* ([key (in-list seq)]
                 [val (in-value (hash-ref elems key *missing*))]
                 #:unless (eq? val *missing*))
            (f key val))]))


;;
;; next-key/val
;;
(define-syntax-rule (next-key/val elems seq)
  (cond
    [(pair? seq)
     (define key (car seq))
     (define val (hash-ref elems key *missing*))
     (if (eq? val *missing*)
         (next-key/val-proc elems (cdr seq))
         (values key val (cdr seq)))]
    [else (values #f #f #f)]))

(define (next-key/val-proc elems seq)
  (cond
    [(pair? seq)
     (define key (car seq))
     (define val (hash-ref elems key *missing*))
     (if (eq? val *missing*)
         (next-key/val-proc elems (cdr seq))
         (values key val (cdr seq)))]
    [else (values #f #f #f)]))

;;
;; in-ddict-proc
;;
(define (in-ddict-proc dd)
  (cond
    [(ddict? dd)
     (define alist (ddict->list dd))
     (define-values (keys vals)
       (for/lists (ks vs)
         ([p (in-list alist)])
         (values (car p) (cdr p))))
     (in-parallel keys vals)]
    [else
     (raise-argument-error 'in-ddict "ddict?" dd)]))

;;
;; in-ddict
;;
(define-sequence-syntax in-ddict
  (λ () #'in-ddict-proc)
  (λ (stx)
    (syntax-case stx ()
      [[(key val) (_ dd-exp)]
       #'[(key val)
          (:do-in
           ;; ([(outer-id ...) outer-expr] ...)
           ([(elems seq)
             (let ([dd dd-exp])
               (unless (ddict? dd)
                 (raise-argument-error 'in-ddict "ddict?" dd))
               (values (unsafe-ddict-elems dd)
                       (unsafe-ddict-seq dd)))])
           ;; outer-check
           #t
           ;; ([loop-id loop-expr] ...)
           ([pos seq])
           ;; pos-guard
           #t
           ;; ([(inner-id ...) inner-expr] ...)
           ([(key val rst) (next-key/val elems pos)])
           ;; pre-guard
           rst
           ;; post-guard
           #t
           ;; (loop-arg ...)
           (rst))]])))

;;
;; next-key
;;
(define-syntax-rule (next-key elems seq)
  (cond
    [(pair? seq)
     (define key (car seq))
     (if (eq? *missing* (hash-ref elems key *missing*))
         (next-keyproc elems (cdr seq))
         (values key (cdr seq)))]
    [else (values #f #f)]))

(define (next-keyproc elems seq)
  (cond
    [(pair? seq)
     (define key (car seq))
     (if (eq? *missing* (hash-ref elems key *missing*))
         (next-keyproc elems (cdr seq))
         (values key (cdr seq)))]
    [else (values #f #f)]))

(define (in-ddict-keys-proc dd)
  (cond
    [(ddict? dd) (ddict-keys dd)]
    [else
     (raise-argument-error 'in-ddict-keys "ddict?" dd)]))

;;
;; in-ddict-keys
;;
(define-sequence-syntax in-ddict-keys
  (λ () #'in-ddict-keys-proc)
  (λ (stx)
    (syntax-case stx ()
      [[(key) (_ dd-exp)]
       #'[(key)
          (:do-in
           ;; ([(outer-id ...) outer-expr] ...)
           ([(elems seq)
             (let ([dd dd-exp])
               (unless (ddict? dd)
                 (raise-argument-error 'in-ddict-keys "ddict?" dd))
               (values (unsafe-ddict-elems dd)
                       (unsafe-ddict-seq dd)))])
           ;; outer-check
           #t
           ;; ([loop-id loop-expr] ...)
           ([pos seq])
           ;; pos-guard
           #t
           ;; ([(inner-id ...) inner-expr] ...)
           ([(key rst) (next-key elems pos)])
           ;; pre-guard
           rst
           ;; post-guard
           #t
           ;; (loop-arg ...)
           (rst))]])))


;;
;; next-val
;;
(define-syntax-rule (next-val elems seq)
  (cond
    [(pair? seq)
     (define val (hash-ref elems (car seq) *missing*))
     (if (eq? val *missing*)
         (next-valproc elems (cdr seq))
         (values val (cdr seq)))]
    [else (values #f #f)]))

(define (next-valproc elems seq)
  (cond
    [(pair? seq)
     (define val (hash-ref elems (car seq) *missing*))
     (if (eq? val *missing*)
         (next-valproc elems (cdr seq))
         (values val (cdr seq)))]
    [else (values #f #f)]))

(define (in-ddict-values-proc dd)
  (cond
    [(ddict? dd) (ddict-values dd)]
    [else
     (raise-argument-error 'in-ddict-values "ddict?" dd)]))

;;
;; in-ddict-values
;;
(define-sequence-syntax in-ddict-values
  (λ () #'in-ddict-values-proc)
  (λ (stx)
    (syntax-case stx ()
      [[(val) (_ dd-exp)]
       #'[(val)
          (:do-in
           ;; ([(outer-id ...) outer-expr] ...)
           ([(elems seq)
             (let ([dd dd-exp])
               (unless (ddict? dd)
                 (raise-argument-error 'in-ddict-values "ddict?" dd))
               (values (unsafe-ddict-elems dd)
                       (unsafe-ddict-seq dd)))])
           ;; outer-check
           #t
           ;; ([loop-id loop-expr] ...)
           ([pos seq])
           ;; pos-guard
           #t
           ;; ([(inner-id ...) inner-expr] ...)
           ([(val rst) (next-val elems pos)])
           ;; pre-guard
           rst
           ;; post-guard
           #t
           ;; (loop-arg ...)
           (rst))]])))


(define-syntax-rule (define-for-immutable-ddict for-name for/derived empty-hash)
  (define-syntax (for-name stx)
    (syntax-case stx ()
      [(_ clauses . defs+exprs)
       (with-syntax ([original stx])
         (syntax/loc stx
           (let-values
               ([(elems seq _)
                 (for/derived original
                   ([elems empty-hash]
                    [seq '()]
                    [count 0])
                   clauses
                   (let*-values ([(key val) (let () . defs+exprs)]
                                 [(elems) (hash-set elems key val)])
                     (if (eqv? (hash-count elems) count)
                         (values elems seq count)
                         (values elems (cons key seq) (add1 count)))))])
             (immutable-ddict elems 0 seq))))])))



(define-for-immutable-ddict for/ddict for/fold/derived #hash())
(define-for-immutable-ddict for/ddicteqv for/fold/derived #hasheqv())
(define-for-immutable-ddict for/ddicteq for/fold/derived #hasheq())
(define-for-immutable-ddict for*/ddict for*/fold/derived #hash())
(define-for-immutable-ddict for*/ddicteqv for*/fold/derived #hasheqv())
(define-for-immutable-ddict for*/ddicteq for*/fold/derived #hasheq())


(define-syntax-rule (define-for-mutable-ddict for-name for/derived make-empty-hash)
  (define-syntax (for-name stx)
    (syntax-case stx ()
      [(_ clauses . defs+exprs)
       (with-syntax ([original stx])
         (syntax/loc stx
           (let*-values
               ([(elems) (make-empty-hash)]
                [(seq _)
                 (for/derived original
                   ([seq '()]
                    [count 0])
                   clauses
                   (let-values ([(key val) (let () . defs+exprs)])
                     (hash-set! elems key val)
                     (if (eqv? (hash-count elems) count)
                         (values seq count)
                         (values (cons key seq) (add1 count)))))])
             (mutable-ddict elems 0 seq))))])))


(define-for-mutable-ddict for/mutable-ddict for/fold/derived make-hash)
(define-for-mutable-ddict for/mutable-ddicteqv for/fold/derived make-hasheqv)
(define-for-mutable-ddict for/mutable-ddicteq for/fold/derived make-hasheq)
(define-for-mutable-ddict for*/mutable-ddict for*/fold/derived make-hash)
(define-for-mutable-ddict for*/mutable-ddicteqv for*/fold/derived make-hasheqv)
(define-for-mutable-ddict for*/mutable-ddicteq for*/fold/derived make-hasheq)



