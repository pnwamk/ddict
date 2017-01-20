#lang racket/base

(require (only-in racket/unsafe/ops
                  unsafe-struct*-ref
                  unsafe-struct*-set!
                  unsafe-unbox*
                  unsafe-box*-cas!
                  unsafe-set-box*!
                  unsafe-vector*-ref)
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
         ddict-count
         ddict-keys
         ddict-values
         ddict->list
         ddict-map
         ddict-for-each
         ddict-keys-subset?
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
         ddict-compact?
         ddict-compact
         ddict-compact!)


(define-syntax-rule (no-key-err-thunk fun-name key)
  (λ () (raise (make-exn:fail:contract
                (format "~a: no value found for key\n key: ~a" (quote fun-name) key)
                (current-continuation-marks)))))


(define *missing* (gensym 'missing))

;; standard "default value call if val is thunk else return val" behavior
(define-syntax-rule (default val)
  (let ([d val]) (if (procedure? d) (d) d)))

(define-syntax-rule (unbox-key keybox)
  (weak-box-value keybox *missing*))

;; updates an immutable hash table and, if it's a new key,
;; that key is added to seq
(define-syntax-rule (update-elems+seq elems seq key val)
  (let ([prev-count (hash-count elems)]
        [elems (hash-set elems key val)])
    (values elems (if (eqv? prev-count (hash-count elems))
                      seq
                      (cons (make-weak-box key) seq)))))

;; 
;; ddict-print
;; 
(define (iddict-print dd port mode)
  (if mode
      (write-string "#<ddict: " port)
      (write-string "(ddict " port))
  (let ([recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (λ (p port) (print p port mode))])])
    (recur (ddict->list dd) port))
  (if mode
      (write-string ">" port)
      (write-string ")" port)))

(define (mddict-print dd port mode)
  (if mode
      (write-string "#<mutable-ddict: " port)
      (write-string "(mutable-ddict " port))
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
(define (iddict=? dd1 dd2 rec-equal?)
  (rec-equal? (immutable-ddict-elems dd1)
              (immutable-ddict-elems dd2)))

(define (mddict=? dd1 dd2 rec-equal?)
  (rec-equal? (unsafe-vector*-ref (unsafe-unbox* (mutable-ddict-content-box dd1)) 0)
              (unsafe-vector*-ref (unsafe-unbox* (mutable-ddict-content-box dd2)) 0)))

;; 
;; ddict-hash-code
;; 
(define (iddict-hash-code dd rec-hc)
  (rec-hc (immutable-ddict-elems dd)))

(define (mddict-hash-code dd rec-hc)
  (rec-hc (content-elems (unbox (mutable-ddict-content-box dd)))))


(struct immutable-ddict (elems del seq)
  #:constructor-name iddict
  #:methods gen:equal+hash
  [(define equal-proc iddict=?)
   (define hash-proc iddict-hash-code)
   (define hash2-proc iddict-hash-code)]
  #:methods gen:custom-write
  [(define write-proc iddict-print)])

(struct mutable-ddict (content-box)
  #:constructor-name unsafe-mk-mddict
  #:methods gen:equal+hash
  [(define equal-proc mddict=?)
   (define hash-proc mddict-hash-code)
   (define hash2-proc mddict-hash-code)]
  #:methods gen:custom-write
  [(define write-proc mddict-print)])

;; NOTE: we assume this vector is of length 3 w/ unsafe ops,
;; change any/all unsafe-vector... operations if this is modified
(define-syntax-rule (content a b c) (vector-immutable a b c))

;; NOTE: we assume this structure for mddicts (i.e. that it contains
;; a box which contains a 'content-vector' -- if any of this is changed,
;; all unsafe ops must also be changed)
(define-syntax-rule (mddict elems del seq)
  (unsafe-mk-mddict (box (content elems del seq))))

(define (content-elems c) (vector-ref c 0))
(define (content-del c) (vector-ref c 1))
(define (content-seq c) (vector-ref c 2))

(define (ddict? x) (or (immutable-ddict? x) (mutable-ddict? x)))

(define-syntax-rule (try-update-mddict-content! mdd elems del seq)
  (let* ([content-box (mutable-ddict-content-box mdd)]
         [orig-content (unsafe-unbox* content-box)]
         [new-content (content elems del seq)])
    (unsafe-box*-cas! content-box orig-content new-content)))

(define (fore-update-mddict-content! mdd elems del seq)
  (define content-box (mutable-ddict-content-box mdd))
  (unsafe-set-box*! content-box (content elems del seq)))

;; NOTE: keep these in sync w/ above defs!!!!!!


(define empty-ddict (iddict #hash() 0 '()))
(define empty-ddicteqv (iddict #hasheqv() 0 '()))
(define empty-ddicteq (iddict #hasheq() 0 '()))

;; constructor template for immutable ddicts
(define-syntax-rule (immutable-ddict-constructor name empty init-hash)
  (case-lambda
    [() empty]
    [args (build-immutable-ddict (quote name) init-hash 0 '() args)]))

(define (build-immutable-ddict name init-hash del init-seq initial-args)
  (let loop ([args initial-args]
             [elems init-hash]
             [seq init-seq])
    (cond
      [(pair? args)
       (cond
         [(pair? (cdr args))
          (define key (car args))
          (define val (cadr args))
          (let-values ([(elems seq) (update-elems+seq elems seq key val)])
            (loop (cddr args) elems seq))]
         [else
          (raise-argument-error
           name
           "an even number of arguments"
           initial-args)])]
      [(null? args) (iddict elems del seq)]
      [else (error name "impossible! you found a bug!")])))

(define ddict*    (immutable-ddict-constructor ddict empty-ddict #hash()))
(define ddicteqv* (immutable-ddict-constructor ddicteqv empty-ddicteqv #hasheqv()))
(define ddicteq*   (immutable-ddict-constructor ddicteq empty-ddicteq #hasheq()))

;; constructor template for mutable ddicts
(define-syntax-rule (mutable-ddict-constructor name init-hash)
  (case-lambda
    [() (mddict init-hash 0 '())]
    [args (define mdd (mddict init-hash 0 '()))
          (add-to-mutable-dict! (quote name) mdd init-hash 0 '() args)
          mdd]))

(define (add-to-mutable-dict! name mdd elems del seq initial-args)
  (let loop ([args initial-args]
             [elems elems]
             [seq seq])
    (cond
      [(pair? args)
       (cond
         [(pair? (cdr args))
          (define key (car args))
          (define val (cadr args))
          (let-values ([(elems seq) (update-elems+seq elems seq key val)])
            (loop (cddr args) elems seq))]
         [else
          (raise-argument-error
           name
           "an even number of arguments"
           initial-args)])]
      [(null? args) (unless (try-update-mddict-content! mdd elems del seq)
                      (add-to-mutable-dict! name mdd elems del seq initial-args))]
      [else (error name "impossible! you found a bug!")])))

(define mutable-ddict* (mutable-ddict-constructor mutable-ddict #hash()))
(define mutable-ddicteqv* (mutable-ddict-constructor mutable-ddicteqv #hasheqv()))
(define mutable-ddicteq* (mutable-ddict-constructor mutable-ddicteq #hasheq()))


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
            (let-values ([(elems seq) (update-elems+seq elems seq (car p) (cdr p))])
              (loop rst elems seq))]
           [else (raise-argument-error (quote name)
                                       "(listof pair?)"
                                       initial-alist)])]
        [(null? alist) (iddict elems 0 seq)]
        [else (raise-argument-error (quote name)
                                    "(listof pair?)"
                                    initial-alist)]))))

(define make-ddict (make-ddict/template make-ddict #hash()))
(define make-ddicteqv (make-ddict/template make-ddicteqv #hasheqv()))
(define make-ddicteq (make-ddict/template make-ddicteq #hasheq()))

;; "make-" constructor template for mutable ddicts
(define-syntax-rule (make-mutable-ddict/template name init-hash)
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
            (let-values ([(elems seq) (update-elems+seq elems seq (car p) (cdr p))])
              (loop rst elems seq))]
           [else (raise-argument-error (quote name)
                                       "(listof pair?)"
                                       initial-alist)])]
        [(null? alist) (mddict elems 0 seq)]
        [else (raise-argument-error (quote name)
                                    "(listof pair?)"
                                    initial-alist)]))))

(define make-mutable-ddict (make-mutable-ddict/template make-ddict #hash()))
(define make-mutable-ddicteqv (make-mutable-ddict/template make-ddicteqv #hasheqv()))
(define make-mutable-ddicteq (make-mutable-ddict/template make-ddicteq #hasheq()))

(define-for-syntax (parse-i-bindings dd-id elems-id del-id seq-id)
  (with-syntax ([dd dd-id]
                [elems elems-id]
                [del del-id]
                [seq seq-id])
    (append
     (if (eq? '_ (syntax->datum #'elems))
         (list)
         (list #'[elems (unsafe-struct*-ref dd 0)]))
     (if (eq? '_ (syntax->datum #'del))
         (list)
         (list #'[del (unsafe-struct*-ref dd 1)]))
     (if (eq? '_ (syntax->datum #'seq))
         (list)
         (list #'[seq (unsafe-struct*-ref dd 2)])))))

(define-for-syntax (parse-m-bindings dd-id elems-id del-id seq-id)
  (with-syntax ([dd dd-id]
                [elems elems-id]
                [del del-id]
                [seq seq-id]
                [content #`#,(gensym 'content)])
    (let ([bindings (append
                     (if (eq? '_ (syntax->datum #'elems))
                         (list)
                         (list #'[elems (unsafe-vector*-ref content 0)]))
                     (if (eq? '_ (syntax->datum #'del))
                         (list)
                         (list #'[del (unsafe-vector*-ref content 1)]))
                     (if (eq? '_ (syntax->datum #'seq))
                         (list)
                         (list #'[seq (unsafe-vector*-ref content 2)])))])
      (if (null? bindings)
          '()
          (cons #'[content (unsafe-unbox* (unsafe-struct*-ref dd 0))]
                bindings)))))

;; macro for defining functions whose first argument is a ddict
;;
;; This automatically inserts a ddict? check (or immutable/mutable-ddict?)
;; and raise-argument-error for failure, as well as pattern matching
;; out the ddict's fields quickly after the check is complete
(define-syntax (define/dd-match stx)
  (syntax-case stx (ddict iddict mddict)
    ;; ddict function for both mutable and immutable
    [(_ (name dd . other-args)
        [(iddict i-elems i-n i-seq) . i-body]
        [(mddict m-elems m-n m-seq) . m-body])
     (and (identifier? #'i-elems) (identifier? #'i-n) (identifier? #'i-seq)
          (identifier? #'m-elems) (identifier? #'m-n) (identifier? #'m-seq))
     (with-syntax ([i-bindings (parse-i-bindings #'dd #'i-elems #'i-n #'i-seq)]
                   [m-bindings (parse-m-bindings #'dd #'m-elems #'m-n #'m-seq)])
       (quasisyntax/loc stx
         (define (name dd . other-args)
           (cond
             [(immutable-ddict? dd) #,(syntax/loc #'i-body (let i-bindings . i-body))]
             [(mutable-ddict? dd) #,(syntax/loc #'m-body (let* m-bindings . m-body))]
             [else (raise-argument-error (quote name) "ddict?" dd)]))))]
    ;; only immutable ddict function
    [(_ (name dd . other-args)
        [(iddict elems n seq) . body])
     (and (identifier? #'elems) (identifier? #'n) (identifier? #'seq))
     (with-syntax ([bindings (parse-i-bindings #'dd #'elems #'n #'seq)])
       (quasisyntax/loc stx
         (define (name dd . other-args)
           (cond
             [(immutable-ddict? dd) #,(syntax/loc #'body (let bindings . body))]
             [else (raise-argument-error (quote name) "immutable-ddict?" dd)]))))]
    ;; only mutable ddict function
    [(_ (name dd . other-args)
        [(mddict elems n seq) . body])
     (and (identifier? #'elems) (identifier? #'n) (identifier? #'seq))
     (with-syntax ([bindings (parse-m-bindings #'dd #'elems #'n #'seq)])
       (quasisyntax/loc stx
         (define (name dd . other-args)
           (cond
             [(mutable-ddict? dd) #,(syntax/loc #'body (let* bindings . body))]
             [else (raise-argument-error (quote name) "mutable-ddict?" dd)]))))]))

(define/dd-match (ddict-equal? dd)
  [(iddict elems _ _) (hash-equal? elems)]
  [(mddict elems _ _) (hash-equal? elems)])
(define/dd-match (ddict-eqv? dd)
  [(iddict elems _ _) (hash-eqv? elems)]
  [(mddict elems _ _) (hash-eqv? elems)])
(define/dd-match (ddict-eq? dd)
  [(iddict elems _ _) (hash-eq? elems)]
  [(mddict elems _ _) (hash-eq? elems)])


(define/dd-match (immutable-ddict-elems+seq mdd)
  [(iddict elems _ seq) (values elems seq)])

(define/dd-match (mutable-ddict-elems mdd)
  [(mddict elems _ _) elems])

(define/dd-match (mutable-ddict-elems+seq mdd)
  [(mddict elems _ seq) (values elems seq)])

(define/dd-match (mutable-ddict-seq mdd)
  [(mddict _ _ seq) seq])

;;
;; ddict-set
;;
(define/dd-match (ddict-set dd key val)
  [(iddict elems del seq)
   (let-values ([(elems seq) (update-elems+seq elems seq key val)])
     (iddict elems del seq))])

;;
;; ddict-set!
;;
(define/dd-match (ddict-set! mdd key val)
  [(mddict elems del seq)
   (let-values ([(elems seq) (update-elems+seq elems seq key val)])
     (unless (try-update-mddict-content! mdd elems del seq)
       (ddict-set! mdd key val)))])


;;
;; ddict-ref
;;
(define/dd-match (ddict-ref dd key [failure (no-key-err-thunk ddict-ref key)])
  [(iddict elems _ _) (hash-ref elems key failure)]
  [(mddict elems _ _) (hash-ref elems key failure)])

;;
;; ddict-ref!
;;
(define/dd-match (ddict-ref! mdd key to-set)
  [(mddict elems del seq)
   (define val (hash-ref elems key *missing*))
   (cond
     [(eq? val *missing*)
      (define val (default to-set))
      (let-values ([(elems seq) (update-elems+seq elems seq key val)])
        (if (try-update-mddict-content! mdd elems del seq)
            val
            (ddict-set! mdd key val)))]
     [else val])])


;;;
;;; ddict-update
;;;
(define/dd-match (ddict-update dd key updater [failure (no-key-err-thunk ddict-update key)])
  [(iddict elems del seq)
   (unless (and (procedure? updater)
                (procedure-arity-includes? updater 1))
     (raise-argument-error 'ddict-update "(any/c . -> . any/c)" updater))
   (define prev-val (hash-ref elems key failure))
   (let-values ([(elems seq) (update-elems+seq elems seq key (updater prev-val))])
     (iddict elems del seq))])

;;
;; ddict-update!
;;
(define/dd-match (ddict-update! mdd key updater [failure (no-key-err-thunk ddict-update! key)])
  [(mddict elems del seq)
   (unless (and (procedure? updater)
                (procedure-arity-includes? updater 1))
     (raise-argument-error 'ddict-update! "(any/c . -> . any/c)" updater))
   (define val (updater (hash-ref elems key failure)))
   (let-values ([(elems seq) (update-elems+seq elems seq key val)])
     (unless (try-update-mddict-content! mdd elems del seq)
       (ddict-set! mdd key val)))])

;;
;; ddict-set*
;;
(define/dd-match (ddict-set* dd . args)
  [(iddict elems del seq)
   (build-immutable-ddict 'ddict-set* elems del seq args)])

;;
;; ddict-set*!
;;
(define/dd-match (ddict-set*! mdd . initial-args)
  [(mddict _ _ _)
   (let loop ([args initial-args])
     (cond
       [(pair? args)
        (define key (car args))
        (define rst (cdr args))
        (cond
          [(pair? rst)
           (define val (car rst))
           (ddict-set! mdd key val)
           (loop (cdr rst))]
          [else
           (raise-argument-error
            'ddict-set*!
            "an even number of arguments"
            initial-args)])]
       [(null? args) (void)]
       [else (raise-argument-error
              'ddict-set*!
              "a list of keys and values"
              initial-args)]))])

(define-syntax-rule (too-fragmented? elems del)
  (>= del (hash-count elems)))

;;
;; ddict-remove
;;
(define/dd-match (ddict-remove dd key)
  [(iddict elems del seq)
   (let ([elems (hash-remove elems key)])
     (if (too-fragmented? elems del)
         (iddict elems 0 (filter-seq elems seq))
         (iddict elems (add1 del) seq)))])

(define-syntax-rule (filter-seq elems seq)
  (for/list ([keyb (in-list seq)]
             #:unless (eq? *missing* (hash-ref elems (unbox-key keyb) *missing*)))
    keyb))

;;
;; ddict-remove!
;;
(define/dd-match (ddict-remove! mdd key)
  [(mddict elems del seq)
   (let ([elems (hash-remove elems key)])
     (cond
       [(too-fragmented? elems del)
        (unless (try-update-mddict-content! mdd elems 0 (filter-seq elems seq))
          (ddict-remove! mdd key))]
       [else
        (unless (try-update-mddict-content! mdd elems (add1 del) seq)
          (ddict-remove! mdd key))]))])


;;
;; ddict-clear!
;;
(define/dd-match (ddict-clear! mdd)
  [(mddict elems _ _)
   (fore-update-mddict-content! mdd (hash-clear elems) 0 '())])

;;
;; ddict-clear
;;
(define/dd-match (ddict-clear dd)
  [(iddict elems _ _)
   (iddict (hash-clear elems) 0 '())])

;;;
;;; ddict-copy-clear
;;;
(define/dd-match (ddict-copy-clear dd)
  [(iddict elems _ _) (iddict (hash-clear elems) 0 '())]
  [(mddict elems _ _) (mddict (hash-clear elems) 0 '())])

;;
;; ddict-copy
;;
(define/dd-match (ddict-copy dd)
  [(mddict elems del seq) (mddict elems del seq)])

;;
;; ddict-has-key?
;;
(define/dd-match (ddict-has-key? dd key)
  [(iddict elems _ _) (hash-has-key? elems key)]
  [(mddict elems _ _) (hash-has-key? elems key)])

;;
;; ddict-empty?
;;
(define/dd-match (ddict-empty? dd)
  [(iddict elems _ _) (hash-empty? elems)]
  [(mddict elems _ _) (hash-empty? elems)])

;;
;; ddict-count
;;
(define/dd-match (ddict-count dd)
  [(iddict elems _ _) (hash-count elems)]
  [(mddict elems _ _) (hash-count elems)])

;;
;; ddict-compact?
;;
(define/dd-match (ddict-compact? dd)
  [(iddict _ del _) (eqv? 0 del)]
  [(mddict _ del _) (eqv? 0 del)])

;;
;; ddict-compact
;;
(define/dd-match (ddict-compact dd)
  [(iddict elems del seq)
   (cond
     [(eqv? 0 del) dd]
     [else (iddict elems 0 (for*/list ([keyb (in-list seq)]
                                       [key (in-value (unbox-key keyb))]
                                       #:when (hash-has-key? elems key))
                             key))])])

;;
;; ddict-compact!
;;
(define/dd-match (ddict-compact! mdd)
  [(mddict elems del seq)
   (unless (eqv? 0 del)
     (let ([seq (for*/list ([keyb (in-list seq)]
                            [key (in-value (unbox-key keyb))]
                            #:when (hash-has-key? elems key))
                  key)])
       (unless (try-update-mddict-content! mdd elems 0 seq)
         (ddict-compact! mdd))))])

;;
;; ddict-keys
;;
(define/dd-match (ddict-keys dd)
  [(iddict elems del seq)
   (for*/list ([keyb (in-list seq)]
               [key (in-value (unbox-key keyb))]
               #:when (hash-has-key? elems key))
     key)]
  [(mddict elems del seq)
   (for*/list ([keyb (in-list seq)]
               [key (in-value (unbox-key keyb))]
               #:when (hash-has-key? elems key))
     key)])

;; 
;; ddict-values
;; 
(define/dd-match (ddict-values dd)
  [(iddict elems _ seq)
   (for*/list ([keyb (in-list seq)]
               [val (in-value (hash-ref elems (unbox-key keyb) *missing*))]
               #:unless (eq? val *missing*))
     val)]
  [(mddict elems _ seq)
   (for*/list ([keyb (in-list seq)]
               [val (in-value (hash-ref elems (unbox-key keyb) *missing*))]
               #:unless (eq? val *missing*))
     val)])

;;
;; ddict->list
;;
(define/dd-match (ddict->list dd)
  [(iddict elems _ seq)
   (for*/list ([keyb (in-list seq)]
               [key (in-value (unbox-key keyb))]
               [val (in-value (hash-ref elems key *missing*))]
               #:unless (eq? *missing* val))
     (cons key val))]
  [(mddict elems _ seq)
   (for*/list ([keyb (in-list seq)]
               [key (in-value (unbox-key keyb))]
               [val (in-value (hash-ref elems key *missing*))]
               #:unless (eq? *missing* val))
     (cons key val))])

;;
;; ddict-map
;;
(define/dd-match (ddict-map dd f)
  [(iddict elems _ seq)
   (unless (and (procedure? f)
                (procedure-arity-includes? f 2))
     (raise-argument-error 'ddict-map "(any/c any/c . -> . any/c)" f))
   (for*/list ([keyb (in-list seq)]
               [key (in-value (unbox-key keyb))]
               [val (in-value (hash-ref elems key *missing*))]
               #:unless (eq? *missing* val))
     (f key val))]
  [(mddict elems _ seq)
   (unless (and (procedure? f)
                (procedure-arity-includes? f 2))
     (raise-argument-error 'ddict-map "(any/c any/c . -> . any/c)" f))
   (for*/list ([keyb (in-list seq)]
               [key (in-value (unbox-key keyb))]
               [val (in-value (hash-ref elems key *missing*))]
               #:unless (eq? *missing* val))
     (f key val))])

;;
;; ddict-for-each
;;
(define/dd-match (ddict-for-each dd f)
  [(iddict elems _ seq)
   (unless (and (procedure? f)
                (procedure-arity-includes? f 2))
     (raise-argument-error 'ddict-for-each "(any/c any/c . -> . any/c)" f))
   (for* ([keyb (in-list seq)]
          [key (in-value (unbox-key keyb))]
          [val (in-value (hash-ref elems key *missing*))]
          #:unless (eq? *missing* val))
     (f key val))]
  [(mddict elems _ seq)
   (unless (and (procedure? f)
                (procedure-arity-includes? f 2))
     (raise-argument-error 'ddict-for-each "(any/c any/c . -> . any/c)" f))
   (for* ([keyb (in-list seq)]
          [key (in-value (unbox-key keyb))]
          [val (in-value (hash-ref elems key *missing*))]
          #:unless (eq? *missing* val))
     (f key val))])

(define-syntax-rule (unequal-hashes kind1 kind2)
  (raise (make-exn:fail:contract
          (format "ddict-keys-subset?: given ddicts do not use the same key comparison:\n ddict 1: ~a\n ddict 2: ~a"
                  kind1 kind2)
          (current-continuation-marks))))

(define/dd-match (ddict-keys-subset? dd1 dd2)
  [(iddict elems1 _ _)
   (unless (immutable-ddict? dd2)
     (raise-argument-error 'ddict-keys-subset?
                           "immutable-ddict?"
                           dd2))
   (define elems2 (immutable-ddict-elems dd2))
   (cond [(hash-equal? elems1)
          (cond [(hash-eqv? elems2) (unequal-hashes "ddict" "ddicteqv")]
                [(hash-eq? elems2) (unequal-hashes "ddict" "ddicteq")])]
         [(hash-eqv? elems1)
          (cond [(hash-equal? elems2) (unequal-hashes "ddicteqv" "ddict")]
                [(hash-eq? elems2) (unequal-hashes "ddicteqv" "ddicteq")])]
         [else
          (cond [(hash-equal? elems2) (unequal-hashes "ddicteq" "ddict")]
                [(hash-eqv? elems2) (unequal-hashes "ddicteq" "ddicteqv")])])
   (hash-keys-subset? elems1 elems2)]
  [(mddict elems1 _ _)
   (unless (mutable-ddict? dd2)
     (raise-argument-error 'ddict-keys-subset?
                           "mutable-ddict?"
                           dd2))
   (define elems2 (mutable-ddict-elems dd2))
   (cond [(hash-equal? elems1)
          (cond [(hash-eqv? elems2) (unequal-hashes "mutable-ddict" "mutable-ddicteqv")]
                [(hash-eq? elems2) (unequal-hashes "mutable-ddict" "mutable-ddicteq")])]
         [(hash-eqv? elems1)
          (cond [(hash-equal? elems2) (unequal-hashes "mutable-ddicteqv" "mutable-ddict")]
                [(hash-eq? elems2) (unequal-hashes "mutable-ddicteqv" "mutable-ddicteq")])]
         [else
          (cond [(hash-equal? elems2) (unequal-hashes "mutable-ddicteq" "mutable-ddict")]
                [(hash-eqv? elems2) (unequal-hashes "mutable-ddicteq" "mutable-ddicteqv")])])
   (hash-keys-subset? elems1 elems2)])

;;
;; next-key/val
;;
(define-syntax-rule (next-key/val elems seq)
  (cond
    [(pair? seq)
     (define key (unbox-key (car seq)))
     (define val (hash-ref elems key *missing*))
     (if (eq? val *missing*)
         (next-key/val-proc elems (cdr seq))
         (values key val (cdr seq)))]
    [else (values #f #f #f)]))

(define (next-key/val-proc elems seq)
  (cond
    [(pair? seq)
     (define key (unbox-key (car seq)))
     (define val (hash-ref elems key *missing*))
     (if (eq? val *missing*)
         (next-key/val-proc elems (cdr seq))
         (values key val (cdr seq)))]
    [else (values #f #f #f)]))

;;
;; in-ddict-proc
;;
(define ((in-ddict-proc name pred? pred-str) dd)
  (cond
    [(pred? dd)
     (define alist (ddict->list dd))
     (define-values (keys vals)
       (for/lists (ks vs)
         ([p (in-list alist)])
         (values (car p) (cdr p))))
     (in-parallel keys vals)]
    [else (raise-argument-error name pred-str dd)]))

(define-syntax-rule (get-elems+seq name dd-exp)
  (let ([dd dd-exp])
    (cond [(immutable-ddict? dd)
           (immutable-ddict-elems+seq dd)]
          [(mutable-ddict? dd)
           (mutable-ddict-elems+seq dd)]
          [else (raise-argument-error
                 (quote name)
                 "ddict?"
                 dd)])))

;;
;; in-ddict
;;
(define-sequence-syntax in-ddict
  (λ () #'(in-ddict-proc 'in-ddict ddict? "ddict?"))
  (λ (stx)
    (syntax-case stx ()
      [[(key val) (_ dd-exp)]
       #'[(key val)
          (:do-in
           ;; ([(outer-id ...) outer-expr] ...)
           ([(elems seq) (get-elems+seq in-ddict dd-exp)])
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
           (rst))]]
      [[xs (_ dd-exp)]
       (list? (syntax->datum #'xs))
       (raise-syntax-error 'in-ddict
                           (format "expected an identifier list of length 2, given ~a"
                                   (length (syntax->list #'xs)))
                           #'xs)]
      [blah (raise-syntax-error 'in-ddict "invalid usage" #'blah)])))

(define (next-key elems seq)
  (cond
    [(pair? seq)
     (define key (unbox-key (car seq)))
     (if (hash-has-key? elems key)
         (values key (cdr seq))
         (next-key-proc elems (cdr seq)))]
    [else (values #f #f)]))

(define (next-key-proc elems seq)
  (cond
    [(pair? seq)
     (define key (unbox-key (car seq)))
     (if (hash-has-key? elems key)
         (values key (cdr seq))
         (next-key-proc elems (cdr seq)))]
    [else (values #f #f)]))




;; in-ddict-keys-proc
(define ((in-ddict-keys-proc name pred? pred-str) dd)
  (cond
    [(pred? dd) (ddict-keys dd)]
    [else (raise-argument-error name pred-str dd)]))


;;
;; in-ddict-keys
;;
(define-sequence-syntax in-ddict-keys
  (λ () #'(in-ddict-keys-proc 'in-ddict-keys
                              ddict?
                              "ddict?"))
  (λ (stx)
    (syntax-case stx ()
      [[(key) (_ dd-exp)]
       #'[(key)
          (:do-in
           ;; ([(outer-id ...) outer-expr] ...)
           ([(elems seq) (get-elems+seq in-ddict-keys dd-exp)])
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
           (rst))]]
      [[xs (_ dd-exp)]
       (list? (syntax->datum #'xs))
       (raise-syntax-error 'in-ddict-keys
                           (format "expected a single identifier, given ~a"
                                   (length (syntax->list #'xs)))
                           #'xs)]
      [blah (raise-syntax-error 'in-ddict-keys "invalid usage" #'blah)])))



;;
;; next-val
;;
(define-syntax-rule (next-val elems seq)
  (cond
    [(pair? seq)
     (define val (hash-ref elems (unbox-key (car seq)) *missing*))
     (if (eq? val *missing*)
         (next-val-proc elems (cdr seq))
         (values val (cdr seq)))]
    [else (values #f #f)]))

(define (next-val-proc elems seq)
  (cond
    [(pair? seq)
     (define val (hash-ref elems (unbox-key (car seq)) *missing*))
     (if (eq? val *missing*)
         (next-val-proc elems (cdr seq))
         (values val (cdr seq)))]
    [else (values #f #f)]))

(define ((in-ddict-values-proc name pred? pred-str) dd)
  (cond
    [(pred? dd) (ddict-values dd)]
    [else (raise-argument-error name pred-str dd)]))

(define get-elems+seq-for-in-ddict-vals
  (let ()
    (define/dd-match (in-ddict-values dd)
      [(iddict elems _ seq) (values elems seq)]
      [(mddict elems _ seq) (values elems seq)])
    in-ddict-values))

;;
;; in-ddict-values
;;
(define-sequence-syntax in-ddict-values
  (λ () #'(in-ddict-values-proc 'in-ddict-values ddict? "ddict?"))
  (λ (stx)
    (syntax-case stx ()
      [[(val) (_ dd-exp)]
       #'[(val)
          (:do-in
           ;; ([(outer-id ...) outer-expr] ...)
           ([(elems seq) (get-elems+seq in-ddict-values dd-exp)])
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
           (rst))]]
      [[xs (_ dd-exp)]
       (list? (syntax->datum #'xs))
       (raise-syntax-error 'in-ddict-values
                           (format "expected a single identifier, given ~a"
                                   (length (syntax->list #'xs)))
                           #'xs)]
      [blah (raise-syntax-error 'in-ddict-values "invalid usage" #'blah)])))

(define-syntax-rule (define-for-ddict for-name for/derived mk empty-hash)
  (define-syntax (for-name stx)
    (syntax-case stx ()
      [(_ clauses . defs+exprs)
       (with-syntax ([original stx])
         (syntax/loc stx
           (let-values
               ([(elems seq)
                 (for/derived original
                   ([elems empty-hash]
                    [seq '()])
                   clauses
                   (let-values ([(key val) (let () . defs+exprs)])
                     (update-elems+seq elems seq key val)))])
             (mk elems 0 seq))))])))



(define-for-ddict for/ddict     for/fold/derived  iddict #hash())
(define-for-ddict for/ddicteqv  for/fold/derived  iddict #hasheqv())
(define-for-ddict for/ddicteq   for/fold/derived  iddict #hasheq())
(define-for-ddict for*/ddict    for*/fold/derived iddict #hash())
(define-for-ddict for*/ddicteqv for*/fold/derived iddict #hasheqv())
(define-for-ddict for*/ddicteq  for*/fold/derived iddict #hasheq())
(define-for-ddict for/mutable-ddict     for/fold/derived  mddict #hash())
(define-for-ddict for/mutable-ddicteqv  for/fold/derived  mddict #hasheqv())
(define-for-ddict for/mutable-ddicteq   for/fold/derived  mddict #hasheq())
(define-for-ddict for*/mutable-ddict    for*/fold/derived mddict #hash())
(define-for-ddict for*/mutable-ddicteqv for*/fold/derived mddict #hasheqv())
(define-for-ddict for*/mutable-ddicteq  for*/fold/derived mddict #hasheq())
