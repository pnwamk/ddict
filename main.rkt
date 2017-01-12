#lang racket/base

(require racket/unsafe/ops
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
         ddict-compact?
         ddict-compact!
         ddict-empty?
         ddict-count
         ddict-keys
         ddict-values
         ddict->list
         in-ddict
         ddict-map
         ddict-for-each)

(define-syntax-rule (in? elems)
  (λ (x) (hash-has-key? elems x)))

(define-syntax-rule (too-fragmented? elems del)
  (> del (arithmetic-shift (hash-count elems) -1)))

(define-syntax-rule (ref-fail name-sym)
  (λ () (error name-sym "invalid ddict state! (possibly from concurrent mutation)")))

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

(define-syntax-rule (unsafe-ddict-del dd)          (unsafe-struct-ref  dd 1))
(define-syntax-rule (unsafe-set-ddict-del! dd val) (unsafe-struct-set! dd 1 val))

(define-syntax-rule (unsafe-ddict-seq dd)          (unsafe-struct-ref  dd 2))
(define-syntax-rule (unsafe-set-ddict-seq! dd val) (unsafe-struct-set! dd 2 val))

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
  (let ([l (filter (in? elems) (ddict-seq dd))]
        [recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (λ (p port) (print p port mode))])])
    (when (not (null? l))
      (for* ([key (in-list l)]
             [val (in-value (hash-ref elems key (ref-fail 'ddict-print)))])
        (write-string " " port)
        (recur (cons key val) port))))
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
           (quote name)
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
      [(null? args) (unsafe-set-ddict-del! dd 0)
                    (unsafe-set-ddict-seq! dd seq)]
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
                  [prev-count (hash-count elems)]
                  [elems (hash-set elems key val)])
        (if (eqv? (hash-count elems) prev-count)
            (values elems key)
            (values elems (cons key seq)))))
    (immutable-ddict elems 0 seq)))

(define make-ddict (make-ddict/template make-ddict #hash()))
(define make-ddicteqv (make-ddict/template make-ddicteqv #hasheqv()))
(define make-ddicteq (make-ddict/template make-ddicteq #hasheq()))

;; "make-" constructor template for mutable ddicts
(define-syntax-rule (make-mutable-ddict/template name make-init-hash)
  (λ (alist)
    (define elems (make-init-hash))
    (define seq
      (for*/fold ([seq '()])
                 ([p (in-list alist)]
                  [key (in-value (car p))]
                  [val (in-value (cdr p))]
                  [prev-count (hash-count elems)])
        (hash-set! elems key val)
        (if (eqv? (hash-count elems) prev-count)
            seq
            (cons key seq))))
    (mutable-ddict elems 0 seq)))

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
                         [failure (λ () (raise-argument-error
                                         'ddict-update
                                         "a key with an entry in the ddict"
                                         key))])
  (unless (and (procedure? updater)
               (procedure-arity-includes? updater 1))
    (raise-argument-error 'ddict-update "procedure with arity 1" updater))
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
                          [failure (λ () (raise-argument-error
                                          'ddict-update
                                          "a key with an entry in the ddict"
                                          key))])
  (unless (and (procedure? updater)
               (procedure-arity-includes? updater 1))
    (raise-argument-error 'ddict-update! "procedure with arity 1" updater))
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
    (unsafe-set-ddict-del! dd del)
    (when (too-fragmented? elems del)
      (unsafe-set-ddict-seq! dd (filter (in? elems) seq)))))

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
  (define initial-count (hash-count elems))
  (define val (hash-ref! elems key to-set))
  (unless (eqv? initial-count (hash-count elems))
    (unsafe-set-ddict-seq! dd (cons key seq)))
  val)

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
  (unsafe-ddict-compact! dd elems del seq))

(define-syntax-rule (unsafe-ddict-compact! dd elems del seq)
  (unless (zero? del)
    (define seq* (filter (in? elems) seq))
    (unsafe-set-ddict-seq! dd seq*)
    (unsafe-set-ddict-del! dd 0)))

;;
;; ddict-clean?
;;
(define/dd (ddict-compact? [(ddict _ del _) dd])
  (zero? del))

;;
;; ddict-keys
;;
(define/dd (ddict-keys [(ddict elems del seq) dd])
  (unsafe-ddict-compact! dd elems del seq)
  (ddict-seq dd))

;; 
;; ddict-values
;; 
(define/dd (ddict-values [(ddict elems del seq) dd])
  (unsafe-ddict-compact! dd elems del seq)
  (for/list ([key (in-list (unsafe-ddict-seq dd))])
    (hash-ref elems key (ref-fail 'ddict-values))))

;;
;; ddict->list
;;
(define/dd (ddict->list [(ddict elems del seq) dd])
  (unsafe-ddict-compact! dd elems del seq)
  (for/list ([key (in-list (unsafe-ddict-seq dd))])
    (cons key (hash-ref elems key (ref-fail 'ddict->list)))))

;;
;; ddict-map
;;
(define/dd (ddict-map [(ddict elems del seq) dd] f)
  (unsafe-ddict-compact! dd elems del seq)
  (for/list ([key (in-list (unsafe-ddict-seq dd))])
    (f key (hash-ref elems key (ref-fail 'ddict-map)))))

;;
;; ddict-for-each
;;
(define/dd (ddict-for-each [(ddict elems del seq) dd] f)
  (unsafe-ddict-compact! dd elems del seq)
  (for ([key (in-list (unsafe-ddict-seq dd))])
    (f key (hash-ref elems key (ref-fail 'ddict-for-each)))))


;;
;; next-valid-pos
;;
(define-syntax-rule (next-valid-pos elems seq)
  (and (pair? seq) seq))


;;
;; in-ddict-proc
;;
(define (in-ddict-proc dd)
  (ddict-compact! dd)
  (cond
    [(ddict? dd)
     (define elems (unsafe-ddict-elems dd))
     (define seq (unsafe-ddict-seq dd))
     (make-do-sequence
      (λ ()
        (values
         (λ (pos)
           (let ([key (car pos)])
             (values key (hash-ref elems key (ref-fail 'in-ddict)))))
         (λ (pos) (next-valid-pos elems (cdr pos)))
         (next-valid-pos elems seq)
         values
         #f
         #f)))]
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
               (ddict-compact! dd)
               (values (unsafe-ddict-elems dd)
                       (unsafe-ddict-seq dd)))])
           ;; outer-check
           #t
           ;; ([loop-id loop-expr] ...)
           ([pos seq])
           ;; pos-guard
           (pair? pos)
           ;; ([(inner-id ...) inner-expr] ...)
           ([(key val rst)
             (let ([a (car pos)]
                   [d (cdr pos)])
               (values a
                       (hash-ref elems a (ref-fail 'in-ddict))
                       d))])
           ;; pre-guard
           #t
           ;; post-guard
           #t
           ;; (loop-arg ...)
           (rst))]])))






#;(module+ test
  (define (h1) (hash "a" 'a "b" 'b "c" 'c "d" 'd "e" 'e "f" 'f))
  (define (h2) (hash "a1" 'a "b1" 'b "c1" 'c "d1" 'd "e1" 'e "f1" 'f
                     "a2" 'a "b2" 'b "c2" 'c "d2" 'd "e2" 'e "f2" 'f
                     "a3" 'a "b3" 'b "c3" 'c "d3" 'd "e3" 'e "f3" 'f))
  (define (dd1) (ddict* "a" 'a "b" 'b "c" 'c "d" 'd "e" 'e "f" 'f))
  (define (dd2) (ddict* "a1" 'a "b1" 'b "c1" 'c "d1" 'd "e1" 'e "f1" 'f
                        "a2" 'a "b2" 'b "c2" 'c "d2" 'd "e2" 'e "f2" 'f
                        "a3" 'a "b3" 'b "c3" 'c "d3" 'd "e3" 'e "f3" 'f))
  (define (dirty-dd1) (ddict-remove (ddict* "a" 'a "b" 'b "c" 'c "d" 'd "e" 'e "f" 'f "g" 'g) "g"))
  (define (dirty-dd2) (ddict-remove (ddict* "a1" 'a "b1" 'b "c1" 'c "d1" 'd "e1" 'e "f1" 'f
                                            "a2" 'a "b2" 'b "c2" 'c "d2" 'd "e2" 'e "f2" 'f
                                            "a3" 'a "b3" 'b "c3" 'c "d3" 'd "e3" 'e "f3" 'f
                                            "g" 'g) "g"))

  (displayln "\nin-* tests\n")
  (displayln "in-hash (6)")
  (time (for ([_ (in-range 1000000)])
          (for ([(k v) (in-hash (h1))])
            (void))))

  (collect-garbage)
  (collect-garbage)

  (displayln "in-hash (18)")
  (time (for ([_ (in-range 1000000)])
          (for ([(k v) (in-hash (h2))])
            (void))))

  (collect-garbage)
  (collect-garbage)

  (displayln "in-ddict (6)")
  (time (for ([_ (in-range 1000000)])
          (for ([(k v) (in-ddict (dd1))])
            (void))))

  (collect-garbage)
  (collect-garbage)

  (displayln "in-ddict (18)")
  (time (for ([_ (in-range 1000000)])
          (for ([(k v) (in-ddict (dd2))])
            (void))))

  (collect-garbage)
  (collect-garbage)

  (displayln "dirty in-ddict (6)")
  (time (for ([_ (in-range 1000000)])
          (for ([(k v) (in-ddict (dirty-dd1))])
            (void))))

  (collect-garbage)
  (collect-garbage)

  (displayln "dirty in-ddict (18)")
  (time (for ([_ (in-range 1000000)])
          (for ([(k v) (in-ddict (dirty-dd2))])
            (void))))

  (collect-garbage)
  (collect-garbage)

  (displayln "in-clean-ddict (6)")
  (time (for ([_ (in-range 1000000)])
          (for ([(k v) (in-clean-ddict (dd1))])
            (void))))

  (collect-garbage)
  (collect-garbage)

  (displayln "in-clean-ddict (18)")
  (time (for ([_ (in-range 1000000)])
          (for ([(k v) (in-clean-ddict (dd2))])
            (void))))

  (collect-garbage)
  (collect-garbage)

  (displayln "dirty in-clean-ddict (6)")
  (time (for ([_ (in-range 1000000)])
          (for ([(k v) (in-clean-ddict (dirty-dd1))])
            (void))))

  (collect-garbage)
  (collect-garbage)

  (displayln "dirty in-clean-ddict (18)")
  (time (for ([_ (in-range 1000000)])
          (for ([(k v) (in-clean-ddict (dirty-dd2))])
            (void))))

  ;(displayln "\n*-set tests")
  ;(displayln "hash")
  ;(time (for ([_ (in-range 1000000)])
  ;        (hash-set h "a" 42)
  ;        (hash-set h 42 42)))
  ;
  ;(collect-garbage)
  ;(collect-garbage)
  ;
  ;(displayln "ddict")
  ;(time (for ([_ (in-range 1000000)])
  ;        (ddict-set dd "a" 42)
  ;        (ddict-set dd 42 42)))
  ;
  ;(displayln "\n*-ref tests")
  ;(displayln "hash")
  ;(time (for ([_ (in-range 1000000)])
  ;        (hash-ref h "a")
  ;        (hash-ref h 42 #f)))
  ;
  ;(collect-garbage)
  ;(collect-garbage)
  ;
  ;(displayln "ddict")
  ;(time (for ([_ (in-range 1000000)])
  ;        (ddict-ref dd "a")
  ;        (ddict-ref dd 42 #f)))
  ;
  ;
  ;(displayln "\n*-remove tests")
  ;(displayln "hash")
  ;(time (for ([_ (in-range 1000000)])
  ;        (hash-remove h "a")
  ;        (hash-remove h 42)))
  ;
  ;(collect-garbage)
  ;(collect-garbage)
  ;
  ;(displayln "ddict")
  ;(time (for ([_ (in-range 1000000)])
  ;        (ddict-remove dd "a")
  ;        (ddict-remove dd 42)))
  )