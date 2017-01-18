#lang racket/base

(require (only-in racket/unsafe/ops
                  unsafe-struct*-ref
                  unsafe-struct*-set!)
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
         in-ddict
         in-immutable-ddict
         in-mutable-ddict
         in-ddict-keys
         in-immutable-ddict-keys
         in-mutable-ddict-keys
         in-ddict-values
         in-immutable-ddict-values
         in-mutable-ddict-values
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
         ddict-compact!)


(define-syntax-rule (no-key-err-thunk fun-name key)
  (λ () (raise (make-exn:fail:contract
                (format "~a: no value found for key\n key: ~a" (quote fun-name) key)
                (current-continuation-marks)))))


(define *missing* (gensym 'missing))

;; standard "default value call if val is thunk else return val" behavior
(define-syntax-rule (get-default-value val)
  (let ([d val]) (if (procedure? d) (d) d)))


;; 
;; ddict-print
;; 
(define (ddict-print dd port mode)
  (if mode
      (if (immutable? (elems-of dd))
          (write-string "#<ddict: " port)
          (write-string "#<mutable-ddict: " port))
      (if (immutable? (elems-of dd))
          (write-string "(ddict " port)
          (write-string "(mutable-ddict " port)))
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
  (rec-equal? (elems-of dd1)
              (elems-of dd2)))

;; 
;; ddict-hash-code
;; 
(define (ddict-hash-code dd rec-hc)
  (rec-hc (elems-of dd)))


(struct immutable-ddict (elems [del #:mutable] [seq #:mutable])
  #:methods gen:equal+hash
  [(define equal-proc ddict=?)
   (define hash-proc ddict-hash-code)
   (define hash2-proc ddict-hash-code)]
  #:methods gen:custom-write
  [(define write-proc ddict-print)])

(struct mutable-ddict (elems [del #:mutable] seq-box)
  #:methods gen:equal+hash
  [(define equal-proc ddict=?)
   (define hash-proc ddict-hash-code)
   (define hash2-proc ddict-hash-code)]
  #:methods gen:custom-write
  [(define write-proc ddict-print)])

(define (ddict? x) (or (immutable-ddict? x) (mutable-ddict? x)))

;; NOTE: keep these in sync w/ above defs!!!!!!
(define-syntax-rule (elems-of dd)     (unsafe-struct*-ref dd 0))
(define-syntax-rule (del-of dd)         (unsafe-struct*-ref  dd 1))
(define-syntax-rule (set-del! dd val)   (unsafe-struct*-set! dd 1 val))
(define-syntax-rule (seq-of dd)       (unsafe-struct*-ref  dd 2))
(define-syntax-rule (set-seq! dd val) (unsafe-struct*-set! dd 2 val))


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
             [seq init-seq])
    (cond
      [(pair? args)
       (cond
         [(pair? (cdr args))
          (define key (car args))
          (define val (cadr args))
          (let-values ([(elems seq) (unsafe-iddict-set elems seq key val)])
            (loop (cddr args) elems seq))]
         [else
          (raise-argument-error
           name
           "an even number of arguments"
           initial-args)])]
      [(null? args) (immutable-ddict elems del seq)]
      [else (error name "impossible! you found a bug!")])))

(define ddict*    (immutable-ddict-constructor ddict empty-ddict #hash()))
(define ddicteqv* (immutable-ddict-constructor ddicteqv empty-ddicteqv #hasheqv()))
(define ddicteq*   (immutable-ddict-constructor ddicteq empty-ddicteq #hasheq()))

;; constructor template for mutable ddicts
(define-syntax-rule (mutable-ddict-constructor name make-init-hash)
  (case-lambda
    [() (mutable-ddict (make-init-hash) 0 (box '()))]
    [args (define elems (make-init-hash))
          (define seq-box (box '()))
          (add-to-mutable-dict! (quote name) elems seq-box args)
          (mutable-ddict elems 0 seq-box)]))

(define (add-to-mutable-dict! name elems seq-box initial-args)
  (unless (box? seq-box)
    (error name "not a box! args:\n elems: ~a\n seq-box: ~a\n initial-args: ~a\n\n"
           elems seq-box initial-args))
  (let loop ([args initial-args])
    (cond
      [(pair? args)
       (cond
         [(pair? (cdr args))
          (define key (car args))
          (define val (cadr args))
          (unsafe-mddict-set! elems seq-box key val)
          (loop (cddr args))]
         [else
          (raise-argument-error
           name
           "an even number of arguments"
           initial-args)])]
      [(null? args) (void)]
      [else (error name "impossible! you found a bug!")])))

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
            (define val (cdr p))
            (let-values ([(elems seq) (unsafe-iddict-set elems seq key val)])
              (loop rst elems seq))]
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
    (define seq-box (box '()))
    (let loop ([alist initial-alist])
      (cond
        [(pair? alist)
         (define p (car alist))
         (define rst (cdr alist))
         (cond
           [(pair? p)
            (define key (car p))
            (define val (cdr p))
            (unsafe-mddict-set! elems seq-box key val)
            (loop rst)]
           [else (raise-argument-error (quote name)
                                       "(listof pair?)"
                                       initial-alist)])]
        [(null? alist) (mutable-ddict elems 0 seq-box)]
        [else (raise-argument-error (quote name)
                                    "(listof pair?)"
                                    initial-alist)]))))

(define make-mutable-ddict (make-mutable-ddict/template make-ddict make-hash))
(define make-mutable-ddicteqv (make-mutable-ddict/template make-ddicteqv make-hasheqv))
(define make-mutable-ddicteq (make-mutable-ddict/template make-ddicteq make-hasheq))

(define-for-syntax (parse-bindings dd-id elems-id del-id seq-id)
  (with-syntax ([dd dd-id]
                [elems elems-id]
                [del del-id]
                [seq seq-id])
    (append
     (if (eq? '_ (syntax->datum #'elems))
         (list)
         (list #'[elems (elems-of dd)]))
     (if (eq? '_ (syntax->datum #'del))
         (list)
         (list #'[del (del-of dd)]))
     (if (eq? '_ (syntax->datum #'seq))
         (list)
         (list #'[seq (seq-of dd)])))))

;; macro for defining functions whose first argument is a ddict
;;
;; This automatically inserts a ddict? check (or immutable/mutable-ddict?)
;; and raise-argument-error for failure, as well as pattern matching
;; out the ddict's fields quickly after the check is complete
(define-syntax (define/dd-match stx)
  (syntax-case stx (ddict iddict mddict)
    ;; ddict, same body function
    [(_ (name dd . other-args)
        [(ddict elems n seq) . body])
     (and (identifier? #'elems) (identifier? #'n) (identifier? #'seq))
     (with-syntax ([bindings (parse-bindings #'dd #'elems #'n #'seq)])
       (quasisyntax/loc stx
         (define (name dd . other-args)
           (cond
             [(ddict? dd) #,(syntax/loc #'body (let bindings . body))]
             [else (raise-argument-error (quote name) "ddict?" dd)]))))]
    ;; ddict function with different bodies
    [(_ (name dd . other-args)
        [(iddict i-elems i-n i-seq) . i-body]
        [(mddict m-elems m-n m-seq) . m-body])
     (and (identifier? #'i-elems) (identifier? #'i-n) (identifier? #'i-seq)
          (identifier? #'m-elems) (identifier? #'m-n) (identifier? #'m-seq))
     (with-syntax ([i-bindings (parse-bindings #'dd #'i-elems #'i-n #'i-seq)]
                   [m-bindings (parse-bindings #'dd #'m-elems #'m-n #'m-seq)])
       (quasisyntax/loc stx
         (define (name dd . other-args)
           (cond
             [(immutable-ddict? dd) #,(syntax/loc #'i-body (let i-bindings . i-body))]
             [(mutable-ddict? dd) #,(syntax/loc #'m-body (let m-bindings . m-body))]
             [else (raise-argument-error (quote name) "ddict?" dd)]))))]
    ;; only immutable ddict function
    [(_ (name dd . other-args)
        [(iddict elems n seq) . body])
     (and (identifier? #'elems) (identifier? #'n) (identifier? #'seq))
     (with-syntax ([bindings (parse-bindings #'dd #'elems #'n #'seq)])
       (quasisyntax/loc stx
         (define (name dd . other-args)
           (cond
             [(immutable-ddict? dd) #,(syntax/loc #'body (let bindings . body))]
             [else (raise-argument-error (quote name) "immutable-ddict?" dd)]))))]
    ;; only mutable ddict function
    [(_ (name dd . other-args)
        [(mddict elems n seq) . body])
     (and (identifier? #'elems) (identifier? #'n) (identifier? #'seq))
     (with-syntax ([bindings (parse-bindings #'dd #'elems #'n #'seq)])
       (quasisyntax/loc stx
         (define (name dd . other-args)
           (cond
             [(mutable-ddict? dd) #,(syntax/loc #'body (let bindings . body))]
             [else (raise-argument-error (quote name) "mutable-ddict?" dd)]))))]))

(define/dd-match (ddict-equal? dd)
  [(ddict elems _ _) (hash-equal? elems)])
(define/dd-match (ddict-eqv? dd)
  [(ddict elems _ _) (hash-eqv? elems)])
(define/dd-match (ddict-eq? dd)
  [(ddict elems _ _) (hash-eq? elems)])

;;
;; ddict-set
;;
(define/dd-match (ddict-set dd key val)
  [(iddict elems del seq)
   (let-values ([(elems seq) (unsafe-iddict-set elems seq key val)])
     (immutable-ddict elems del seq))])

(define-syntax-rule (unsafe-iddict-set elems seq key val)
  (let ([prev-count (hash-count elems)]
        [elems (hash-set elems key val)])
    (values elems (if (eqv? prev-count (hash-count elems))
                      seq
                      (cons (make-weak-box key) seq)))))

;;
;; ddict-set!
;;
(define/dd-match (ddict-set! dd key val)
  [(mddict elems _ seq-box)
   (unsafe-mddict-set! elems seq-box key val)])

(define-syntax-rule (unsafe-mddict-set! elems seq-box key val)
  (let ([maybe-e (hash-ref elems key *missing*)])
    (cond
      [(eq? maybe-e *missing*)
       (define entry (mcons key val))
       (hash-set! elems key entry)
       (define seq (unbox seq-box))
       (update-seq-box! seq-box seq (cons entry seq))]
      [else (set-mcdr! maybe-e val)])))

(define-syntax (update-seq-box! stx)
  (syntax-case stx ()
    [(_ seq-box seq new-seq-construction-exp)
     (and (identifier? #'seq-box) (identifier? #'seq))
     (syntax/loc stx
       (unless (box-cas! seq-box seq new-seq-construction-exp)
         ;; update 'seq', but if we were preempted
         ;; then try again until we succeed in adding 'e'
         ;; w/o dropping an element that appeared before we
         ;; could add the entry 'e'
         (let try-again! ([seq (unbox seq-box)])
           (unless (box-cas! seq-box seq new-seq-construction-exp)
             (try-again! (unbox seq-box))))))]))


;;
;; ddict-ref
;;
(define/dd-match (ddict-ref dd key [failure *missing*])
  [(iddict elems _ _)
   (define val (hash-ref elems key failure))
   (cond
     [(eq? *missing* val)
      (raise (make-exn:fail:contract
              (format "~a: no value found for key\n key: ~a" 'ddict-ref key)
              (current-continuation-marks)))]
     [else val])]
  [(mddict elems _ _)
   (define ret (hash-ref elems key #f))
   (cond
     [(mpair? ret) (mcdr ret)]
     [(eq? failure *missing*)
      (raise (make-exn:fail:contract
              (format "~a: no value found for key\n key: ~a" 'ddict-ref key)
              (current-continuation-marks)))]
     [else (get-default-value failure)])])

;;
;; ddict-ref!
;;
(define/dd-match (ddict-ref! dd key to-set)
  [(mddict elems _ seq-box)
   (define entry (hash-ref elems key #f))
   (cond
     [(mpair? entry) (mcdr entry)]
     [else
      (define val (get-default-value to-set))
      (unsafe-mddict-set! elems seq-box key val)
      val])])


;;;
;;; ddict-update
;;;
(define/dd-match (ddict-update dd key updater [failure *missing*])
  [(iddict elems del seq)
   (unless (and (procedure? updater)
                (procedure-arity-includes? updater 1))
     (raise-argument-error 'ddict-update "(any/c . -> . any/c)" updater))
   (define prev-val (hash-ref elems key failure))
   (cond
     [(eq? *missing* prev-val)
      (raise (make-exn:fail:contract
              (format "~a: no value found for key\n key: ~a" 'ddict-update key)
              (current-continuation-marks)))]
     [else
      (let-values ([(elems seq) (unsafe-iddict-set elems seq key (updater prev-val))])
        (immutable-ddict elems del seq))])])

;;
;; ddict-update!
;;
(define/dd-match (ddict-update! dd key updater [failure *missing*])
  [(mddict elems _ seq-box)
   (unless (and (procedure? updater)
                (procedure-arity-includes? updater 1))
     (raise-argument-error 'ddict-update! "(any/c . -> . any/c)" updater))
   (define ret (hash-ref elems key #f))
   (cond
     [(mpair? ret)
      (define prev-val (mcdr ret))
      (set-mcdr! ret (updater prev-val))]
     [(eq? failure *missing*)
      (raise (make-exn:fail:contract
              (format "~a: no value found for key\n key: ~a" 'ddict-update! key)
              (current-continuation-marks)))]
     [else
      (define entry (mcons key (updater (get-default-value failure))))
      (hash-set! elems key entry)
      (define seq (unbox seq-box))
      (update-seq-box! seq-box seq (cons entry seq))])])

;;
;; ddict-set*
;;
(define/dd-match (ddict-set* dd . args)
  [(iddict elems del seq) (build-immutable-ddict 'ddict-set* elems del seq args)])

;;
;; ddict-set*!
;;
(define/dd-match (ddict-set*! dd . args)
  [(mddict elems _ seq-box)
   (add-to-mutable-dict! 'ddict-set*! elems seq-box args)])


;;
;; ddict-compact?
;;
(define/dd-match (ddict-compact? dd)
  [(iddict _ del _) (eqv? 0 del)]
  [(mddict _ del _) (eqv? 0 del)])

;;
;; ddict-compact!
;;
(define/dd-match (ddict-compact! dd)
  [(iddict elems del seq)
   (set-del! dd 0)
   (set-seq! dd (filter-iddict-seq elems seq))]
  [(mddict elems del seq-box)
   (define seq (unbox seq-box))
   (set-del! dd 0)
   (update-seq-box! seq-box seq (filter-mddict-seq elems seq))])


(define-syntax-rule (too-fragmented? elems del)
  (>= del (hash-count elems)))

;;
;; ddict-remove
;;
(define/dd-match (ddict-remove dd key)
  [(iddict elems del seq)
   (let ([elems (hash-remove elems key)])
     (if (too-fragmented? elems del)
         (immutable-ddict elems 0 (filter-iddict-seq elems seq))
         (immutable-ddict elems (add1 del) seq)))])

(define-syntax-rule (filter-iddict-seq elems seq)
  (for/list ([keyb (in-list seq)]
             #:when (hash-has-key? elems (weak-box-value keyb *missing*)))
    keyb))

;;
;; ddict-remove!
;;
(define/dd-match (ddict-remove! dd key)
  [(mddict elems del seq-box)
   (define entry (hash-ref elems key *missing*))
   (when (mpair? entry)
     (set-mcar! entry *missing*)
     (set-mcdr! entry *missing*)
     (hash-remove! elems key)
     (set-del! dd (add1 (del-of dd)))
     (when (too-fragmented? elems del)
       (define seq (unbox seq-box))
       (set-del! dd 0)
       (update-seq-box! seq-box seq (filter-mddict-seq elems seq))))])


(define-syntax-rule (filter-mddict-seq elems seq)
  (for*/list ([entry (in-list seq)]
              [key (in-value (mcar entry))]
              #:unless (eq? *missing* key))
    entry))


;;
;; ddict-clear!
;;
(define/dd-match (ddict-clear! dd)
  [(mddict elems _ seq-box)
   (hash-clear! elems)
   (set-box! seq-box '())])

;;
;; ddict-clear
;;
(define/dd-match (ddict-clear dd)
  [(iddict elems _ _)
   (immutable-ddict (hash-clear elems) 0 '())])

;;;
;;; ddict-copy-clear
;;;
(define/dd-match (ddict-copy-clear dd)
  [(iddict elems _ _) (immutable-ddict (hash-copy-clear elems) 0 '())]
  [(mddict elems _ _) (mutable-ddict (hash-copy-clear elems) 0 (box '()))])

;;
;; ddict-copy
;;
(define/dd-match (ddict-copy dd)
  [(mddict elems del seq)
   (let ([elems (hash-copy elems)])
     (mutable-ddict elems del seq))])

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
;; ddict-keys
;;
(define/dd-match (ddict-keys dd)
  [(iddict elems del seq)
   (if (eqv? 0 del)
       (map weak-box-value seq)
       (for*/list ([keyb (in-list seq)]
                   [key (in-value (weak-box-value keyb))]
                   #:when (hash-has-key? elems key))
         key))]
  [(mddict elems _ seq-box)
   (for*/list ([entry (in-list (unbox seq-box))]
               [key (in-value (mcar entry))]
               #:unless (eq? key *missing*))
     key)])

;; 
;; ddict-values
;; 
(define/dd-match (ddict-values dd)
  [(iddict elems _ seq)
   (for*/list ([keyb (in-list seq)]
               [val (in-value (hash-ref elems (weak-box-value keyb *missing*) *missing*))]
               #:unless (eq? val *missing*))
     val)]
  [(mddict elems _ seq-box)
   (for*/list ([entry (in-list (unbox seq-box))]
               [val (in-value (mcdr entry))]
               #:unless (eq? val *missing*))
     val)])

;;
;; ddict->list
;;
(define/dd-match (ddict->list dd)
  [(iddict elems _ seq)
   (for*/list ([keyb (in-list seq)]
               [key (in-value (weak-box-value keyb *missing*))]
               [val (in-value (hash-ref elems key *missing*))]
               #:unless (or (eq? *missing* key) (eq? *missing* val)))
     (cons key val))]
  [(mddict elems _ seq-box)
   (for*/list ([entry (in-list (unbox seq-box))]
               [key (in-value (mcar entry))]
               [val (in-value (mcdr entry))]
               #:unless (or (eq? *missing* key) (eq? *missing* val)))
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
               [key (in-value (weak-box-value keyb *missing*))]
               [val (in-value (hash-ref elems key *missing*))]
               #:unless (or (eq? *missing* key) (eq? *missing* val)))
     (f key val))]
  [(mddict elems _ seq-box)
   (unless (and (procedure? f)
                (procedure-arity-includes? f 2))
     (raise-argument-error 'ddict-map "(any/c any/c . -> . any/c)" f))
   (for*/list ([entry (in-list (unbox seq-box))]
               [key (in-value (mcar entry))]
               [val (in-value (mcdr entry))]
               #:unless (or (eq? *missing* key) (eq? *missing* val)))
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
          [key (in-value (weak-box-value keyb *missing*))]
          [val (in-value (hash-ref elems key *missing*))]
          #:unless (or (eq? *missing* key) (eq? *missing* val)))
     (f key val))]
  [(mddict elems _ seq-box)
   (unless (and (procedure? f)
                (procedure-arity-includes? f 2))
     (raise-argument-error 'ddict-for-each "(any/c any/c . -> . any/c)" f))
   (for* ([entry (in-list (unbox seq-box))]
          [key (in-value (mcar entry))]
          [val (in-value (mcdr entry))]
          #:unless (or (eq? *missing* key) (eq? *missing* val)))
     (f key val))])


;;
;; immutable-next-key/val
;;
(define-syntax-rule (immutable-next-key/val elems seq)
  (cond
    [(pair? seq)
     (define key (weak-box-value (car seq) *missing*))
     (define val (hash-ref elems key *missing*))
     (if (eq? val *missing*)
         (immutable-next-key/val-proc elems (cdr seq))
         (values key val (cdr seq)))]
    [else (values #f #f #f)]))

(define (immutable-next-key/val-proc elems seq)
  (cond
    [(pair? seq)
     (define key (weak-box-value (car seq) *missing*))
     (define val (hash-ref elems key *missing*))
     (if (eq? val *missing*)
         (immutable-next-key/val-proc elems (cdr seq))
         (values key val (cdr seq)))]
    [else (values #f #f #f)]))

;;
;; mutable-next-key/val
;;
(define-syntax-rule (mutable-next-key/val elems seq)
  (cond
    [(pair? seq)
     (define entry (car seq))
     (define key (mcar entry))
     (define val (mcdr entry))
     (if (or (eq? key *missing*)
             (eq? val *missing*))
         (mutable-next-key/val-proc elems (cdr seq))
         (values key val (cdr seq)))]
    [else (values #f #f #f)]))

(define (mutable-next-key/val-proc elems seq)
  (cond
    [(pair? seq)
     (define entry (car seq))
     (define key (mcar entry))
     (define val (mcdr entry))
     (if (or (eq? key *missing*)
             (eq? val *missing*))
         (mutable-next-key/val-proc elems (cdr seq))
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
           ([(elems seq next)
             (let ([dd dd-exp])
               (unless (ddict? dd)
                 (raise-argument-error 'in-ddict "ddict?" dd))
               (let ([elems (elems-of dd)])
                 (cond
                   [(immutable? elems) (values elems (seq-of dd) immutable-next-key/val-proc)]
                   [else (values elems (unbox (seq-of dd)) mutable-next-key/val-proc)])))])
           ;; outer-check
           #t
           ;; ([loop-id loop-expr] ...)
           ([pos seq])
           ;; pos-guard
           #t
           ;; ([(inner-id ...) inner-expr] ...)
           ([(key val rst) (next elems pos)])
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

;;
;; in-immutable-ddict
;;
(define-sequence-syntax in-immutable-ddict
  (λ () #'(in-ddict-proc 'in-immutable-ddict immutable-ddict? "immutable-ddict?"))
  (λ (stx)
    (syntax-case stx ()
      [[(key val) (_ dd-exp)]
       #'[(key val)
          (:do-in
           ;; ([(outer-id ...) outer-expr] ...)
           ([(elems seq)
             (let ([dd dd-exp])
               (unless (immutable-ddict? dd)
                 (raise-argument-error 'in-immutable-ddict "immutable-ddict?" dd))
               (values (elems-of dd) (seq-of dd)))])
           ;; outer-check
           #t
           ;; ([loop-id loop-expr] ...)
           ([pos seq])
           ;; pos-guard
           #t
           ;; ([(inner-id ...) inner-expr] ...)
           ([(key val rst) (immutable-next-key/val elems pos)])
           ;; pre-guard
           rst
           ;; post-guard
           #t
           ;; (loop-arg ...)
           (rst))]]
      [[xs (_ dd-exp)]
       (list (syntax->datum #'xs))
       (raise-syntax-error 'in-immutable-ddict
                           (format "expected an identifier list of length 2, given ~a"
                                   (length (syntax->list #'xs)))
                           #'xs)]
      [blah (raise-syntax-error 'in-immutable-ddict "invalid usage" #'blah)])))

;;
;; in-mutable-ddict
;;
(define-sequence-syntax in-mutable-ddict
  (λ () #'(in-ddict-proc 'in-mutable-ddict mutable-ddict? "mutable-ddict?"))
  (λ (stx)
    (syntax-case stx ()
      [[(key val) (_ dd-exp)]
       #'[(key val)
          (:do-in
           ;; ([(outer-id ...) outer-expr] ...)
           ([(elems seq)
             (let ([dd dd-exp])
               (unless (mutable-ddict? dd)
                 (raise-argument-error 'in-mutable-ddict "mutable-ddict?" dd))
               (values (elems-of dd) (unbox (seq-of dd))))])
           ;; outer-check
           #t
           ;; ([loop-id loop-expr] ...)
           ([pos seq])
           ;; pos-guard
           #t
           ;; ([(inner-id ...) inner-expr] ...)
           ([(key val rst) (mutable-next-key/val elems pos)])
           ;; pre-guard
           rst
           ;; post-guard
           #t
           ;; (loop-arg ...)
           (rst))]]
      [[xs (_ dd-exp)]
       (list? (syntax->datum #'xs))
       (raise-syntax-error 'in-mutable-ddict
                           (format "expected an identifier list of length 2, given ~a"
                                   (length (syntax->list #'xs)))
                           #'xs)]
      [blah (raise-syntax-error 'in-mutable-ddict "invalid usage" #'blah)])))

(define (immutable-next-key-proc elems seq)
  (cond
    [(pair? seq)
     (define key (weak-box-value (car seq) *missing*))
     (if (hash-has-key? elems key)
         (values key (cdr seq))
         (immutable-next-key-proc elems (cdr seq)))]
    [else (values #f #f)]))

(define (immutable-next-key/no-del-proc elems pos)
  (if (pair? pos)
      (values (weak-box-value (car pos) *missing*)
              (cdr pos))
      (values #f #f)))


;;
;; mutable-next-key
;;
(define-syntax-rule (mutable-next-key elems seq)
  (cond
    [(pair? seq)
     (define entry (car seq))
     (define key (mcar entry))
     (if (eq? key *missing*)
         (mutable-next-key-proc elems (cdr seq))
         (values key (cdr seq)))]
    [else (values #f #f)]))

(define (mutable-next-key-proc elems seq)
  (cond
    [(pair? seq)
     (define entry (car seq))
     (define key (mcar entry))
     (if (eq? key *missing*)
         (mutable-next-key-proc elems (cdr seq))
         (values key (cdr seq)))]
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
           ([(elems seq next)
             (let ([dd dd-exp])
               (unless (ddict? dd)
                 (raise-argument-error 'in-ddict-keys "ddict?" dd))
               (let ([elems (elems-of dd)])
                 (cond
                   [(immutable? elems) (values elems (seq-of dd) immutable-next-key-proc)]
                   [else (values elems (unbox (seq-of dd)) mutable-next-key-proc)])))])
           ;; outer-check
           #t
           ;; ([loop-id loop-expr] ...)
           ([pos seq])
           ;; pos-guard
           #t
           ;; ([(inner-id ...) inner-expr] ...)
           ([(key rst) (next elems pos)])
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
;; in-immutable-ddict-keys
;;
(define-sequence-syntax in-immutable-ddict-keys
  (λ () #'(in-ddict-keys-proc 'in-immutable-ddict-keys
                              immutable-ddict?
                              "immutable-ddict?"))
  (λ (stx)
    (syntax-case stx ()
      [[(key) (_ dd-exp)]
       #'[(key)
          (:do-in
           ;; ([(outer-id ...) outer-expr] ...)
           ([(elems seq next)
             (let ([dd dd-exp])
               (unless (immutable-ddict? dd)
                 (raise-argument-error 'in-immutable-ddict-keys "immutable-ddict?" dd))
               (if (eqv? 0 (del-of dd))
                   (values (elems-of dd) (seq-of dd) immutable-next-key/no-del-proc)
                   (values (elems-of dd) (seq-of dd) immutable-next-key-proc)))])
           ;; outer-check
           #t
           ;; ([loop-id loop-expr] ...)
           ([pos seq])
           ;; pos-guard
           #t
           ;; ([(inner-id ...) inner-expr] ...)
           ([(key rst) (next elems pos)])
           ;; pre-guard
           rst
           ;; post-guard
           #t
           ;; (loop-arg ...)
           (rst))]]
      [[xs (_ dd-exp)]
       (list? (syntax->datum #'xs))
       (raise-syntax-error 'in-immutable-ddict-keys
                           (format "expected a single identifier, given ~a"
                                   (length (syntax->list #'xs)))
                           #'xs)]
      [blah (raise-syntax-error 'in-immutable-ddict-keys "invalid usage" #'blah)])))

;;
;; in-mutable-ddict-keys
;;
(define-sequence-syntax in-mutable-ddict-keys
  (λ () #'(in-ddict-keys-proc 'in-mutable-ddict-keys
                              mutable-ddict?
                              "mutable-ddict?"))
  (λ (stx)
    (syntax-case stx ()
      [[(key) (_ dd-exp)]
       #'[(key)
          (:do-in
           ;; ([(outer-id ...) outer-expr] ...)
           ([(elems seq)
             (let ([dd dd-exp])
               (unless (mutable-ddict? dd)
                 (raise-argument-error 'in-mutable-ddict-keys "mutable-ddict?" dd))
               (values (elems-of dd) (unbox (seq-of dd))))])
           ;; outer-check
           #t
           ;; ([loop-id loop-expr] ...)
           ([pos seq])
           ;; pos-guard
           #t
           ;; ([(inner-id ...) inner-expr] ...)
           ([(key rst) (mutable-next-key elems pos)])
           ;; pre-guard
           rst
           ;; post-guard
           #t
           ;; (loop-arg ...)
           (rst))]]
      [[xs (_ dd-exp)]
       (list? (syntax->datum #'xs))
       (raise-syntax-error 'in-mutable-ddict-keys
                           (format "expected a single identifier, given ~a"
                                   (length (syntax->list #'xs)))
                           #'xs)]
      [blah (raise-syntax-error 'in-mutable-ddict-keys "invalid usage" #'blah)])))


;;
;; immutable-next-val
;;
(define-syntax-rule (immutable-next-val elems seq)
  (cond
    [(pair? seq)
     (define val (hash-ref elems (weak-box-value (car seq) *missing*) *missing*))
     (if (eq? val *missing*)
         (immutable-next-val-proc elems (cdr seq))
         (values val (cdr seq)))]
    [else (values #f #f)]))

(define (immutable-next-val-proc elems seq)
  (cond
    [(pair? seq)
     (define val (hash-ref elems (weak-box-value (car seq) *missing*) *missing*))
     (if (eq? val *missing*)
         (immutable-next-val-proc elems (cdr seq))
         (values val (cdr seq)))]
    [else (values #f #f)]))

;;
;; mutable-next-val
;;
(define-syntax-rule (mutable-next-val elems seq)
  (cond
    [(pair? seq)
     (define val (mcdr (car seq)))
     (if (eq? val *missing*)
         (mutable-next-val-proc elems (cdr seq))
         (values val (cdr seq)))]
    [else (values #f #f)]))

(define (mutable-next-val-proc elems seq)
  (cond
    [(pair? seq)
     (define val (mcdr (car seq)))
     (if (eq? val *missing*)
         (mutable-next-val-proc elems (cdr seq))
         (values val (cdr seq)))]
    [else (values #f #f)]))

(define ((in-ddict-values-proc name pred? pred-str) dd)
  (cond
    [(pred? dd) (ddict-values dd)]
    [else (raise-argument-error name pred-str dd)]))

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
           ([(elems seq next)
             (let ([dd dd-exp])
               (unless (ddict? dd)
                 (raise-argument-error 'in-ddict-values "ddict?" dd))
               (let ([elems (elems-of dd)])
                 (cond
                   [(immutable? elems) (values elems (seq-of dd) immutable-next-val-proc)]
                   [else (values elems (unbox (seq-of dd)) mutable-next-val-proc)])))])
           ;; outer-check
           #t
           ;; ([loop-id loop-expr] ...)
           ([pos seq])
           ;; pos-guard
           #t
           ;; ([(inner-id ...) inner-expr] ...)
           ([(val rst) (next elems pos)])
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

;;
;; in-immutable-ddict-values
;;
(define-sequence-syntax in-immutable-ddict-values
  (λ () #'(in-ddict-values-proc 'in-immutable-ddict-values
                                immutable-ddict?
                                "immutable-ddict?"))
  (λ (stx)
    (syntax-case stx ()
      [[(val) (_ dd-exp)]
       #'[(val)
          (:do-in
           ;; ([(outer-id ...) outer-expr] ...)
           ([(elems seq)
             (let ([dd dd-exp])
               (unless (immutable-ddict? dd)
                 (raise-argument-error 'in-immutable-ddict-values "immutable-ddict?" dd))
               (values (elems-of dd) (seq-of dd)))])
           ;; outer-check
           #t
           ;; ([loop-id loop-expr] ...)
           ([pos seq])
           ;; pos-guard
           #t
           ;; ([(inner-id ...) inner-expr] ...)
           ([(val rst) (immutable-next-val elems pos)])
           ;; pre-guard
           rst
           ;; post-guard
           #t
           ;; (loop-arg ...)
           (rst))]]
      [[xs (_ dd-exp)]
       (list? (syntax->datum #'xs))
       (raise-syntax-error 'in-immutable-ddict-values
                           (format "expected a single identifier, given ~a"
                                   (length (syntax->list #'xs)))
                           #'xs)]
      [blah (raise-syntax-error 'in-immutable-ddict-values "invalid usage" #'blah)])))

;;
;; in-mutable-ddict-values
;;
(define-sequence-syntax in-mutable-ddict-values
  (λ () #'(in-ddict-values-proc 'in-mutable-ddict-values
                                mutable-ddict?
                                "mutable-ddict?"))
  (λ (stx)
    (syntax-case stx ()
      [[(val) (_ dd-exp)]
       #'[(val)
          (:do-in
           ;; ([(outer-id ...) outer-expr] ...)
           ([(elems seq)
             (let ([dd dd-exp])
               (unless (mutable-ddict? dd)
                 (raise-argument-error 'in-mutable-ddict-values "mutable-ddict?" dd))
               (values (elems-of dd) (unbox (seq-of dd))))])
           ;; outer-check
           #t
           ;; ([loop-id loop-expr] ...)
           ([pos seq])
           ;; pos-guard
           #t
           ;; ([(inner-id ...) inner-expr] ...)
           ([(val rst) (mutable-next-val elems pos)])
           ;; pre-guard
           rst
           ;; post-guard
           #t
           ;; (loop-arg ...)
           (rst))]]
      [[xs (_ dd-exp)]
       (list? (syntax->datum #'xs))
       (raise-syntax-error 'in-mutable-ddict-values
                           (format "expected a single identifier, given ~a"
                                   (length (syntax->list #'xs)))
                           #'xs)]
      [blah (raise-syntax-error 'in-mutable-ddict-values "invalid usage" #'blah)])))



(define-syntax-rule (define-for-immutable-ddict for-name for/derived empty-hash)
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
                   (let*-values ([(key val) (let () . defs+exprs)]
                                 [(elems seq) (unsafe-iddict-set elems seq key val)])
                     (values elems seq)))])
             (immutable-ddict elems 0 seq))))])))



(define-for-immutable-ddict for/ddict     for/fold/derived  #hash())
(define-for-immutable-ddict for/ddicteqv  for/fold/derived  #hasheqv())
(define-for-immutable-ddict for/ddicteq   for/fold/derived  #hasheq())
(define-for-immutable-ddict for*/ddict    for*/fold/derived #hash())
(define-for-immutable-ddict for*/ddicteqv for*/fold/derived #hasheqv())
(define-for-immutable-ddict for*/ddicteq  for*/fold/derived #hasheq())


(define-syntax-rule (define-for-mutable-ddict for-name for/derived make-empty-hash)
  (define-syntax (for-name stx)
    (syntax-case stx ()
      [(_ clauses . defs+exprs)
       (with-syntax ([original stx])
         (syntax/loc stx
           (let*-values
               ([(elems seq-box) (values (make-empty-hash) (box '()))]
                [(_)
                 (for/derived original
                   ([_ (void)])
                   clauses
                   (let-values ([(key val) (let () . defs+exprs)])
                     (unsafe-mddict-set! elems seq-box key val)))])
             (mutable-ddict elems 0 seq-box))))])))


(define-for-mutable-ddict for/mutable-ddict     for/fold/derived  make-hash)
(define-for-mutable-ddict for/mutable-ddicteqv  for/fold/derived  make-hasheqv)
(define-for-mutable-ddict for/mutable-ddicteq   for/fold/derived  make-hasheq)
(define-for-mutable-ddict for*/mutable-ddict    for*/fold/derived make-hash)
(define-for-mutable-ddict for*/mutable-ddicteqv for*/fold/derived make-hasheqv)
(define-for-mutable-ddict for*/mutable-ddicteq  for*/fold/derived make-hasheq)
