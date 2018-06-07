#lang racket/base

(require racket/dict
         (only-in racket/unsafe/ops
                  unsafe-struct*-ref
                  unsafe-struct*-set!
                  unsafe-unbox*
                  unsafe-box*-cas!
                  unsafe-set-box*!
                  unsafe-vector*-ref)
         (for-syntax racket/base)
         syntax/id-table
         syntax/id-set)

(provide free-id-ddict?
         immutable-free-id-ddict?
         mutable-free-id-ddict?
         (rename-out [free-id-ddict* free-id-ddict]
                     [mutable-free-id-ddict* mutable-free-id-ddict])
         make-free-id-ddict
         make-mutable-free-id-ddict
         free-id-ddict-set
         free-id-ddict-set*
         free-id-ddict-set!
         free-id-ddict-set*!
         free-id-ddict-ref
         free-id-ddict-ref!
         free-id-ddict-update
         free-id-ddict-update!
         free-id-ddict-remove
         free-id-ddict-remove!
         free-id-ddict-clear!
         free-id-ddict-clear
         free-id-ddict-copy
         free-id-ddict-copy-clear
         free-id-ddict-has-key?
         free-id-ddict-empty?
         free-id-ddict-count
         free-id-ddict-keys
         free-id-ddict-values
         free-id-ddict->list
         free-id-ddict-map
         free-id-ddict-for-each
         free-id-ddict-keys-subset?
         free-id-ddict-iterate-first
         free-id-ddict-iterate-next
         free-id-ddict-iterate-key
         free-id-ddict-iterate-value
         free-id-ddict-position?
         in-free-id-ddict
         in-free-id-ddict-keys
         in-free-id-ddict-values
         for/free-id-ddict
         for*/free-id-ddict
         for/mutable-free-id-ddict
         for*/mutable-free-id-ddict
         free-id-ddict-compact?
         free-id-ddict-compact
         free-id-ddict-compact!)


(define-syntax-rule (no-key-err-thunk fun-name key)
  (λ ()
    (raise
     (make-exn:fail:contract
      (format "~a: no value found for key\n key: ~a" (quote fun-name) key)
      (current-continuation-marks)))))


(define *missing* (datum->syntax #f (gensym 'missing)))

;; standard "default value call if val is thunk else return val" behavior
(define-syntax-rule (default val)
  (let ([d val]) (if (procedure? d) (d) d)))

(define-syntax-rule (unbox-key keybox)
  (weak-box-value keybox *missing*))

;; updates an immutable free-id-table table and, if it's a new key,
;; that key is added to seq
(define (update-elems+seq elems seq key val)
  (let ([prev-count (free-id-table-count elems)]
        [elems (free-id-table-set elems key val)])
    (values elems (if (eqv? prev-count (free-id-table-count elems))
                      seq
                      (cons (make-weak-box key) seq)))))


(define (free-id-ddict? x)
  (or (immutable-free-id-ddict? x) (mutable-free-id-ddict? x)))

;; constructor template for immutable free-id-ddicts
(define-syntax-rule (immutable-free-id-ddict-constructor name empty init-free-id-table)
  (case-lambda
    [() empty]
    [args (build-immutable-free-id-ddict (quote name) init-free-id-table 0 '() args)]))

(define (build-immutable-free-id-ddict name init-free-id-table del init-seq initial-args)
  (let loop ([args initial-args]
             [elems init-free-id-table]
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
      [(null? args) (ifree-id-ddict elems del seq)]
      [else (error name "impossible! you found a bug!")])))

;; TODO There should be a way to specify phase level
(define free-id-ddict*
  (immutable-free-id-ddict-constructor free-id-ddict
                                       empty-free-id-ddict
                                       (make-immutable-free-id-table)))

;; constructor template for mutable free-id-ddicts
(define-syntax-rule (mutable-free-id-ddict-constructor name init-hash)
  (case-lambda
    [() (mfree-id-ddict init-hash 0 '())]
    [initial-args
     (let loop ([args initial-args]
                [elems init-hash]
                [seq '()])
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
              (quote name)
              "an even number of arguments"
              initial-args)])]
         [(null? args) (mfree-id-ddict elems 0 seq)]
         [else (error (quote name) "impossible! you found a bug!")]))]))

(define mutable-free-id-ddict*
  (mutable-free-id-ddict-constructor mutable-free-id-ddict
                                     (make-immutable-free-id-table)))


;; "make-" constructor template for immutable free-id-ddicts
(define-syntax-rule (make-free-id-ddict/template name init-hash)
  (λ ([initial-alist '()])
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
        [(null? alist) (ifree-id-ddict elems 0 seq)]
        [else (raise-argument-error (quote name)
                                    "(listof pair?)"
                                    initial-alist)]))))


(define make-free-id-ddict
  (make-free-id-ddict/template make-free-id-ddict
                               (make-immutable-free-id-table)))


;; "make-" constructor template for mutable free-id-ddicts
(define-syntax-rule (make-mutable-free-id-ddict/template name init-hash)
  (λ ([initial-alist '()])
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
        [(null? alist) (mfree-id-ddict elems 0 seq)]
        [else (raise-argument-error (quote name)
                                    "(listof pair?)"
                                    initial-alist)]))))

(define make-mutable-free-id-ddict
  (make-mutable-free-id-ddict/template make-free-id-ddict
                                       (make-immutable-free-id-table)))

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

;; macro for defining functions whose first argument is a free-id-ddict
;; This automatically inserts a free-id-ddict? check 
;; and raise-argument-error for failure, as well as pattern matching
;; out the free-id-ddict's fields quickly after the check is complete
(define-syntax (define/dd-match stx)
  (syntax-case stx (ifree-id-ddict mfree-id-ddict)
    ;; free-id-ddict function for both mutable and immutable
    [(_ (name dd . other-args)
        [(ifree-id-ddict i-elems i-n i-seq) . i-body]
        [(mfree-id-ddict m-elems m-n m-seq) . m-body])
     (and (identifier? #'i-elems) (identifier? #'i-n) (identifier? #'i-seq)
          (identifier? #'m-elems) (identifier? #'m-n) (identifier? #'m-seq))
     (with-syntax ([i-bindings (parse-i-bindings #'dd #'i-elems #'i-n #'i-seq)]
                   [m-bindings (parse-m-bindings #'dd #'m-elems #'m-n #'m-seq)])
       (quasisyntax/loc stx
         (define (name dd . other-args)
           (cond
             [(immutable-free-id-ddict? dd) #,(syntax/loc #'i-body (let i-bindings . i-body))]
             [(mutable-free-id-ddict? dd) #,(syntax/loc #'m-body (let* m-bindings . m-body))]
             [else (raise-argument-error (quote name) "free-id-ddict?" dd)]))))]
    ;; only immutable free-id-ddict function
    [(_ (name dd . other-args)
        [(ifree-id-ddict elems n seq) . body])
     (and (identifier? #'elems) (identifier? #'n) (identifier? #'seq))
     (with-syntax ([bindings (parse-i-bindings #'dd #'elems #'n #'seq)])
       (quasisyntax/loc stx
         (define (name dd . other-args)
           (cond
             [(immutable-free-id-ddict? dd) #,(syntax/loc #'body (let bindings . body))]
             [else (raise-argument-error (quote name) "immutable-free-id-ddict?" dd)]))))]
    ;; only mutable free-id-ddict function
    [(_ (name dd . other-args)
        [(mfree-id-ddict elems n seq) . body])
     (and (identifier? #'elems) (identifier? #'n) (identifier? #'seq))
     (with-syntax ([bindings (parse-m-bindings #'dd #'elems #'n #'seq)])
       (quasisyntax/loc stx
         (define (name dd . other-args)
           (cond
             [(mutable-free-id-ddict? dd) #,(syntax/loc #'body (let* bindings . body))]
             [else (raise-argument-error (quote name) "mutable-free-id-ddict?" dd)]))))]))

;;
;; free-id-ddict-set
;;
(define/dd-match (free-id-ddict-set dd key val)
  [(ifree-id-ddict elems del seq)
   (let-values ([(elems seq) (update-elems+seq elems seq key val)])
     (ifree-id-ddict elems del seq))])

;;
;; free-id-ddict-set!
;;
(define/dd-match (free-id-ddict-set! mdd key val)
  [(mfree-id-ddict elems del seq)
   (let-values ([(elems seq) (update-elems+seq elems seq key val)])
     (unless (try-update-mfree-id-ddict-content! mdd elems del seq)
       (free-id-ddict-set! mdd key val)))])

;;
;; free-id-ddict-ref
;;
(define/dd-match (free-id-ddict-ref
                  dd
                  key
                  [failure (no-key-err-thunk free-id-ddict-ref key)])
  [(ifree-id-ddict elems _ _) (free-id-table-ref elems key failure)]
  [(mfree-id-ddict elems _ _) (free-id-table-ref elems key failure)])

;;
;; free-id-ddict-ref!
;;
(define/dd-match (free-id-ddict-ref! mdd key to-set)
  [(mfree-id-ddict _ _ _)
   (define val (free-id-ddict-ref mdd key *missing*))
   (cond
     [(eq? val *missing*)
      (let ([val (default to-set)])
        (free-id-ddict-set! mdd key val)
        val)]
     [else val])])

;;;
;;; free-id-ddict-update
;;;
(define/dd-match (free-id-ddict-update
                  dd
                  key
                  updater
                  [failure (no-key-err-thunk free-id-ddict-update key)])
  [(ifree-id-ddict elems del seq)
   (unless (and (procedure? updater)
                (procedure-arity-includes? updater 1))
     (raise-argument-error 'free-id-ddict-update "(any/c . -> . any/c)" updater))
   (define prev-val (free-id-table-ref elems key failure))
   (let-values ([(elems seq) (update-elems+seq elems seq key (updater prev-val))])
     (ifree-id-ddict elems del seq))])

;;
;; free-id-ddict-update!
;;
(define/dd-match (free-id-ddict-update!
                  mdd
                  key
                  updater
                  [failure (no-key-err-thunk free-id-ddict-update! key)])
  [(mfree-id-ddict _ _ _)
   (unless (and (procedure? updater)
                (procedure-arity-includes? updater 1))
     (raise-argument-error 'free-id-ddict-update! "(any/c . -> . any/c)" updater))
   (define val (free-id-ddict-ref mdd key failure))
   (free-id-ddict-set! mdd key (updater val))])

;;
;; free-id-ddict-set*
;;
(define/dd-match (free-id-ddict-set* dd . args)
  [(ifree-id-ddict elems del seq)
   (build-immutable-free-id-ddict 'free-id-ddict-set* elems del seq args)])

;;
;; free-id-ddict-set*!
;;
(define/dd-match (free-id-ddict-set*! mdd . initial-args)
  [(mfree-id-ddict _ _ _)
   (let loop ([args initial-args])
     (cond
       [(pair? args)
        (define key (car args))
        (define rst (cdr args))
        (cond
          [(pair? rst)
           (define val (car rst))
           (free-id-ddict-set! mdd key val)
           (loop (cdr rst))]
          [else
           (raise-argument-error
            'free-id-ddict-set*!
            "an even number of arguments"
            initial-args)])]
       [(null? args) (void)]
       [else (raise-argument-error
              'free-id-ddict-set*!
              "a list of keys and values"
              initial-args)]))])

(define-syntax-rule (too-fragmented? elems del)
  (>= del (free-id-table-count elems)))

;;
;; free-id-ddict-remove
;;
(define/dd-match (free-id-ddict-remove dd key)
  [(ifree-id-ddict elems del seq)
   (let ([elems (free-id-table-remove elems key)])
     (if (too-fragmented? elems del)
         (ifree-id-ddict elems 0 (free-id-filter-seq elems seq))
         (ifree-id-ddict elems (add1 del) seq)))])

(define (free-id-table-has-key? id-table key)
  (not (eq? *missing* (free-id-table-ref id-table key *missing*))))

(define-syntax-rule (free-id-filter-seq elems seq)
  (for*/list ([keyb (in-list seq)]
              [key  (in-value (unbox-key keyb))]
              #:when (free-id-table-has-key? elems key))
    keyb))

;;
;; free-id-ddict-remove!
;;
(define/dd-match (free-id-ddict-remove! mdd key)
  [(mfree-id-ddict elems del seq)
   (let ([elems (free-id-table-remove elems key)])
     (cond
       [(too-fragmented? elems del)
        (let ([seq (free-id-filter-seq elems seq)])
          (unless (try-update-mfree-id-ddict-content! mdd elems 0 seq)
            (free-id-ddict-remove! mdd key)))]
       [else
        (unless (try-update-mfree-id-ddict-content! mdd elems (add1 del) seq)
          (free-id-ddict-remove! mdd key))]))])


;;
;; free-id-ddict-clear!
;;
(define/dd-match (free-id-ddict-clear! mdd)
  [(mfree-id-ddict elems _ _)
   (force-update-mfree-id-ddict-content! mdd
                                         (make-immutable-free-id-table)
                                         0
                                         '())])

;;
;; free-id-ddict-clear
;;
(define/dd-match (free-id-ddict-clear dd)
  [(ifree-id-ddict elems _ _)
   (ifree-id-ddict (make-free-id-table) 0 '())])

;;;
;;; free-id-ddict-copy-clear
;;;
(define/dd-match (free-id-ddict-copy-clear dd)
  [(ifree-id-ddict elems _ _) (ifree-id-ddict (make-free-id-table) 0 '())]
  [(mfree-id-ddict elems _ _) (mfree-id-ddict (make-free-id-table) 0 '())])

;;
;; free-id-ddict-copy
;;
(define/dd-match (free-id-ddict-copy dd)
  [(mfree-id-ddict elems del seq) (mfree-id-ddict elems del seq)])


;;
;; free-id-ddict-has-key?
;;
(define/dd-match (free-id-ddict-has-key? dd key)
  [(ifree-id-ddict elems _ _) (free-id-table-has-key? elems key)]
  [(mfree-id-ddict elems _ _) (free-id-table-has-key? elems key)])


(define (free-id-table-empty? id-table)
  (= 0 (free-id-table-count id-table)))
;;
;; free-id-ddict-empty?
;;
(define/dd-match (free-id-ddict-empty? dd)
  [(ifree-id-ddict elems _ _) (free-id-table-empty? elems)]
  [(mfree-id-ddict elems _ _) (free-id-table-empty? elems)])

;;
;; free-id-ddict-count
;;
(define/dd-match (free-id-ddict-count dd)
  [(ifree-id-ddict elems _ _) (free-id-table-count elems)]
  [(mfree-id-ddict elems _ _) (free-id-table-count elems)])

;;
;; free-id-ddict-compact?
;;
(define/dd-match (free-id-ddict-compact? dd)
  [(ifree-id-ddict _ del _) (eqv? 0 del)]
  [(mfree-id-ddict _ del _) (eqv? 0 del)])

;;
;; free-id-ddict-compact
;;
(define/dd-match (free-id-ddict-compact dd)
  [(ifree-id-ddict elems del seq)
   (cond
     [(eqv? 0 del) dd]
     [else (ifree-id-ddict elems 0 (free-id-filter-seq elems seq))])])

;;
;; free-id-ddict-compact!
;;
(define/dd-match (free-id-ddict-compact! mdd)
  [(mfree-id-ddict elems del seq)
   (unless (eqv? 0 del) 
     (let ([seq (free-id-filter-seq elems seq)])
       (unless (try-update-mfree-id-ddict-content! mdd elems 0 seq)
         (free-id-ddict-compact! mdd))))])

(define (free-id-filter-keys elems seq)
  (for*/list ([keyb (in-list seq)]
              [key  (in-value (unbox-key keyb))]
              #:when (free-id-table-has-key? elems key))
    key))

;;
;; free-id-ddict-keys
;;
(define/dd-match (free-id-ddict-keys dd)
  [(ifree-id-ddict elems del seq) (free-id-filter-keys elems seq)]
  [(mfree-id-ddict elems del seq) (free-id-filter-keys elems seq)])

(define (free-id-filter-values elems seq)
  (for*/list ([keyb (in-list seq)]
              [key  (in-value (unbox-key keyb))]
              [value (in-value (free-id-table-ref elems key *missing*))]
              #:unless (eq? *missing* value))
    value))

;; 
;; free-id-ddict-values
;;
(define/dd-match (free-id-ddict-values dd)
  [(ifree-id-ddict elems _ seq) (free-id-filter-values elems seq)]
  [(mfree-id-ddict elems _ seq) (free-id-filter-values elems seq)])

(define (free-id-filter-key-values elems seq)
  (for*/list ([keyb (in-list seq)]
              [key  (in-value (unbox-key keyb))]
              [value (in-value (free-id-table-ref elems key *missing*))]
              #:unless (eq? *missing* value))
    (cons key value)))

;;
;; free-id-ddict->list
;;
(define/dd-match (free-id-ddict->list dd)
  [(ifree-id-ddict elems _ seq) (free-id-filter-key-values elems seq)]
  [(mfree-id-ddict elems _ seq) (free-id-filter-key-values elems seq)])


;;
;; free-id-ddict-map
;;
(define/dd-match (free-id-ddict-map dd f)
  [(ifree-id-ddict elems _ seq)
   (unless (and (procedure? f)
                (procedure-arity-includes? f 2))
     (raise-argument-error 'free-id-ddict-map "(any/c any/c . -> . any/c)" f))
   (for*/list ([keyb (in-list seq)]
               [key (in-value (unbox-key keyb))]
               [val (in-value (free-id-table-ref elems key *missing*))]
               #:unless (eq? *missing* val))
     (f key val))]
  [(mfree-id-ddict _ _ seq)
   (unless (and (procedure? f)
                (procedure-arity-includes? f 2))
     (raise-argument-error 'free-id-ddict-map "(any/c any/c . -> . any/c)" f))
   (for*/list ([keyb (in-list seq)]
               [key (in-value (unbox-key keyb))]
               [elems (in-value (unsafe-mutable-free-id-ddict-elems dd))]
               [val (in-value (free-id-table-ref elems key *missing*))]
               #:unless (eq? *missing* val))
    (f key val))])

;;
;; free-id-ddict-for-each
;;
(define/dd-match (free-id-ddict-for-each dd f)
  [(ifree-id-ddict elems _ seq)
   (unless (and (procedure? f)
                (procedure-arity-includes? f 2))
     (raise-argument-error 'free-id-ddict-for-each "(any/c any/c . -> . any/c)" f))
   (for* ([keyb (in-list seq)]
          [key (in-value (unbox-key keyb))]
          [val (in-value (free-id-table-ref elems key *missing*))]
          #:unless (eq? *missing* val))
     (f key val))]
  [(mfree-id-ddict _ _ seq)
   (unless (and (procedure? f)
                (procedure-arity-includes? f 2))
     (raise-argument-error 'free-id-ddict-for-each "(any/c any/c . -> . any/c)" f))
   (for* ([keyb (in-list seq)]
          [key (in-value (unbox-key keyb))]
          [elems (in-value (unsafe-mutable-free-id-ddict-elems dd))]
          [val (in-value (free-id-table-ref elems key *missing*))]
          #:unless (eq? *missing* val))
     (f key val))])

(define (free-id-table-keys-subset? t1 t2)
  (free-id-subset? (free-id-table-keys t1) (free-id-table-keys t2)))

(define/dd-match (free-id-ddict-keys-subset? dd1 dd2)
  [(ifree-id-ddict elems1 _ _)
   (unless (immutable-free-id-ddict? dd2)
     (raise-argument-error 'free-id-ddict-keys-subset?
                           "immutable-free-id-ddict?"
                           dd2))
   (free-id-table-keys-subset? elems1 (unsafe-immutable-free-id-ddict-elems dd2))]
  [(mfree-id-ddict elems1 _ _)
   (unless (mutable-free-id-ddict? dd2)
     (raise-argument-error 'free-id-ddict-keys-subset?
                           "mutable-free-id-ddict?"
                           dd2))
   (free-id-table-keys-subset? elems1 (unsafe-immutable-free-id-ddict-elems dd2))])

;;
;; next-key/val
;;
(define-syntax-rule (next-key/val dd seq)
  (let ([elems
         (cond
           [(immutable-free-id-ddict? dd)
            (unsafe-immutable-free-id-ddict-elems dd)]
           [(mutable-free-id-ddict? dd)
            (unsafe-mutable-free-id-ddict-elems dd)]
           [else (error 'next-key/val "impossible! you found a bug!")])])
    (cond
      [(pair? seq)
       (define key (unbox-key (car seq)))
       (define val (free-id-table-ref elems key *missing*))
       (if (eq? val *missing*)
           (next-key/val-proc elems (cdr seq))
           (values key val (cdr seq)))]
      [else (values #f #f #f)])))

(define (next-key/val-proc elems seq)
  (cond
    [(pair? seq)
     (define key (unbox-key (car seq)))
     (define val (free-id-table-ref elems key *missing*))
     (if (eq? val *missing*)
         (next-key/val-proc elems (cdr seq))
         (values key val (cdr seq)))]
    [else (values #f #f #f)]))

;;
;; in-free-id-ddict-proc
;;
(define ((in-free-id-ddict-proc name pred? pred-str) dd)
  (cond
    [(pred? dd)
     (define alist (free-id-ddict->list dd))
     (define-values (keys vals)
       (for/lists (ks vs)
         ([p (in-list alist)])
         (values (car p) (cdr p))))
     (in-parallel keys vals)]
    [else (raise-argument-error name pred-str dd)]))

;;
;; in-free-id-ddict
;;
(define-sequence-syntax in-free-id-ddict
  (λ () #'(in-free-id-ddict-proc 'in-free-id-ddict free-id-ddict? "free-id-ddict?"))
  (λ (stx)
    (syntax-case stx ()
      [[(key val) (_ dd-exp)]
       #'[(key val)
          (:do-in
           ;; ([(outer-id ...) outer-expr] ...)
           ([(dd seq)
             (let ([dd dd-exp])
               (cond
                 [(immutable-free-id-ddict? dd)
                  (values dd (unsafe-immutable-free-id-ddict-seq dd))]
                 [(mutable-free-id-ddict? dd)
                  (values dd (unsafe-mutable-free-id-ddict-seq dd))]
                 [else (raise-argument-error 'in-free-id-ddict "free-id-ddict?" dd)]))])
           ;; outer-check
           #t
           ;; ([loop-id loop-expr] ...)
           ([pos seq])
           ;; pos-guard
           #t
           ;; ([(inner-id ...) inner-expr] ...)
           ([(key val rst) (next-key/val dd pos)])
           ;; pre-guard
           rst
           ;; post-guard
           #t
           ;; (loop-arg ...)
           (rst))]]
      [[xs (_ dd-exp)]
       (list? (syntax->datum #'xs))
       (raise-syntax-error 'in-free-id-ddict
                           (format "expected an identifier list of length 2, given ~a"
                                   (length (syntax->list #'xs)))
                           #'xs)]
      [blah (raise-syntax-error 'in-free-id-ddict "invalid usage" #'blah)])))

(define (next-key dd seq)
  (let ([elems
         (cond
           [(immutable-free-id-ddict? dd)
            (unsafe-immutable-free-id-ddict-elems dd)]
           [(mutable-free-id-ddict? dd)
            (unsafe-mutable-free-id-ddict-elems dd)]
           [else (error 'next-key "impossible! you found a bug!")])])
    (cond
      [(pair? seq)
       (define key (unbox-key (car seq)))
       (if (free-id-table-has-key? elems key)
           (values key (cdr seq))
           (next-key-proc elems (cdr seq)))]
      [else (values #f #f)])))

(define (next-key-proc elems seq)
  (cond
    [(pair? seq)
     (define key (unbox-key (car seq)))
     (if (free-id-table-has-key? elems key)
         (values key (cdr seq))
         (next-key-proc elems (cdr seq)))]
    [else (values #f #f)]))




;; in-free-id-ddict-keys-proc
(define ((in-free-id-ddict-keys-proc name pred? pred-str) dd)
  (cond
    [(pred? dd) (free-id-ddict-keys dd)]
    [else (raise-argument-error name pred-str dd)]))


;;
;; in-free-id-ddict-keys
;;
(define-sequence-syntax in-free-id-ddict-keys
  (λ () #'(in-free-id-ddict-keys-proc 'in-free-id-ddict-keys
                                      free-id-ddict?
                                      "free-id-ddict?"))
  (λ (stx)
    (syntax-case stx ()
      [[(key) (_ dd-exp)]
       #'[(key)
          (:do-in
           ;; ([(outer-id ...) outer-expr] ...)
           ([(dd seq) (let ([dd dd-exp])
                        (cond
                          [(immutable-free-id-ddict? dd)
                           (values dd (unsafe-immutable-free-id-ddict-seq dd))]
                          [(mutable-free-id-ddict? dd)
                           (values dd (unsafe-mutable-free-id-ddict-seq dd))]
                          [else (raise-argument-error 'in-free-id-ddict-keys "free-id-ddict?" dd)]))])
           ;; outer-check
           #t
           ;; ([loop-id loop-expr] ...)
           ([pos seq])
           ;; pos-guard
           #t
           ;; ([(inner-id ...) inner-expr] ...)
           ([(key rst) (next-key dd pos)])
           ;; pre-guard
           rst
           ;; post-guard
           #t
           ;; (loop-arg ...)
           (rst))]]
      [[xs (_ dd-exp)]
       (list? (syntax->datum #'xs))
       (raise-syntax-error 'in-free-id-ddict-keys
                           (format "expected a single identifier, given ~a"
                                   (length (syntax->list #'xs)))
                           #'xs)]
      [blah (raise-syntax-error 'in-free-id-ddict-keys "invalid usage" #'blah)])))



;;
;; next-val
;;
(define-syntax-rule (next-val dd seq)
  (let ([elems (cond
                 [(immutable-free-id-ddict? dd) (unsafe-immutable-free-id-ddict-elems dd)]
                 [(mutable-free-id-ddict? dd) (unsafe-mutable-free-id-ddict-elems dd)]
                 [else (error 'next-key "impossible! you found a bug!")])])
    (cond
      [(pair? seq)
       (define val (free-id-table-ref elems (unbox-key (car seq)) *missing*))
       (if (eq? val *missing*)
           (next-val-proc elems (cdr seq))
           (values val (cdr seq)))]
      [else (values #f #f)])))

(define (next-val-proc elems seq)
  (cond
    [(pair? seq)
     (define val (free-id-table-ref elems (unbox-key (car seq)) *missing*))
     (if (eq? val *missing*)
         (next-val-proc elems (cdr seq))
         (values val (cdr seq)))]
    [else (values #f #f)]))

(define ((in-free-id-ddict-values-proc name pred? pred-str) dd)
  (cond
    [(pred? dd) (free-id-ddict-values dd)]
    [else (raise-argument-error name pred-str dd)]))

;;
;; in-free-id-ddict-values
;;
(define-sequence-syntax in-free-id-ddict-values
  (λ () #'(in-free-id-ddict-values-proc 'in-free-id-ddict-values free-id-ddict? "free-id-ddict?"))
  (λ (stx)
    (syntax-case stx ()
      [[(val) (_ dd-exp)]
       #'[(val)
          (:do-in
           ;; ([(outer-id ...) outer-expr] ...)
           ([(dd seq) (let ([dd dd-exp])
                        (cond
                          [(immutable-free-id-ddict? dd)
                           (values dd (unsafe-immutable-free-id-ddict-seq dd))]
                          [(mutable-free-id-ddict? dd)
                           (values dd (unsafe-mutable-free-id-ddict-seq dd))]
                          [else (raise-argument-error 'in-free-id-ddict-values "free-id-ddict?" dd)]))])
           ;; outer-check
           #t
           ;; ([loop-id loop-expr] ...)
           ([pos seq])
           ;; pos-guard
           #t
           ;; ([(inner-id ...) inner-expr] ...)
           ([(val rst) (next-val dd pos)])
           ;; pre-guard
           rst
           ;; post-guard
           #t
           ;; (loop-arg ...)
           (rst))]]
      [[xs (_ dd-exp)]
       (list? (syntax->datum #'xs))
       (raise-syntax-error 'in-free-id-ddict-values
                           (format "expected a single identifier, given ~a"
                                   (length (syntax->list #'xs)))
                           #'xs)]
      [blah (raise-syntax-error 'in-free-id-ddict-values "invalid usage" #'blah)])))

(define-syntax-rule (define-for-free-id-ddict for-name for/derived mk empty-hash)
  (define-syntax (for-name stx)
    (syntax-case stx ()
      [(_ clauses body (... ...) tail-expr)
       (with-syntax ([original stx])
         (syntax/loc stx
           (let-values
               ([(elems seq)
                 (for/derived original
                   ([elems empty-hash]
                    [seq '()])
                   clauses
                   body (... ...)
                   (let-values ([(key val) tail-expr])
                     (unless (identifier? key)
                       (raise-result-error 'for-name "identifier?" 1 key val))
                     (update-elems+seq elems seq key val)))])
             (mk elems 0 seq))))])))



(define-for-free-id-ddict for/free-id-ddict
  for/fold/derived  ifree-id-ddict (make-immutable-free-id-table))
(define-for-free-id-ddict for*/free-id-ddict
  for*/fold/derived ifree-id-ddict (make-immutable-free-id-table))
(define-for-free-id-ddict for/mutable-free-id-ddict
  for/fold/derived  mfree-id-ddict (make-immutable-free-id-table))
(define-for-free-id-ddict for*/mutable-free-id-ddict
  for*/fold/derived mfree-id-ddict (make-immutable-free-id-table))

(struct free-id-ddict-position (dd key val rst))


;;
;; free-id-ddict-iterate-first
;;
(define/dd-match (free-id-ddict-iterate-first dd)
  [(ifree-id-ddict _ _ seq)
   (define-values (key val rst) (next-key/val dd seq))
   (and rst (free-id-ddict-position dd key val rst))]
  [(mfree-id-ddict _ _ seq)
   (define-values (key val rst) (next-key/val dd seq))
   (and rst (free-id-ddict-position dd key val rst))])


;;
;; free-id-ddict-iterate-next
;;
(define (free-id-ddict-iterate-next dd pos)
  (unless (free-id-ddict? dd)
    (raise-argument-error 'free-id-ddict-iterate-next "free-id-ddict?" dd))
  (unless (and (free-id-ddict-position? pos)
               (eq? dd (free-id-ddict-position-dd pos)))
    (raise-argument-error 'free-id-ddict-iterate-next
                          "valid free-id-ddict position for given free-id-ddict"
                          pos))
  (define seq (free-id-ddict-position-rst pos))
  (define-values (key val rst) (next-key/val dd seq))
  (and rst (free-id-ddict-position dd key val rst)))

;;
;; free-id-ddict-iterate-key
;;
(define (free-id-ddict-iterate-key dd pos)
  (unless (free-id-ddict? dd)
    (raise-argument-error 'free-id-ddict-iterate-key "free-id-ddict?" dd))
  (unless (and (free-id-ddict-position? pos)
               (eq? dd (free-id-ddict-position-dd pos)))
    (raise-argument-error 'free-id-ddict-iterate-key
                          "valid position for given free-id-ddict"
                          pos))
  (free-id-ddict-position-key pos))

;;
;; free-id-ddict-iterate-value
;;
(define (free-id-ddict-iterate-value dd pos)
  (unless (free-id-ddict? dd)
    (raise-argument-error 'free-id-ddict-iterate-value "free-id-ddict?" dd))
  (unless (and (free-id-ddict-position? pos)
               (eq? dd (free-id-ddict-position-dd pos)))
    (raise-argument-error 'free-id-ddict-iterate-value
                          "valid position for given free-id-ddict"
                          pos))
  (free-id-ddict-position-val pos))


(define (recur-print mode p port)
  (case mode
    [(#t) (write p port)]
    [(#f) (display p port)]
    [else (print p port mode)]))

;; 
;; free-id-ddict-print
;;
(define (ifree-id-ddict-print dd port mode)
  (if mode
      (write-string "#<free-id-ddict: " port)
      (write-string "(free-id-ddict " port))
  (recur-print mode (free-id-ddict->list dd) port)
  (if mode
      (write-string ">" port)
      (write-string ")" port)))

(define (mfree-id-ddict-print dd port mode)
  (if mode
      (write-string "#<mutable-free-id-ddict: " port)
      (write-string "(mutable-free-id-ddict " port))
  (recur-print mode (free-id-ddict->list dd) port)
  (if mode
      (write-string ">" port)
      (write-string ")" port)))

;; 
;; free-id-ddict=?
;; 
(define (ifree-id-ddict=? dd1 dd2 rec-equal?)
  (rec-equal? (immutable-free-id-ddict-elems dd1)
              (immutable-free-id-ddict-elems dd2)))

(define (mfree-id-ddict=? dd1 dd2 rec-equal?)
  (rec-equal?
   (unsafe-vector*-ref (unsafe-unbox* (mutable-free-id-ddict-content-box dd1))
                       0)
   (unsafe-vector*-ref (unsafe-unbox* (mutable-free-id-ddict-content-box dd2))
                       0)))




;; 
;; free-id-ddict-hash-code
;; 
(define (ifree-id-ddict-hash-code dd rec-hc)
  (rec-hc (immutable-free-id-ddict-elems dd)))

(define (mfree-id-ddict-hash-code dd rec-hc)
  (rec-hc (content-elems (unbox (mutable-free-id-ddict-content-box dd)))))


(struct immutable-free-id-ddict (elems del seq)
  #:constructor-name ifree-id-ddict
  #:methods gen:equal+hash
  [(define equal-proc ifree-id-ddict=?)
   (define hash-proc ifree-id-ddict-hash-code)
   (define hash2-proc ifree-id-ddict-hash-code)]
  #:methods gen:custom-write
  [(define write-proc ifree-id-ddict-print)]
  #|#:methods gen:dict
  [(define dict-ref free-id-ddict-ref)
   (define dict-set free-id-ddict-set)
   (define dict-remove free-id-ddict-remove)
   (define dict-iterate-first free-id-ddict-iterate-first)
   (define dict-iterate-next free-id-ddict-iterate-next)
   (define dict-iterate-key free-id-ddict-iterate-key)
   (define dict-iterate-value free-id-ddict-iterate-value)
   (define dict-has-key? free-id-ddict-has-key?)
   (define dict-set* free-id-ddict-set*)
   (define dict-update free-id-ddict-update)
   (define dict-map free-id-ddict-map)
   (define dict-for-each free-id-ddict-for-each)
   (define dict-empty free-id-ddict-empty?)
   (define dict-count free-id-ddict-count)
   (define dict-copy free-id-ddict-copy)
   (define dict-clear free-id-ddict-clear)
   (define dict-keys free-id-ddict-keys)
   (define dict-values free-id-ddict-values)
   (define dict->list free-id-ddict->list)
   (define in-dict in-free-id-ddict)
   (define in-dict-keys in-free-id-ddict-keys)
   (define in-dict-values in-free-id-ddict-values)]
  |#)

(define (unsafe-immutable-free-id-ddict-elems dd)
  (unsafe-struct*-ref dd 0))
(define (unsafe-immutable-free-id-ddict-seq dd)
  (unsafe-struct*-ref dd 2))

(struct mutable-free-id-ddict (content-box)
  #:constructor-name unsafe-mk-mfree-id-ddict
  #:methods gen:equal+hash
  [(define equal-proc mfree-id-ddict=?)
   (define hash-proc mfree-id-ddict-hash-code)
   (define hash2-proc mfree-id-ddict-hash-code)]
  #:methods gen:custom-write
  [(define write-proc mfree-id-ddict-print)]
  
  #| Maybe provide the dict interface
     If we use contracts here blame will be screwed up.
     This is because the contract boundary has to be setup here
     and the contracts will blame this code instead of the module
     boundaries. 
  #:methods gen:dict
  [(define dict-ref free-id-ddict-ref)
   (define dict-set! free-id-ddict-set!)
   (define dict-remove! free-id-ddict-remove!)
   (define dict-iterate-first free-id-ddict-iterate-first)
   (define dict-iterate-next free-id-ddict-iterate-next)
   (define dict-iterate-key free-id-ddict-iterate-key)
   (define dict-iterate-value free-id-ddict-iterate-value)
   (define dict-has-key? free-id-ddict-has-key?)
   (define dict-set*! free-id-ddict-set*!)
   (define dict-update! free-id-ddict-update!)
   (define dict-map free-id-ddict-map)
   (define dict-for-each free-id-ddict-for-each)
   (define dict-empty free-id-ddict-empty?)
   (define dict-count free-id-ddict-count)
   (define dict-copy free-id-ddict-copy)
   (define dict-clear! free-id-ddict-clear!)
   (define dict-keys free-id-ddict-keys)
   (define dict-values free-id-ddict-values)
   (define dict->list free-id-ddict->list)
   (define in-dict in-free-id-ddict)
   (define in-dict-keys in-free-id-ddict-keys)
   (define in-dict-values in-free-id-ddict-values)]
  |#)

(define (unsafe-mutable-free-id-ddict-elems mdd)
  (unsafe-vector*-ref (unsafe-unbox* (unsafe-struct*-ref mdd 0)) 0))
(define (unsafe-mutable-free-id-ddict-seq mdd)
  (unsafe-vector*-ref (unsafe-unbox* (unsafe-struct*-ref mdd 0)) 2))

;; NOTE: we assume this vector is of length 3 w/ unsafe ops,
;; change any/all unsafe-vector... operations if this is modified
(define-syntax-rule (content a b c) (vector-immutable a b c))

;; NOTE: we assume this structure for mfree-id-ddicts (i.e. that it contains
;; a box which contains a 'content-vector' -- if any of this is changed,
;; all unsafe ops must also be changed)
(define-syntax-rule (mfree-id-ddict elems del seq)
  (unsafe-mk-mfree-id-ddict (box (content elems del seq))))

(define (content-elems c) (vector-ref c 0))
;(define (content-del c) (vector-ref c 1))
;(define (content-seq c) (vector-ref c 2))

(define-syntax-rule (try-update-mfree-id-ddict-content! mdd elems del seq)
  (let* ([content-box (mutable-free-id-ddict-content-box mdd)]
         [orig-content (unsafe-unbox* content-box)]
         [new-content (content elems del seq)])
    (unsafe-box*-cas! content-box orig-content new-content)))

(define (force-update-mfree-id-ddict-content! mdd elems del seq)
  (define content-box (mutable-free-id-ddict-content-box mdd))
  (unsafe-set-box*! content-box (content elems del seq)))

;; NOTE: keep these in sync w/ above defs!!!!!!
(define empty-free-id-ddict (ifree-id-ddict (make-immutable-free-id-table) 0 '()))

