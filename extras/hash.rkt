#lang racket/base

(require (only-in racket/unsafe/ops
                  [unsafe-struct*-ref struct-ref]
                  [unsafe-struct*-set! struct-set!]
                  [unsafe-fxmodulo modulo]
                  [unsafe-fx+ +]
                  [unsafe-fx* *]
                  [unsafe-fxxor xor]
                  [unsafe-fxlshift <<]
                  [unsafe-fxrshift >>]
                  [unsafe-mcar entry-key]
                  [unsafe-mcdr entry-val]
                  [unsafe-set-mcdr! set-entry-val!]
                  [unsafe-vector*-length vector-length]
                  [unsafe-vector*-ref vector-ref]
                  [unsafe-vector*-set! vector-set!])
         (for-syntax racket/base))

(define-syntax entry (make-rename-transformer #'mcons))

(struct umap (count table) #:transparent)

(define-syntax-rule (countof m) (struct-ref m 0))
(define-syntax-rule (set-count! m val) (struct-set! m 0 val))
(define-syntax-rule (tableof m) (struct-ref m 1))
(define-syntax-rule (set-table! m val) (struct-set! m 1 val))

(define empty-umap (umap 0 (make-vector 10 '())))

(define (grow! m table table-len)
  (define new-table-len (+ 1 (<< table-len 1)))
  (define new-table (make-vector new-table-len '()))
  (for* ([bucket (in-vector table)]
         [e (in-list bucket)])
    (define idx (modulo (equal-hash-code (entry-key e)) new-table-len))
    (vector-set! new-table idx (cons e (vector-ref new-table idx))))
  (set-table! m new-table))

(define (umap-set! m key val)
  (unless (umap? m) (error 'umap-set! "not a umap! ~a" m))
  (define count (countof m))
  (define table (tableof m))
  (define table-len (vector-length table))
  (when (> count (* 3 (>> table-len 2)))
    (grow! m table table-len))
  (define key-hc (equal-hash-code key))
  (define bucket-idx (modulo key-hc (vector-length table)))
  (define bucket (vector-ref table bucket-idx))
  (cond
    [(pair? bucket)
     (define e0 (car bucket))
     (cond
       [(equal? key (entry-key e0))
        (set-entry-val! e0 val)]
       [else
        (let loop ([rst (cdr bucket)])
          (cond
            [(pair? rst)
             (define e (car bucket))
             (cond
               [(equal? key (entry-key e))
                (set-entry-val! e val)]
               [else (loop (cdr rst))])]
            [else (vector-set! table bucket-idx (cons (entry key val) bucket))
                  (set-count! m (+ 1 count))]))])]
    [else
     (vector-set! table bucket-idx (cons (entry key val) '()))
     (set-count! m (+ 1 count))]))

(define (make-umap) (umap 0 (make-vector 7 '())))

(define (random-key)
  (list->string
   (map integer->char
        (for/list ([i (in-range 1 (add1 (random 20)))])
          (random 256)))))


(define (test-set! num)
  (define keys (for/list ([_ (in-range num)]) (random-key)))
  (define iters (/ 100000 num))
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (printf "hash-set! (~a)\n" num)
  (time (for ([_ (in-range iters)])
          (let ([h (make-hash)])
            (for ([key (in-list keys)])
              (hash-set! h key key)))))
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (printf "uamp-set! (~a)\n" num)
  (time (for ([_ (in-range iters)])
          (let ([u (make-umap)])
            (for ([key (in-list keys)])
              (umap-set! u key key))))))


(test-set! 10)
(test-set! 100)
(test-set! 1000)
(test-set! 10000)
(test-set! 100000)



