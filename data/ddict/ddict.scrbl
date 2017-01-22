#lang scribble/doc

@(require scribble/manual
          scribble/example
          (for-label racket/base
                     racket/contract
                     data/ddict))

@title{Simple, Deterministic Dictionaries}
@author{@(author+email "Andrew Kent" "andmkent@iu.edu")}

@(define (concurrency-caveat)
  @elemref['(caveat "concurrent ddict modification")]{caveats concerning concurrent modification})
@(define (mutable-key-caveat)
  @elemref['(caveat "mutating ddict keys")]{caveat concerning mutable keys})

@(define (see-also-caveats)
   @t{See also the @concurrency-caveat[] and the @mutable-key-caveat[] above.})
@(define (see-also-concurrency-caveat)
   @t{See also the @concurrency-caveat[] above.})
@(define (see-also-mutable-key-caveat)
@t{See also the @mutable-key-caveat[] above.})


@(define the-eval (make-base-eval))
@(the-eval '(require data/ddict))



@defmodule[data/ddict]

This package defines immutable and mutable @deftech{
 deterministic dictionaries} (or @deftech{ddict}s, for
short). A @tech{ddict} is a
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{
 dictionary} whose interface mimics that of a
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{
 hash table} but which also guarantees LIFO ordering when iterating
over the elements of the dictionary.


@examples[
 #:eval the-eval
 (define dd (ddict 'A 0 'B 1 'C 2))
 dd
 "Note the ordering is LIFO w.r.t. the keys' insertion order"
 (ddict->list (ddict-set dd 'Z 25))
 
 (define mdd (for/mutable-ddict ([idx (in-naturals)]
                                 [name (in-list '(null eins zwei drei))])
               (values idx name)))
 mdd
 (ddict-set! mdd 4 'vier)
 (ddict-set! mdd 6 'sechs)
 (ddict-remove! mdd 1)
 (ddict-remove! mdd 3)
 (for ([(key val) (in-ddict mdd)]
       [n (in-naturals)])
   (printf "key ~a: ~a\n" n key)
   (printf "val ~a: ~a\n" n val))
 ]



@elemtag['(caveat "concurrent ddict modification")]{@bold{Caveats concerning
  concurrent modification:}} A mutable ddict can be
manipulated with @racket[ddict-set!],
@racket[ddict-remove!], @racket[ddict-ref!], and
@racket[ddict-update!] concurrently by multiple threads;
updates to internal state are performed with a check-and-set
operation which protects from invalid internal states.
However, the following caveats apply:

 @itemize[

 @item{Keys which are removed concurrently during the
  following operations may or may not be visited during the
  traversal: @racket[ddict-map], @racket[ddict-for-each],
  @racket[in-ddict] @racket[in-ddict-keys], and
  @racket[in-ddict-values].}

 @item{The @racket[ddict-update!] and @racket[ddict-ref!]
  functions perform a separate @racket[ddict-ref] and
  @racket[ddict-set!] when required as part of their
  functionality, which means that the update as a whole is not
  ``atomic.''}
]

@elemtag['(caveat "mutating ddict keys")]{@bold{Caveat concerning
  mutable keys:}} If a key in an @racket[equal?]-based ddict
is mutated, then the ddict's behavior for insertion and
lookup operations becomes unpredictable.



@section{Constructors}


@deftogether[(
  @defproc[(ddict [key any/c] [val any/c] ... ...) (and/c immutable-ddict? ddict-equal?)]
  @defproc[(ddicteqv [key any/c] [val any/c] ... ...) (and/c immutable-ddict? ddict-eqv?)]
  @defproc[(ddicteq [key any/c] [val any/c] ... ...) (and/c immutable-ddict? ddict-eq?)]
  @defproc[(mutable-ddict [key any/c] [val any/c] ... ...) (and/c mutable-ddict? ddict-equal?)]
  @defproc[(mutable-ddicteqv [key any/c] [val any/c] ... ...) (and/c mutable-ddict? ddict-eqv?)]
  @defproc[(mutable-ddicteq [key any/c] [val any/c] ... ...) (and/c mutable-ddict? ddict-eq?)]
)]{

 Creates a @tech{ddict} with each @racket[key] mapped to the
 immediately following @racket[val]. Each @racket[key] must
 have a @racket[val], so the total number of arguments must
 be even. Each constructor also specifies how keys are
 compared (e.g. @racket[ddict] compares keys with @racket[equal?],
 @racket[ddicteqv] compares keys with @racket[eqv?], etc).

The @racket[key] to @racket[val] mappings are added to the table in the order they appear in
the argument list, so later mappings can hide earlier ones if the @racket[key]s are equal.
}

@deftogether[(
   @defproc[(make-ddict [assocs (listof pair?) null]) (and/c immutable-ddict? ddict-equal?)]
   @defproc[(make-ddicteqv [assocs (listof pair?) null]) (and/c immutable-ddict? ddict-eqv?)]
   @defproc[(make-ddicteq [assocs (listof pair?) null]) (and/c immutable-ddict? ddict-eq?)]
   @defproc[(make-mutable-ddict [assocs (listof pair?) null]) (and/c mutable-ddict? ddict-equal?)]
   @defproc[(make-mutable-ddicteqv [assocs (listof pair?) null]) (and/c mutable-ddict? ddict-eqv?)]
   @defproc[(make-mutable-ddicteq [assocs (listof pair?) null]) (and/c mutable-ddict? ddict-eq?)]
)]{
Creates a @tech{ddict} that is initialized with the contents of @racket[assocs]. In each element of
@racket[assocs], the @racket[car] is a key, and the @racket[cdr] is the corresponding value. The mappings 
are added to the table in the order they appear in the argument list, so later mappings can hide earlier
ones if the @racket[key]s are equivalent w.r.t. the table's key comparison function.

}

@section{Basic Predicates}

@defproc[(ddict? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a @tech{ddict} (i.e. if it is either
 a @tech{immutable-ddict} or a @tech{mutable-ddict}), @racket[#f] otherwise.
}

@defproc[(immutable-ddict? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is an @deftech{immutable-ddict}, @racket[#f] otherwise.
}

@defproc[(mutable-ddict? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a @deftech{mutable-ddict}, @racket[#f] otherwise.
}


@deftogether[(
  @defproc[(ddict-equal? [dd ddict?]) boolean?]
  @defproc[(ddict-eqv? [dd ddict?]) boolean?]
  @defproc[(ddict-eq? [dd ddict?]) boolean?]
)]{

 @racket[ddict-equal?] returns @racket[#t] if the given @tech{ddict}'s keys are compared with @racket[equal?], @racket[#f] otherwise.

 @racket[ddict-eqv?] returns @racket[#t] if the given @tech{ddict}'s keys are compared with @racket[eqv?], @racket[#f] otherwise.

 @racket[ddict-eq?] returns @racket[#t] if the given @tech{ddict}'s keys are compared with @racket[eq?], @racket[#f] otherwise.
}

@section{Basic Operations}

@defproc[(ddict-set [dd immutable-ddict?] [key any/c] [val any/c]) immutable-ddict?]{
 Functionally extends @racket[dd] by mapping @racket[key] to @racket[val], overwriting any existing mapping
 for @racket[key], and returning the extended @tech{ddict}.

 @see-also-mutable-key-caveat[]
}

@defproc[(ddict-set! [dd mutable-ddict?] [key any/c] [val any/c]) void?]{
 Extends @racket[dd] by mapping @racket[key] to @racket[val],
 overwriting any existing mapping for @racket[key].

 @see-also-caveats[]
}

@defproc[(ddict-ref [dd ddict?]
                    [key any/c]
                    [failure-result (failure-result/c any/c)
                     (λ () (raise (exn:fail:contract ....)))])
         any/c]{
Returns the value for @racket[key] in @racket[dd]. If no value is found for @racket[key], then
@racket[failure-result] determines the result:

@itemize[

 @item{If @racket[failure-result] is a procedure, it is
   called with no arguments to produce the result.}

 @item{Otherwise, @racket[failure-result] is returned as the result.}

]

@see-also-mutable-key-caveat[]
}

@defproc[(ddict-has-key? [dd ddict?] [key any/c]) boolean?]{
Returns @racket[#t] if @racket[dd] contains a value for the given @racket[key], @racket[#f] otherwise.
}

@defproc[(ddict-remove [dd immutable-ddict?] [key any/c]) immutable-ddict?]{
Functionally removes any existing mapping for @racket[key] in @racket[dd], returning the fresh @tech{ddict}.

@see-also-mutable-key-caveat[]
}

@defproc[(ddict-remove! [dd mutable-ddict?] [key any/c]) void?]{
Removes any existing mapping for @racket[key] in @racket[dd].

@see-also-caveats[]
}

@defproc[(ddict-count [dd ddict?]) exact-nonnegative-integer?]{
Returns the number of keys mapped by @racket[dd].
}

@defproc[(ddict-empty? [dd ddict?]) boolean?]{
Returns @racket[#t] just in case @racket[(zero? (ddict-count dd))] is @racket[#t], @racket[#f] otherwise.
}

@section{Additional Operations}

@defproc[(ddict-set* [dd immutable-ddict?] [key any/c] [val any/c] ... ...) immutable-ddict?]{
Functionally extends @racket[dd] by mapping each @racket[key] to the following @racket[val], overwriting
any existing mapping for each @racket[key], and returning the extended @tech{ddict}. Mappings are added to
the table in the order they appear in the argument list, so later mappings can hide earlier ones if the 
@racket[key]s are equivalent w.r.t. the table's key comparison function.
}

@defproc[(ddict-set*! [dd mutable-ddict?] [key any/c] [val any/c] ... ...) void?]{
Extends @racket[dd] by mapping each @racket[key] to the following @racket[val], overwriting
any existing mapping for each @racket[key], and returning the extended @tech{ddict}. Mappings are added to
the table in the order they appear in the argument list, so later mappings can hide earlier ones if the 
@racket[key]s are equivalent w.r.t. the table's key comparison function.
}



@defproc[(ddict-ref! [dd mutable-ddict?] [key any/c] [to-set any/c])
         any/c]{

Returns the value for @racket[key] in @racket[dd].  If no value is
found for @racket[key], then @racket[to-set] determines the result as
in @racket[ddict-ref] (i.e., it is either a thunk that computes a value
or a plain value), and this result is stored in @racket[dd] for the
@racket[key].

@see-also-caveats[]}

@defproc[(ddict-update [dd immutable-ddict?]
                       [key any/c]
                       [updater (any/c . -> . any/c)]
                       [failure-result (failure-result/c any/c)
                        (λ () (raise (exn:fail:contract ....)))])
         any/c]{

Composes @racket[ddict-ref] and @racket[ddict-set] to functionally
update an existing mapping in @racket[dd], where the optional
@racket[failure-result] argument is used as in @racket[ddict-ref] when
no mapping exists for @racket[key] already.

@see-also-mutable-key-caveat[]
}


@defproc[(ddict-update! [dd mutable-ddict?]
                        [key any/c]
                        [updater (any/c . -> . any/c)]
                        [failure-result (failure-result/c any/c)
                         (λ () (raise (exn:fail:contract ....)))])
         void?]{

 Composes @racket[ddict-ref] and @racket[ddict-set!] to
 update an existing mapping in @racket[dd], where the
 optional @racket[failure-result] argument is used as in
 @racket[ddict-ref] when no mapping exists for @racket[key]
 already.

@see-also-caveats[]
}

@defproc[(ddict-keys-subset? [dd1 ddict?] [dd2 ddict?])
         boolean?]{

Returns @racket[#t] if @racket[dd2] contains an entry for each
        key present in @racket[dd1], otherwise returns @racket[#f].}

@defproc[(ddict-clear! [dd mutable-ddict?])
         void?]{

Removes all mappings from @racket[dd] in constant time.

@see-also-caveats[]}


@defproc[(ddict-clear [dd immutable-ddict?])
         immutable-ddict?]{

Functionally removes all mappings from @racket[dd] in constant time.}

@defproc[(ddict-copy [dd ddict?]) 
         mutable-ddict?]{

Returns a mutable ddict with the same mappings, same
key-comparison mode as @racket[dd].}

@defproc[(ddict-copy-clear [dd ddict?]) ddict?]{

Produces an empty @tech{ddict} with the same key-comparison
procedure and mutability of @racket[dd].}


@section{Traversal and Iteration Functions}

@defproc[(ddict-map [dd ddict?] [proc (any/c any/c . -> . any/c)]) (listof any/c)]{
Applies the procedure @racket[proc] to each key and associated value of @racket[dd] in LIFO order w.r.t. the order
they were inserted into @racket[dd], accumulating the results into a list.
}

@defproc[(ddict-for-each [dd ddict?] [proc (any/c any/c . -> . any/c)]) void?]{
Applies the procedure @racket[proc] to each key and associated value of @racket[dd] (for the side-effects of
@racket[proc]) in LIFO order w.r.t. the order they were inserted into @racket[dd].
}

@defproc[(ddict->list [dd ddict?]) (listof (cons/c any/c any/c))]{
Returns a list of the key--value pairs of @racket[dd] in in LIFO order w.r.t. the order
they were inserted into @racket[dd].
}

@defproc[(ddict-keys [dd ddict?]) (listof any/c)]{
Returns a list of the keys in @racket[dd] in LIFO order w.r.t. the order
they were inserted into @racket[dd].
}

@defproc[(ddict-values [dd ddict?]) (listof any/c)]{
Returns a list of the values in @racket[dd] in LIFO order w.r.t. the order
their associated keys were inserted into @racket[dd].
}


@defproc[(ddict-position? [v any/c])
         boolean?]{

Returns @racket[#t] if @racket[v] is a position used for iterating over a ddict
 , otherwise returns @racket[#f].}


@deftogether[(
  @defproc[(ddict-iterate-first [dd ddict?]) ddict-position?]
  @defproc[(ddict-iterate-next [dd ddict?] [pos ddict-position?]) ddict-position?]
  @defproc[(ddict-iterate-key [dd ddict?] [pos ddict-position?]) any/c]
  @defproc[(ddict-iterate-value [dd ddict?] [pos ddict-position?]) any/c]
  )]{

 Functions which allow for the manual iteration over a ddict, in LIFO order w.r.t.
 the insertion order of the keys.}


@deftogether[(
  @defproc[(in-ddict [dd ddict?]) sequence?]
  @defproc[(in-ddict-keys [dd ddict?]) sequence?]
  @defproc[(in-ddict-values [dd ddict?]) sequence?]
)]{

 @racket[in-ddict] returns a sequence containing the keys and associated values of @racket[dd]
 in LIFO order w.r.t. the order they were inserted into @racket[dd].

 @racket[in-ddict-keys] returns a sequence containing the keys of @racket[dd]
 in LIFO order w.r.t. the order they were inserted into @racket[dd].

 @racket[in-ddict-values] returns a sequence containing the values of @racket[dd]
 in LIFO order w.r.t. the order their keys were inserted into @racket[dd].

 These forms provide for fast, direct iteration when used in
 conjunction with Racket's various @racket[for] loops.}

@deftogether[(
@defform[(for/ddict (for-clause ...) body-or-break ... body)]
@defform[(for/ddicteqv (for-clause ...) body-or-break ... body)]
@defform[(for/ddicteq (for-clause ...) body-or-break ... body)]
@defform[(for*/ddict (for-clause ...) body-or-break ... body)]
@defform[(for*/ddicteqv (for-clause ...) body-or-break ... body)]
@defform[(for*/ddicteq (for-clause ...) body-or-break ... body)]
@defform[(for/mutable-ddict (for-clause ...) body-or-break ... body)]
@defform[(for/mutable-ddicteqv (for-clause ...) body-or-break ... body)]
@defform[(for/mutable-ddicteq (for-clause ...) body-or-break ... body)]
@defform[(for*/mutable-ddict (for-clause ...) body-or-break ... body)]
@defform[(for*/mutable-ddicteqv (for-clause ...) body-or-break ... body)]
@defform[(for*/mutable-ddicteq (for-clause ...) body-or-break ... body)]
)]{
  Like @racket[for/hash], but producing a @tech{ddict} with the
 respective mutability and key comparison function.
}

@section{Performance and Memory Usage}

@bold{Performance.} Immutable and mutable @tech{ddict}s
internally use Racket's immutable @racket[hash] data
structure along with a @racket[list] of keys in order to
provide @racket[hash]-like performance and a deterministic
iteration order. @tech{ddict} operations obviously have
overhead which the native hash operations do not, but in
micro benchmarks @tech{ddict} operations appear no worse
than 2x their equivalent hash operation, and as the size of
a dictionary increases the overhead becomes less noticeable
(i.e. since the overhead is constant for the atomic @tech{
 ddict} operations the asymptotic complexity is unaffected).

@bold{Memory Usage.} In order to keep @tech{ddict}
operations such as @racket[ddict-remove] efficient (i.e.
non-linear), we do not immediately remove elements from the
internal key list. Instead, a certain amount of
``fragmentation'' in the key list is tolerated, but once it
passes a predetermined threshold (when the number of removed
key slots exceeds the number of active keys), the list is
then ``defragmented''. To prevent this fragmentation from
causing unexpected memory leaks, each key in the key list is
stored in a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{
 weak-box} so its presence in the key list
after removal does not prevent garbage collection that would
otherwise occur.

The hope is that these implementation details are mostly
unobservable to @tech{ddict} users since their costs will be
amortized.

If a user is concerned and wants more fine grained control
over the presence of internal fragmentation (e.g. if
removals are performed early on in computation then never
again) the following functions report on the presence
of fragmentation and allow a user to remove any:


@defproc[(ddict-compact? [dd ddict?]) boolean?]{
Returns @racket[#t] if @racket[dd] contains no fragmentation
        in its key list, otherwise returns @racket[#f].
}

@defproc[(ddict-compact [dd immutable-ddict?]) immutable-ddict?]{
 Returns a defragmented version of @racket[dd] (i.e. the
 mappings are the same, but any unnecessary space in the
 internal key list is removed).}

@defproc[(ddict-compact! [dd mutable-ddict?]) void?]{
 Defragments the internal key list of @racket[dd].}