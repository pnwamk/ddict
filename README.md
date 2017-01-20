# Deterministic Dictionaries for Racket

[![Build Status](https://travis-ci.org/pnwamk/racket-ddict.svg?branch=master)](https://travis-ci.org/pnwamk/racket-ddict)

This library provides a simple dictionary data type that resembles Racket's built in `hash` data structure, except that it provides deterministic (LIFO) ordering for iteration and key/value lists. This library does not have any dependencies aside from `racket/base`.

Functionality is intended to match Racket's built in `hash` in the user API and performance as much as possible, so that `ddict`'s can be a simple drop-in replacement for `hash`'s when determinstic ordering is desired.

# API

The API for these dictionaries is intended to be identical to the `hash` API as much as possible. The following identifiers are provided:

```
ddict?
immutable-ddict?
mutable-ddict?
ddict-equal?
ddict-eqv?
ddict-eq?
ddict
ddicteqv
ddicteq
mutable-ddict
mutable-ddicteqv
mutable-ddicteq
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
ddict-iterate-first
ddict-iterate-next
ddict-iterate-key
ddict-iterate-value
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
ddict-compact!
```
