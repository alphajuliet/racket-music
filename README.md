# racket-music

Explorations in musical theory and manipulation using Racket, and the associated
`rsound` package. Support from my functional library `rakeda`.

This contains a bunch of utilities focused on lists of integers modulo 12 (notes in an octave)
and note names. We also create named chords, and define some Neo-Riemannian transforms.

This is very much a work in progress.

## Modules

### music.rkt

The basic functions for manipulating and converting between note numbers (integers) and note names ('C to 'B), and providing basic functions for transposing, inverting, and modulo 12. It also provides some utility functions for mapping over either single items, or lists.   

There are also wrappers for functions that expect note numbers that can be used for note names, so that you can, for example, transpose the notes '(C D E).

Most functions that take more than one argument are already curried so that you can easily use them in higher-order functions without explicitly doing partial evaluation. An example is `(wrap (transpose 2) '(C D E))`.

### chord.rkt

Chords are implemented as a simple type with two fields: root note and chord name, e.g. `(chord 'E 'minor)`. A hash table of common chords are defined in `chords` so that we can switch between note numbers, note names, and chord names using the provided conversion functions. This includes a reverse lookup `(num->chord...)` that tries to match a list of note numbers to a defined chord name.

A wrapper function is also provided for chords, e.g. `(wrapc inversions (chord 'D 'major))`. There are also functions for finding notes in one or more chords. 

### neoriemann.rkt

Define the PLR group of functions on triads from [Neo-Riemannian Theory] (https://en.wikipedia.org/wiki/Neo-Riemannian_theory).

- P is an involution (i.e. a permutation with cycle length 2) between the major and
  minor triad of the same root: P(Cmaj) = Cmin
- R is an involution between relative major and minor triads: R(Cmaj) = Amin
- L doesn't have a recognised musical mapping: L(Cmaj) = Emin

It also defines the related transforms N, S, and H, and a function to more easily chain them over a starting chord.

