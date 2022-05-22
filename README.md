# racket-music

Explorations in musical theory and manipulation using Racket, and the associated
`rsound` package. Support from my functional library `rakeda`.

This contains a bunch of utilities focused on lists of integers modulo 12 (notes in an octave)
and note names. We also create named chords, and define some Neo-Riemannian transforms.

This is very much a work in progress.

## Modules

### core.rkt

The basic functions for manipulating and converting between note numbers (integers) and note names ('C to 'B), and providing basic functions for transposing, inverting, and modulo 12. 

- `transpose transpose* invert invert*`
- `inversions`
- `note->num num->note num->note* collapse`
- `map-note`

Transpositions and inversions expect note numbers. `map-note` is a wrapper for note names, for example `(map-note (transpose 4) '(C D E))`.

Functions that take more than one argument are already curried so that you can easily use them in higher-order functions without explicitly doing partial evaluation. 

### chord.rkt

Chords are implemented as a simple type with two fields: root note and chord name, e.g. `(chord 'E 'minor)`. A hash table of common chords are defined in `chords` so that we can switch between note numbers, note names, and chord names using the provided conversion functions. This includes a reverse lookup `(num->chord...)` that tries to match a list of note numbers to a defined chord name.

- `chord->num num->chord note->chord chord->notes chord->notes* canonical`
- `map-chord map-chord<`
- `contains-note? chord-contains?`
- `random-chord random-related-chord random-related-chords random-chords-with-note random-chord-generator`

A mapping function is also provided for chords, e.g. `(map-chord inversions (chord 'D 'major))`. There are also functions for finding notes in one or more chords, and for generating chords within some constraints. 

### scale.rkt

Scales are based on a root note and a scale name, e.g. major, minor, myxolydian

- `scale->num scale->notes scale->notes*`

### neoriemann.rkt

Define the PLR group of functions on triads from [Neo-Riemannian Theory] (https://en.wikipedia.org/wiki/Neo-Riemannian_theory).

- P is an involution (i.e. a permutation with cycle length 2) between the major and
  minor triad of the same root: P(Cmaj) = Cmin
- R is an involution between relative major and minor triads: R(Cmaj) = Amin
- L doesn't have a recognised musical mapping: L(Cmaj) = Emin

It also defines the related transforms N, S, and H, and a function `nrt/compose` to chain them over a starting chord.

### visual.rkt

The function `view-chord` plots a chord as a series of points on a modulo 12 circle with lines joining them, as a way of visualising a chord shape. I scratched the itch but not sure how useful it is.

### audio.rkt

By leveraging the `rsound` Racket package, we can hear notes and chords. This is still in development but the most important function so far is `play-chord`, for obvious reasons. This module could really go anywhere; it's tempting to go all in and build out higher layers of composition and performance, but we already have `Overtone` and others for that.

### sound.rkt

This is a space for defining audio things like instruments. There's a basic dual saw-wave generator, and a simple sine wave. Maybe others to come.
