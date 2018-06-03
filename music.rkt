#lang racket
; music.rkt
; Define musical functions
; AJ 2017-04-28

(require rsound
         threading)

(provide (all-defined-out))

;-----------------------
; Utility functions

(define (identity x) x)

; Generic map that is polymorphic across lists and values
; map+ :: ([a] → [b]) → [a] → [b]
; map+ :: ([a] → [b]) → a → b
(define (map+ f x)
  (if (list? x)
      (map f x)
      (f x)))

; sort-nums :: Sortable a ⇒ [a] → [a]
(define (sort-nums lst) (sort lst <))

; Rotate a list one position left
; e.g. (rotate '(F# A C#)) → '(A C# F#)
; rotate :: [a] → [a]
(define (rotate ch)
  (if (list? ch)
      (append (rest ch) (list (first ch)))
      ch))

; Minimum of a list
; list-min :: Sortable a ⇒ [a] → a
(define list-min (curry argmin identity))

; Canonical structure of a list
; e.g (canonical '(2 6 8)) -> '(0 4 7), i.e. this is a major chord
; canonical :: Sortable a ⇒ [a] → [a]
(define (canonical ch)
  (map+ (curry transpose (- (list-min ch))) ch))

; flatten :: [a] → [a] → [a]
(define (flatten lst)
  (cond ((null? lst) '())
        ((pair? lst)
         (append (flatten (car lst)) (flatten (cdr lst))))
        (else (list lst))))

; zip :: [a] → [a] → ... → [a]
(define zip (curry map list))

; zip-with
(define (zip-with fn lst1 lst2)
  (map (curry apply fn) (zip lst1 lst2)))

; Zip and flatten
; flatzip :: [a] → [a] → [a]
(define flatzip (compose flatten zip))

;-----------------------
; Musical data structures and conversions

; Define the scale and standard note names assuming C as root
; scale :: [Integer]
(define scale (range 0 11))

; Note names, using my favourite mix of sharps and flats
; define NoteName = Symbol
; noteNames :: [NoteNames]
(define noteNames `(C C# D Eb E F F# G Ab A Bb B))

; Polymorphic conversions between note numbers and note names
; num->note :: [Integer] → [NoteName] || Integer → NoteName
(define (num->note n)
  (map+ (curry list-ref noteNames) n))

; note->num :: [NoteName] → [Integer] || NoteName → Integer
(define (note->num n)
  (map+ (curry index-of noteNames) n))

; Sorted version of num->note
; sorted-num->note :: [Integer] → [NoteName]
(define sorted-num->note
  (compose num->note sort-nums))

; Define wrappers around functions that transform note numbers
(define (wrap f)
  (compose num->note f note->num))
(define (wrap* f)
  (compose sorted-num->note f note->num))
(define (wrap2 f x y)
  (~>> y
       note->num
       (map+ (curry f x))
       num->note))

;-----------------------
(define mod12 (curryr modulo 12))
(define transpose (compose mod12 +))
(define (invert n x) (mod12 (- n x)))

;-----------------------
; Define a chord as a list of intervals from the base note. It returns a
; function that takes a given base note number and returns the chord of note numbers.
; define Chord = Integer → [Integer]
; chord :: [Integer] → Chord
(define (chord ns)
  (λ (n) (map (curry transpose n) ns)))

; Define chords as functions operating on a note number
(define-syntax-rule (defchord name ns)
  (define name (chord ns)))

(defchord major   '(0 4 7))
(defchord minor   '(0 3 7))
(defchord x4+5    '(0 5 7))
(defchord x7      '(0 4 7 10))
(defchord maj7    '(0 4 7 11))
(defchord min7    '(0 3 7 10))
(defchord maj9    '(0 4 7 14))
(defchord min9    '(0 3 7 14))
(defchord maj7+9  '(0 4 7 11 14))
(defchord min7+9  '(0 3 7 10 14))
(defchord maj6    '(0 4 9))
(defchord min6    '(0 3 9))
(defchord aug     '(0 4 8))
(defchord maj2    '(0 2 4 7))
(defchord min2    '(0 2 3 7))
(defchord dim     '(0 3 6))

(define chord-defns
  (hash 'major '(0 4 7)
        'minor '(0 3 7)
        'x4+5  '(0 5 7)))

; main-chords :: [Chord]
(define main-chords (list major minor x7 maj7 min7))

; Show a list of chords for a given note
; apply-map :: [Chord] → Symbol → [Symbol]
(define (apply-map fns x)
  (map (λ (f) (f x)) fns))

(define (show-chords ch nt)
  (map num->note (apply-map ch (note->num nt))))

; Match note numbers to one or more chord names
; This will do a match across all the given chords
; @@TODO
(define (match-chord lst)
  (let ([x (map canonical (permutations lst))])
    #f))

;----------------
; Friendly definitions for use

(define (tr* n nt) (wrap2 transpose n nt))
(define (inv* n nt) (wrap2 invert n nt))

; Show chord
; sc :: NoteName → Chord → [NoteName]
(define (sc root ch)
  ((wrap ch) root))

; e.g. (tr* 4 (sc 'F# maj7)) -> '(Bb D F A)

;----------------
; Define the PLR group of functions on triads from Neo-Riemannian Theory
; (see Wikipedia entry at https://en.wikipedia.org/wiki/Neo-Riemannian_theory).
; - P is an involution (i.e. a permutation with maximum cycle length 2) between the major and
;   minor triad of the same root: P(Cmaj) = Cmin
; - R is an involution between relative major and minor triads: R(Cmaj) = Amin
; - L doesn't have a recognised musical mapping: L(Cmaj) = Emin

(define (P triad)
  (map (curry invert (+ (list-ref triad 0)
                        (list-ref triad 2)))
       triad))

(define (L triad)
  (map (curry invert (+ (list-ref triad 1)
                        (list-ref triad 2)))
       triad))

(define (R triad)
  (map (curry invert (+ (list-ref triad 0)
                        (list-ref triad 1)))
       triad))

(define N (compose P L R))
(define S (compose R P L))
(define H (compose L P L))

; Define wrapped versions
; e.g. (P* (sc 'C maj)) → '(C Eb G)
; P* :: [NoteName] → [NoteName]
; etc.
(define P* (wrap* P))
(define L* (wrap* L))
(define R* (wrap* R))
(define N* (wrap* N))
(define S* (wrap* S))
(define H* (wrap* H))

;----------------
(displayln "Loaded musical definitions.")

; The End