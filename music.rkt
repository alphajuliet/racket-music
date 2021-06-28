#lang racket
;; music.rkt
;; Define musical functions
;; AJ 2017-04-28

(require rsound
         threading
         define-with-spec
         rakeda)

(provide (all-defined-out))

;;-----------------------
;; Utility functions

(define/c (map+ f x)
  ;; Generic map that is polymorphic across lists and values
  ;; map+ :: ([a] → [b]) → [a] → [b]
  ;; map+ :: ([a] → [b]) → a → b
  (if (list? x)
      (r:map f x)
      (apply f (list x))))

(define sort-nums
  ;; sort-nums :: Sortable a ⇒ [a] → [a]
  (map+ (r:sort <)))

(define list-min
  ;; Minimum of a list
  ;; list-min :: Sortable a ⇒ [a] → a
  (map+ (r:argmin identity)))

(define mod12
  ;; Modulo 12
  (r:flip modulo 12))

;;-----------------------
(define/c (transpose n x)
  ;; Transpose a note or chord, modulo 12
  ;; e.g. (transpose 2 '(0 3 7)) => '(2 5 9)
  (map+ (compose mod12 (r:add n)) x))

(define/c (invert n x)
  ;; Invert modulo 12
  (map+ (compose mod12 (r:subtract n)) x))

(define (canonical ch)
  ;; Canonical structure of a list
  ;; e.g (canonical '(2 6 8)) -> '(0 4 6), i.e. this is a major chord
  ;; canonical :: Sortable a ⇒ [a] → [a]
  (sort-nums
   (map+ (transpose (- (list-min ch))) ch)))

;;-----------------------
;; Musical data structures and conversions

(define scale
  ;; scale :: [Integer]
  (range 0 12))

(define noteNames
  ;; Note names, using my favourite mix of sharps and flats
  ;; define NoteName = Symbol
  ;; noteNames :: [NoteNames]
  `(C C# D Eb E F F# G Ab A Bb B))

(define num->note
  ;; Polymorphic conversions between note numbers and note names
  ;; num->note :: [Integer] → [NoteName] || Integer → NoteName
  (map+ (compose (r:flip r:nth noteNames) mod12)))

;; note->num :: [NoteName] → [Integer] || NoteName → Integer
(define note->num
  (map+ (r:flip r:index-of noteNames)))

;;-----------------------
(define allNotes
  ;; Comprehensive list of note names
  '((C) (C# Db) (D) (D# Eb) (E) (F) (F# Gb) (G) (G# Ab) (A) (A# Bb) (B)))

(define num->note*
  ;; num->note* :: [Integer] → [[NoteName]] || Integer → [NoteName]
  (map+ (compose (r:flip r:nth allNotes) mod12)))

(define (note->num* n)
  ;; note->num* :: NoteName → Integer || [NoteName] → [Integer]
  (map+ (λ (e) (r:index-where (curry true?) (map (curry r:index-of e) allNotes))) n))

(define/c (collapse note lst)
  ;; Collapse a list of note lists based on the given note
  ;; C# -> D#, F#..., but Db -> Eb, Gb...
  ;; e.g. (collapse 'Eb '((C# Db) (F# Gb) (B))) => '(Db Gb B)
  (let* ([base-note (symbol->string note)])
    (if (and (= 2 (string-length base-note))
             (eq? #\b (string-ref base-note 1)))
        (map last lst)
        (map first lst))))
 
;;-----------------------
;; Define wrappers around functions that transform note numbers

(define/c (wrap f x)
  ((compose num->note f note->num) x))

(define sorted-num->note
  ;; Sorted version of num->note
  ;; sorted-num->note :: [Integer] → [NoteName]
  (compose num->note sort-nums))

(define/c (wrap-sorted f x)
  ((compose sorted-num->note f note->num) x))

(define (wrap2 f x y)
  (~>> y
       note->num
       (map+ (curry f x))
       num->note))

;;-----------------------
;; Define better wrappers

(define/c (wrap* f x)
  ;; Wrap note->num* and num->note*
  ;; e.g. (wrap* (transpose 2) '(C D E)) => '(D E F#)
  ;;      (wrap* maj2 'G) => '(G A B D)
  ((compose (collapse 'C)
            num->note*
            f
            note->num*) x))

(define/c (wrapl* f x)
  ;; Wrap a function that returns a list
  ;; e.g. (wrap* inversions '(C E G)) => '((G C E) (C E G) (E G C))
  ((compose (r:map (collapse 'C))
            (r:map num->note*)
            f
            note->num*) x))

;;-----------------------
;; Define a chord as a list of intervals from the base note. It returns a
;; function that takes a given base note number and returns the chord of note numbers.
;; define Chord = Integer → [Integer]

(define (chord ns)
  ;; chord :: [Integer] → Chord
  ;; e.g. (maj6 4) => '(4 8 1)
  (λ (n) (r:map (transpose n) ns)))

;; Define chords as functions operating on a note number
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

(define main-chords
  ;; main-chords :: [Chord]
  (list major minor x7 maj7 min7))

(define show-main-chords
  ;; Show all the main chords
  ;; e.g. (show-main-chords 'F#) => '((F# A# C#) (F# A C#) (F# A# C# E) (F# A# C# F) (F# A C# E))
  (wrapl* (r:juxt main-chords)))

(define (match-chord lst)
  ;; Match note numbers to one or more chord names
  ;; This will do a match across all the given chords
  ;; @@TODO
  (let ([x (map canonical (permutations lst))])
    #f))

(define (inversions ch)
  ;; Show all inversions of a chord
  (r:nest-list r:rotate-left (length ch) ch))

;;----------------
;; Example usage of the above functions

;; (wrap* (compose (transpose 2) min7) 'G)
;; (wrapl* (compose inversions min6 'E))
;; (show-main-chords 'D)

;;----------------
;; Define the PLR group of functions on triads from Neo-Riemannian Theory
;; (see Wikipedia entry at https://en.wikipedia.org/wiki/Neo-Riemannian_theory).
;; - P is an involution (i.e. a permutation with maximum cycle length 2) between the major and
;;   minor triad of the same root: P(Cmaj) = Cmin
;; - R is an involution between relative major and minor triads: R(Cmaj) = Amin
;; - L doesn't have a recognised musical mapping: L(Cmaj) = Emin

(define (P triad)
  (r:map (invert (r:add (r:nth 0 triad)
                        (r:nth 2 triad)))
         triad))

(define (L triad)
  (r:map (invert (r:add (r:nth 1 triad)
                        (r:nth 2 triad)))
         triad))

(define (R triad)
  (r:map (invert (r:add (r:nth 0 triad)
                        (r:nth 1 triad)))
         triad))

(define N (compose P L R))
(define S (compose R P L))
(define H (compose L P L))

;; Define wrapped versions
;; e.g. (P* (sc 'C maj)) → '(C Eb G)
;; P* :: [NoteName] → [NoteName]
;; etc.
(define P* (wrap* P))
(define L* (wrap* L))
(define R* (wrap* R))
(define N* (wrap* N))
(define S* (wrap* S))
(define H* (wrap* H))

;;----------------
(displayln "Loaded musical definitions.")

;; The End