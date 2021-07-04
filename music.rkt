#lang racket
;; music.rkt
;; Define musical functions
;; AJ 2017-04-28

(require threading
         define-with-spec
         rakeda
         "visual.rkt")

(provide (all-defined-out))

;;-----------------------
;; Utility functions

(define/c (map+ f x)
  ;; Generic map that is polymorphic across lists and values
  ;; map+ :: (a → b) → a → b
  ;; map+ :: (a → b) → [a] → [b]
  (if (list? x)
      (r:map f x)
      (apply f (list x))))

(define list-min
  ;; Minimum value of a list
  ;; list-min :: Sortable a ⇒ [a] → a
  (r:argmin identity))

(define mod12
  ;; Modulo 12
  (r:flip modulo 12))

;;-----------------------
(define/c (transpose n x)
  ;; Transpose a note or chord, modulo 12
  ;; e.g. (transpose 2 '(0 3 7)) => '(2 5 9)
  (map+ (compose1 mod12 (r:+ n)) x))

(define/c (invert n x)
  ;; Invert modulo 12
  (map+ (compose1 mod12 (r:- n)) x))

(define/spec (canonical ch)
  ;; Canonical structure of a list
  ;; e.g (canonical '(2 6 8)) -> '(0 4 6), i.e. this is a major chord
  ;; canonical :: Sortable a ⇒ [a] → [a]
  (-> (listof integer?) (listof integer?))
  (r:sort <
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
  '(C C# D Eb E F F# G Ab A Bb B))

(define allNotes
  ;; Comprehensive list of note names
  '((C) (C# Db) (D) (D# Eb) (E) (F) (F# Gb) (G) (G# Ab) (A) (A# Bb) (B)))

;;-----------------------
(define num->note
  ;; num->note :: [Integer] → [[NoteName]] || Integer → [NoteName]
  (map+ (compose1 (r:flip r:nth allNotes) mod12)))

(define (note->num n)
  ;; note->num :: NoteName → Integer || [NoteName] → [Integer]
  (map+ (λ (e) (r:index-where (curry true?)
                              (map (curry r:index-of e) allNotes)))
        n))

(define/c (collapse note lst)
  ;; Collapse a list of note lists based on the given note
  ;; C# -> D#, F#..., but Db -> Eb, Gb...
  ;; e.g. (collapse 'Eb '((C# Db) (F# Gb) (B))) => '(Db Gb B)
  (let* ([base-note (symbol->string note)])
    (if (and (= 2 (string-length base-note))
             (eq? #\b (string-ref base-note 1)))
        (map last lst)
        (map first lst))))

(define num->note*
  ;; Collapse the note options
  (compose (collapse 'C) num->note))
 
;;-----------------------
;; Define better wrappers

(define/c (wrap f x)
  ;; Wrap note->num and num->note
  ;; e.g. (wrap (transpose 2) '(C D E)) => '(D E F#)
  ;;      (wrap maj2 'G) => '(G A B D)
  (~>> x
       note->num
       f
       num->note*))

(define/c (wrapl f x)
  ;; Wrap a function that returns a list
  ;; e.g. (wrap inversions '(C E G)) => '((G C E) (C E G) (E G C))
  (~>> x
       note->num
       f
       (r:map num->note*)))

;;-----------------------
;; Define a chord as a list of intervals from the base note.

(define chords
  ;; Define chords as a hash for reverse lookups
  (hash 'major   '(0 4 7)
        'minor   '(0 3 7)
        'x4+5    '(0 5 7)
        'x7      '(0 4 7 10)
        'maj7    '(0 4 7 11)
        'min7    '(0 3 7 10)
        'maj9    '(0 4 7 14)
        'min9    '(0 3 7 14)
        'maj7+9  '(0 4 7 11 14)
        'min7+9  '(0 3 7 10 14)
        'maj6    '(0 4 9)
        'min6    '(0 3 9)
        'aug     '(0 4 8)
        'maj2    '(0 2 4 7)
        'min2    '(0 2 3 7)
        'dim     '(0 3 6)
        'maj4    '(0 4 5)
        'min4    '(0 3 5)
        'maj4+6  '(0 4 5 9)
        'min4+6  '(0 3 5 9)))

(define/c (c* note ch)
  ;; Define c* in terms of the hash
  (let ([n (note->num note)])
    (num->note* (r:map (transpose n) (hash-ref chords ch)))))

(define main-chords
  ;; main-chords :: [Chord]
  (list 'major 'minor 'x7 'maj7 'min7))

(define all-chords
  ;; all-chords :: [Chord]
  (hash-keys chords))

(define (apply-chord chs note)
  (map+ (compose num->note* (c* note)) chs))

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

;; (wrap (compose (transpose 2) min7) 'G)
;; (wrapl (compose inversions min6) 'E)
;; (show-main-chords 'D)

;;----------------
;; Define the PLR group of functions on triads from Neo-Riemannian Theory
;; (see Wikipedia entry at https://en.wikipedia.org/wiki/Neo-Riemannian_theory).
;; - P is an involution (i.e. a permutation with maximum cycle length 2) between the major and
;;   minor triad of the same root: P(Cmaj) = Cmin
;; - R is an involution between relative major and minor triads: R(Cmaj) = Amin
;; - L doesn't have a recognised musical mapping: L(Cmaj) = Emin

(define (involute n1 n2 triad)
  ;; Generic function to apply involutions
  (r:map (invert (r:+ (r:nth n1 triad)
                      (r:nth n2 triad)))
         triad))

(define (P triad) (involute 0 2 triad))
(define (L triad) (involute 1 2 triad))
(define (R triad) (involute 0 1 triad))
(define N (compose1 P L R))
(define S (compose1 R P L))
(define H (compose1 L P L))

;;----------------
(displayln ":: Loaded musical definitions.")

;; The End
