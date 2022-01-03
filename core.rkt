#lang racket
;; core.rkt
;; Define musical functions
;; AJ 2017-04-28

(provide (all-defined-out))

(require threading
         rakeda)

;;-----------------------
;; Utility functions

(define/curry (map++ f lst)
  ;; Generic map across a list or list of lists
  (if (and~> lst first list?)
      (map f lst)
      ;; else
      (f lst)))

(define list-min
  ;; Minimum value of a list
  ;; list-min :: Sortable a ⇒ [a] → a
  (curry apply min))

(define mod12
  ;; mod12 :: Integer -> Integer
  (r/flip modulo 12))

(define (hash-lookup h value)
  ;; Return the first key that has the given value, otherwise #f
  ;; hash-lookup :: (Hash k v) -> v -> k
  (let ([lst (filter (λ (k) (equal? value (hash-ref h k)))
                     (hash-keys h))])
    (if (empty? lst)
        #f
        (first lst))))

(define (select-keys h ks)
  ;; Return the hash entries that just contain the given keys
  ;; select-keys :: Hash k v -> [k] -> Hash k v
  (for/hash ([k ks])
    (values k (hash-ref h k))))

(define (hash-map f h)
  ;; Do mapping over all values in a hash and return a new hash.
  ;; hash-map :: (a -> b -> c) -> Hash a b -> Hash a c
  (for/fold ([h-out (make-immutable-hash)])
            ([(k v) (in-hash h)])
    (hash-set h-out k (f k v))))

;;-----------------------
(define/curry (transpose n x)
  ;; Transpose a note or chord
  ;; transpose :: Integer -> Integer -> Integer
  (map+ (r/+ n) x))

(define/curry (transpose* n x)
  ;; Transpose a note or chord, modulo 12
  ;; e.g. (transpose* 14 '(0 3 7)) => '(2 5 9)
  ;; transpose* :: Integer -> Integer -> Integer
  (map+ (compose1 mod12 (r/+ n)) x))

(define/curry (invert n x)
  ;; invert :: Integer -> Integer -> Integer
  (map+ (r/- n) x))

(define/curry (invert* n x)
  ;; Invert modulo 12
  ;; invert* :: Integer -> Integer -> Integer
  (map+ (compose1 mod12 (r/- n)) x))

(define (inversions notes)
  ;; Show all inversions of note numbers
  ;; inversions :: [Integer] -> [[Integer]]
  ;; e.g. inversions (0 4 7) => ((0 4 7) (4 7 0) (7 0 4))
  (if (list? notes)
      (r/iterate r/rotate-left (sub1 (length notes)) notes)
      ;;else
      notes))

;;-----------------------
;; Musical data structures and conversions

(define note-names
  ;; Note names, using my favourite mix of sharps and flats
  ;; define Note = Symbol
  ;; note-names :: [NoteName]
  '(C C# D Eb E F F# G Ab A Bb B))

(define all-notes
  ;; Comprehensive list of note names
  ;; all-notes :: [[NoteName]]
  '((C) (C# Db) (D) (D# Eb) (E) (F) (F# Gb) (G) (G# Ab) (A) (A# Bb) (B)))

(define (sharp-or-flat? note)
  ;; sharp-or-flat? :: NoteName -> Boolean
  (let ([str (symbol->string note)])
    (or (string-suffix? str "#")
        (string-suffix? str "b"))))

;;-----------------------
(define (note->num n)
  ;; note->num :: NoteName → Integer || [NoteName] → [Integer]
  (map+ (λ (e) (r/index-where (curry true?)
                              (map (curry r/index-of e) all-notes)))
        n))

(define/curry (collapse ref note)
  ;; Collapse notes based on the given reference note
  ;; C# -> D#, F#..., but Db -> Eb, Gb...
  ;; e.g. (collapse 'Eb '((C# Db) (F# Gb) (B))) => '(Db Gb B)
  ;; collapse :: NoteName -> [[NoteName]] -> [NoteName]
  (let ([ref-note (symbol->string ref)])
    (if (and ((string-length ref-note) . = . 2)
             (string-suffix? ref-note "b"))
        (map++ last note)
        ;; else
        (map++ first note))))

(define num->note
  ;; num->note :: [Integer] → [[NoteName]] || Integer → [NoteName]
  (map+ (compose1 (r/flip r/nth all-notes) mod12)))

(define num->note*
  ;; Collapsed version of num->note using sharps as default
  (compose (collapse 'C) num->note))
 
;;-----------------------
;; Map over note names
(define/curry (map-note f x)
  ;; Map a function on note numbers over note names
  ;; e.g. (map-note (transpose 2) '(C D E)) => '(D E F#)
  ;; e.g. (map-note inversions '(C E G)) => '((G C E) (C E G) (E G C))
  ;; map-note :: (Integer -> Integer) -> [NoteName] -> NoteName
  (~>> x
       (map+ note->num)
       f
       (map+ num->note*)))

#;(define/curry (map-note-list f x)
  ;; Lift map-note to work on lists
  ;; e.g. (map-note-list inversions '(C E G)) => '((G C E) (C E G) (E G C))
  ;; map-note-list :: (Integer -> [Integer]) -> [NoteName] -> [NoteName]
  (~>> x
       note->num
       f
       (r/map num->note*)))

;; The End
